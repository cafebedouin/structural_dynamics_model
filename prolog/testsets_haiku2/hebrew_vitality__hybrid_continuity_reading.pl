% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Framework
 *   domain: sociolinguistics/language-revitalization
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel 'hebrew_vitality'.
 *   The hybrid_continuity_reading proposes that Hebrew language vitality is
 *   neither constituted by liturgical preservation alone (the
 *   liturgical_reading position) nor by native generation alone (the
 *   native_daily_reading position), but rather by a two-phase process:
 *   liturgical use preserved the linguistic substrate across diaspora;
 *   vernacular revival required reconstruction of native daily use from that
 *   substrate. This reading does not resolve the contest by adjudicating
 *   winner and loser—rather, it reframes the question from 'which is
 *   vitality' to 'what conditions enable vitality', thereby accommodating
 *   both sibling readings as partial truths in a larger mechanism. Low
 *   extractiveness reflects that this is primarily an analytical/interpretive
 *   framework, not an actionable extractive constraint on speakers. High
 *   resistance reflects genuine scholarly and community dispute over whether
 *   the hybrid framing adequately captures the competing claims.
 *
 * KEY AGENTS:
 *   - language_revitalization_scholars: organizational agents holding interpretive authority over vitality doctrine; benefit from reframing dispute as complementary rather than zero-sum.
 *   - hebrew_speech_communities: organized constituencies whose lived practice (liturgical and vernacular simultaneously) benefits from legitimization of hybridity.
 *   - liturgical_preservation_advocates: historically powerful agents (religious institutions, traditional scholars) whose foundational work is relocated from sufficient to necessary-but-insufficient.
 *   - native_generation_advocates: organized Israeli and diaspora activists whose position (native generation constitutes vitality) is contextualized as incomplete without substrate.
 *   - diaspora_jewish_communities: moderate-power constituencies whose liturgical practice is legitimized as substrate contribution rather than secondary engagement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Framework").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language-revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '660de7ce-177d-4599-9bd1-373c3deb91c1').
narrative_ontology:cs_kernel_codification('660de7ce-177d-4599-9bd1-373c3deb91c1', distributed).
narrative_ontology:cs_authority_grounding('660de7ce-177d-4599-9bd1-373c3deb91c1', distributed).
narrative_ontology:cs_reading_relation('660de7ce-177d-4599-9bd1-373c3deb91c1', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('660de7ce-177d-4599-9bd1-373c3deb91c1', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('660de7ce-177d-4599-9bd1-373c3deb91c1', foundational, substrate_and_reconstruction_both_necessary).
narrative_ontology:cs_axiom_status(substrate_and_reconstruction_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('660de7ce-177d-4599-9bd1-373c3deb91c1', substrate_and_reconstruction_both_necessary, instrumental).
narrative_ontology:cs_axiom('660de7ce-177d-4599-9bd1-373c3deb91c1', foundational, vitality_requires_conditions_not_single_mechanism).
narrative_ontology:cs_axiom_status(vitality_requires_conditions_not_single_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('660de7ce-177d-4599-9bd1-373c3deb91c1', vitality_requires_conditions_not_single_mechanism, conventional).
narrative_ontology:cs_reference_frame('660de7ce-177d-4599-9bd1-373c3deb91c1', binary_vitality_contest_unresolved).
narrative_ontology:cs_drift_state('660de7ce-177d-4599-9bd1-373c3deb91c1', contemporary_linguistic_scholarship, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('660de7ce-177d-4599-9bd1-373c3deb91c1', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_speech_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_advocates).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_advocates).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, linguistic_substrate_persistence).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, reconstructionist_methodology_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame the research question around substrate and reconstruction, publish analyses that validate both liturgical preservation and native generation as necessary components. They hold the interpretive authority over what counts as 'vitality' in academic discourse and can shift the terms of the debate by introducing this hybrid framing.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, agenda_setter).

% Participate in both liturgical and vernacular Hebrew use; benefit from a framework that legitimizes both as essential to vitality rather than treating them as competing claims. The hybrid reading accommodates lived multilingual practice (liturgical Hebrew in prayer, vernacular in daily life) as coherent rather than as deficit.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_speech_communities, beneficiary,
    organized, generational, constrained, global).

% Historically held that unbroken liturgical use was sufficient to constitute vitality; the hybrid framework relocates their work from sufficient to necessary-but-insufficient, requiring acknowledgment of a reconstruction phase. They bear the conceptual cost of their contribution being reframed as enabling rather than constitutive.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_advocates, payer,
    powerful, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_advocates, beneficiary).

% Historically argued that only native generation constitutes true vitality; the hybrid framework acknowledges their claim (vitality requires native speakers) while also requiring acknowledgment that native generation drew on liturgical substrate. They bear the cost of their position being contextualized as partial rather than complete.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, native_generation_advocates, beneficiary).

% Maintain liturgical Hebrew use without native daily generation; the hybrid framework legitimizes their practice as part of the substrate that enables vitality elsewhere, rather than as failed or secondary engagement with the language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, diaspora_jewish_communities, beneficiary,
    moderate, biographical, constrained, global).

% Study language revitalization across multiple languages and indigenous contexts; the hybrid framework generates comparative insights (Irish, Welsh, Māori) about the role of ritual/liturgical substrate in enabling vernacular reconstruction.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, non_hebrew_linguists, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles competing framings of language vitality (liturgical vs. native-generational) by proposing that both preserve essential substrate and reconstruct missing capacity. Coordinates scholarly conversation across historically polarized positions by reframing the question from 'which is vitality' to 'what conditions enable vitality'.
% TRANSFER_FUNCTION: Moves interpretive authority from liturgical preservation alone (or native generation alone) to a hybrid model that legitimizes both contributors. Transfers credit for language persistence from a single mechanism to a two-phase process, requiring recognition of multiple actors' roles.
% ABSENT_VOICES: Language speakers who engage with Hebrew outside both liturgical and native-daily registers (heritage learners, secular cultural practitioners, revival activists in minority diaspora contexts) are not explicitly seated in the framework and could contest whether the substrate/reconstruction dyad captures their actual linguistic practice.
% DISAPPEARANCE_RATIONALE: If this hybrid framing disappeared, scholarly debate would revert to competing monolithic claims (liturgical sufficiency vs. native-only authenticity). Some language policy would shift: revival programs emphasizing only native fluency would lose the legitimizing framing that includes liturgical substrate as foundational. Conversely, communities maintaining liturgical practice without native generation might feel their contribution was erased. The constraint's disappearance would not reorganize the actual linguistic practice, but would reshape how scholars and policy-makers interpret and fund it.
% FOUNDING_PROBLEM: Hebrew language vitality doctrine was divided: liturgical preservationists held that unbroken religious use across diaspora constituted vitality; native-generation advocates held that only modern native speakers in Israel constituted true revival. The division blocked understanding of how Hebrew actually persisted and regenerated—liturgical practice was dismissed as 'mere preservation' by revival advocates, while native generation was treated as replacement rather than continuation by traditionalists.
% FOUNDING_PROBLEM_CORROBORATION: Linguists studying Hebrew revitalization (Sappan, Rabin, Hary) attest the founding problem from outside the benefiting parties: the scholarly divide generated incoherent policy and prevented recognition of the substrate mechanism. Israeli language planners and diaspora community leaders attest that the binary framing (preservation vs. revival) failed to describe their actual linguistic life, where liturgical and vernacular use coexist and interpenetrate.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint operates primarily at the interpretive level—it does not transfer material resources or enforce behavior on speakers, but rather reorganizes how scholars and communities understand what has already occurred. Suppression is minimal (0.08) because the constraint persists through reframing rather than coercion; no party must be prevented from speaking or believing in order for the hybrid framework to operate. Theater ratio is low-moderate (0.22) because the actual scholarly work of reconstruction and substrate preservation is real and functionally necessary, though some portion of the constraint's persistence consists of performative consensus-building in academic conferences and policy documents. Accessibility_collapse is moderate (0.35) because the hybrid framework, once understood, opens rather than forecloses alternatives—speakers can adopt it, reject it, or hold partial commitment to components. Resistance is high (0.72) because the competing readings (liturgical_reading, native_daily_reading) are held with strong identity commitment by their constituencies, and the hybrid framing is experienced as threatening to the coherence of each monolithic position. Measurement series show slight rising extractiveness as the hybrid framing becomes institutionalized in curricula and policy (moving from 0.08 to 0.15 over the interval), but it plateaus once adoption stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the organized scholars' seat, the hybrid framework is an analytical achievement that resolves an unsustainable binary. From the liturgical_preservation_advocates' seat (particularly religious traditionalists), the reframing threatens to relativize their historical claim—what was constitutive becomes merely enabling. From the native_generation_advocates' seat (particularly Israeli revivalists), the framing threatens to dilute the specificity of their achievement by distributing credit backward to pre-Zionist preservation. The engine computes these divergences from the structural data: scholars benefit and set the agenda (beneficiary + agenda_setter roles), while both competing-position groups experience the reframing as a partial loss (payer + beneficiary roles, identity_locked exit). The two payer groups have different exit options and power levels, which should produce different effective extraction, but both experience the same formal reframing pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholars hold the lowest d (strongest beneficiary end): they gain interpretive authority and resolving power by introducing the hybrid framework; their exit is mobile (can change fields or positions). Liturgical advocates and native advocates hold higher d (experience net cost despite some benefit): they are identity-locked to their historical positions and experience the reframing as requiring acknowledgment of incompleteness. Diaspora communities hold moderate d (partial beneficiaries): they gain legitimacy for their practice without having to produce native speakers, but the framing still situates them as substrate-providers rather than vitality-generators. Hebrew speakers themselves are not seated as a single stakeholder because the constraint does not directly enforce behavior on them—they exist before and outside the constraint; the constraint is interpretive/institutional, operating on scholarly and policy understanding rather than on speech itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandate obsolescence. The founding problem (binary framing of vitality) remains live because it is rooted in genuine competing values (tradition vs. nativity, preservation vs. innovation) that cannot be finally resolved. The hybrid reading does not eliminate the contest; it reframes it as productive complementarity rather than zero-sum competition. Some actors may feel their mandate has been diluted (liturgical preservationists experiencing their work as demoted from constitutive to enabling), but the framework itself remains coherent and actively defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_completeness_ambiguity,
    'Was liturgical Hebrew preservation sufficient to maintain the full grammatical, phonological, and semantic substrate necessary for native regeneration, or did reconstruction require significant innovation and borrowing beyond what liturgical use could provide?',
    'Comparative historical-linguistic analysis of early Modern Hebrew texts (1880s–1920s) against liturgical sources: if innovations and borrowings are extensive and non-recoverable from liturgy, substrate was incomplete; if recoverable, substrate was sufficient.',
    'If substrate was incomplete, the hybrid reading must relocate the constitutive moment to the reconstruction phase itself (closer to native_daily_reading position). If sufficient, the hybrid reading holds: substrate enabled, reconstruction was necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substrate_completeness_ambiguity, empirical, 'Whether liturgical preservation left sufficient grammatical substrate for native regeneration.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the hybrid reading logically foreclose either the liturgical_reading or the native_daily_reading, or does it coexist with both as a meta-level reframing that different parties can partially adopt?',
    'Test whether a speaker or scholar can hold the hybrid reading AND maintain the sibling reading as their primary normative claim. If yes, they coexist; if the hybrid reading forces explicit rejection of the sibling''s core claim, they foreclose.',
    'If coexistence: the constraint is integrative and should classify as rope (coordination without extraction of competing positions). If foreclosure: the constraint is more aggressive and should classify toward snare (one reading dominating by redefining terms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the hybrid reading is genuinely coexistent with siblings or forecloses them.').

omega_variable(
    identity_lock_mechanism_in_advocates,
    'Is the identity_locked exit status for liturgical_preservation_advocates and native_generation_advocates driven by professional identity (career investment in the position), ideological identity (worldview commitment), or relational identity (role in community that depends on the position)?',
    'Interview/ethnographic data: do advocates who leave the position report identity crisis, career disruption, or community sanction? Which dominates?',
    'If professional: exit costs are calibrated to career alternatives; if ideological: exit requires worldview revision; if relational: exit requires community renegotiation. Different mechanisms produce different directionality profiles for the same power atom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_advocates, empirical, 'The mechanism binding liturgical and native advocates to their positions despite reframing pressure.').

omega_variable(
    substrate_reading_as_normative_cover,
    'Does the hybrid reading''s proposition that ''lithurgical substrate enabled reconstruction'' describe an actual historical causal mechanism, or does it serve as a normative reframing that accommodates competing positions without validating either?',
    'Historical linguistics: does the data support that Hebrew speakers drew on liturgical knowledge when constructing modern vernacular? Or were native regenerators drawing on contemporary European language structure and borrowing?',
    'If mechanisms is real: the reading is descriptively grounded and should persist in scholarly consensus. If it is normative reframing (accommodating without resolving): the reading is more vulnerable to challenge as new evidence emerges about actual regeneration processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_reading_as_normative_cover, empirical, 'Whether substrate/reconstruction is a historically accurate mechanism or a normative reconciliation device.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t8, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t16, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t16, observed).
narrative_ontology:measurement(hebr_tr_t24, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t24, observed).
narrative_ontology:measurement(hebr_tr_t32, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t32, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t8, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t16, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement_basis(hebr_be_t16, observed).
narrative_ontology:measurement(hebr_be_t24, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(hebr_be_t24, observed).
narrative_ontology:measurement(hebr_be_t32, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(hebr_be_t32, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(hebr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t8, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 8, 0.07).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t16, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement_basis(hebr_su_t16, observed).
narrative_ontology:measurement(hebr_su_t24, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 24, 0.08).
narrative_ontology:measurement_basis(hebr_su_t24, observed).
narrative_ontology:measurement(hebr_su_t32, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 32, 0.08).
narrative_ontology:measurement_basis(hebr_su_t32, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(hebr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__hybrid_continuity_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel manifests three structurally distinct constraints: liturgical_reading (extraction of the liturgical preservation position as sufficient), native_daily_reading (extraction of the native-generation position as sufficient), and hybrid_continuity_reading (analytical reframing attempting to accommodate both). Each has different ε (liturgical and native readings are higher-extraction claims with clear beneficiaries; hybrid is low-extraction analytical framework). All three are linked as siblings in the same kernel via reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__hybrid_continuity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
