% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the liturgical-continuity reading of the 'Hebrew
 *   as living language' kernel: the claim that Hebrew persisted as a living
 *   linguistic and cultural system through nearly two millennia of diaspora
 *   via unbroken synagogue recitation, daily prayer, Torah/Talmud study, and
 *   responsa literature, without requiring native generative daily speech.
 *   Under this reading, 'living' is defined by continuous, voluntary,
 *   generationally-transmitted textual and liturgical practice rather than by
 *   vernacular generativity. This is structurally a low-extraction
 *   coordination arrangement: communities voluntarily maintain the practice
 *   because it constitutes their religious and cultural identity, not because
 *   any party coerces participation or extracts rents from it. There is no
 *   victim set — no one is structurally harmed by the continuation of
 *   liturgical Hebrew, and exit (assimilation, secularization, language
 *   shift) has always been available and exercised by many without penalty
 *   beyond communal/religious consequence, which this reading treats as
 *   internal to voluntary religious life rather than extractive suppression.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary and practitioner (organized/institutional, mobile-to-constrained exit) — sustains the practice generationally
 *   - rabbinic_textual_tradition: Agenda-setter and custodian (institutional, arbitrage exit via textual authority) — administers what counts as correct recitation and study
 *   - liturgical_reading_communities: Practicing beneficiaries (organized, constrained exit tied to communal belonging) — carry the practice forward locally
 *   - comparative_historical_linguists: Analytical observer — assesses whether recitation-based continuity constitutes 'living language' status
 *   - hebrew_revivalists_19th_20th_c: Excluded/contesting voice — argue this reading understates what 'living' requires, feeding the sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '2c82e149-e30d-46e0-925b-0c012da941de').
narrative_ontology:cs_kernel_codification('2c82e149-e30d-46e0-925b-0c012da941de', fixed_text).
narrative_ontology:cs_authority_grounding('2c82e149-e30d-46e0-925b-0c012da941de', lineage).
narrative_ontology:cs_interpretation_layer_present('2c82e149-e30d-46e0-925b-0c012da941de').
narrative_ontology:cs_reading_relation('2c82e149-e30d-46e0-925b-0c012da941de', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c82e149-e30d-46e0-925b-0c012da941de', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('2c82e149-e30d-46e0-925b-0c012da941de', foundational, continuous_ritual_transmission_constitutes_life).
narrative_ontology:cs_axiom_status(continuous_ritual_transmission_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('2c82e149-e30d-46e0-925b-0c012da941de', continuous_ritual_transmission_constitutes_life, conventional).
narrative_ontology:cs_axiom('2c82e149-e30d-46e0-925b-0c012da941de', secondary, generative_vernacular_speech_not_required_for_living_status).
narrative_ontology:cs_axiom_status(generative_vernacular_speech_not_required_for_living_status, holdable).
narrative_ontology:cs_axiom_grounding('2c82e149-e30d-46e0-925b-0c012da941de', generative_vernacular_speech_not_required_for_living_status, conventional).
narrative_ontology:cs_reference_frame('2c82e149-e30d-46e0-925b-0c012da941de', post_sovereignty_diaspora_liturgical_transmission).
narrative_ontology:cs_drift_state('2c82e149-e30d-46e0-925b-0c012da941de', post_israeli_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2c82e149-e30d-46e0-925b-0c012da941de', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinic_textual_tradition).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_reading_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, liturgical_reading_communities).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_continuity_via_recitation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain synagogue liturgy, home ritual, and communal Torah/Talmud study in Hebrew across generations and across widely dispersed geographic locations, without political sovereignty or territorial concentration. Exit toward secular or assimilated identity has always been available and has been exercised by many; those who remain do so through continued voluntary religious/cultural commitment rather than coercion.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, civilizational, constrained, global).

% Administers what counts as correct liturgical recitation, canonical text, and permissible interpretive practice through responsa literature, commentary, and communal religious authority. Holds interpretive authority across diaspora communities and adapts guidance to local circumstance while maintaining textual continuity; does not extract material rents from participants for maintaining the practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_textual_tradition, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Local congregations that carry the recitation and study practice forward through weekly and daily observance. They bear the modest time and effort cost of learning and maintaining Hebrew literacy for liturgical purposes, and receive in return communal belonging and continuity of identity; their exit option is real but carries communal/relational cost, not structural penalty.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_reading_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, liturgical_reading_communities, payer).

% Study whether recitation-and-study-based continuity satisfies the criteria linguists typically use for 'living language' status, and compare this diaspora pattern to other cases of liturgical language maintenance (e.g., Sanskrit, Church Latin, Coptic) to assess whether Hebrew's case is structurally distinct.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, comparative_historical_linguists, observer,
    analytical, generational, analytical, global).

% Historical figures and movements (Haskalah writers, Ben-Yehuda and the Hebrew revival movement) who argued that liturgical recitation alone was insufficient for a genuinely living language and pushed instead for generative literary production or native vernacular speech. Their critique is not represented as a voice within this constraint's own framework — it is precisely the position this reading's siblings instantiate.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_revivalists_19th_20th_c, excluded,
    organized, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains shared access to sacred text, communal prayer, and cross-generational religious/cultural identity across geographically dispersed communities lacking political or territorial unity, by maintaining a common liturgical and textual language understood (at some level) across all diaspora locations.
% TRANSFER_FUNCTION: No extractive transfer of material resources between parties; the arrangement moves time, attention, and pedagogical effort from each generation to the next in the form of Hebrew literacy sufficient for liturgical participation and textual study, in exchange for communal belonging and continuity of identity.
% ABSENT_VOICES: Hebrew revivalists (Haskalah writers, Ben-Yehuda-tradition figures) who held that recitation without generative competence does not constitute a living language are not represented as objecting voices within this constraint — their position is structurally external to this reading and materializes instead as the sibling constraints (native_generation_reading, literary_revival_reading).
% DISAPPEARANCE_RATIONALE: If unbroken liturgical recitation and textual study vanished overnight across diaspora, communities would lose a primary infrastructure of continuity between religious practice, communal identity, and historical memory; prayer, life-cycle ritual, and textual study would have to be reconstituted in vernacular languages or abandoned, materially reorganizing diaspora Jewish communal life and severing a documented multi-century transmission chain.
% FOUNDING_PROBLEM: Following the loss of political sovereignty and territorial concentration (notably after 70 CE and subsequent expulsions), diaspora communities needed a means of maintaining shared religious practice, textual access to sacred literature, and cross-community intelligibility absent a common territory or state.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Jewish diaspora and comparative sociolinguists (studying analogous liturgical-language cases such as Sanskrit and Church Latin) attest that the founding problem — maintaining textual/communal continuity without territorial sovereignty — was real and that liturgical Hebrew addressed it for centuries; some of these same outside observers, along with the Hebrew revival movement's own historical arguments, contest whether the problem is now fully 'live' given the existence of a sovereign Hebrew-speaking state and modern vernacular alternatives, making status assessment genuinely disputed rather than settled by either side.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08 at interval end) because the practice is symbol-preservation: no party is coerced into participation, no rents are extracted through the recitation and study system, and communities that exit (via secularization or assimilation) bear no structural penalty beyond loss of communal religious identity, which is not extraction in the framework's sense. Suppression is authored very low (0.05) because there is no active enforcement apparatus compelling recitation — participation is maintained by religious/cultural commitment, not coercion. Theater ratio is low-moderate (0.12) because liturgical recitation is functionally continuous with study and communal identity maintenance, not merely performative; a small theatrical component exists insofar as some recitation is rote rather than comprehended, but this does not dominate the practice. Accessibility collapse is moderate (0.2): alternatives (secular Jewish identity, other liturgical languages, full assimilation) have always existed and were exercised throughout diaspora history, so alternatives never fully collapsed. Resistance is low (0.1) because there is little active opposition to the practice's continuation — decline happens through attrition and assimilation, not confrontation.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/communal agenda-setter seat, the practice is coordination infrastructure they administer and benefit from without collecting extractive rents. From the analytical/comparative-linguistics observer seat, the same practice may be read as a narrower register-preservation phenomenon than a fully 'living' language — this is not extraction, but a definitional divergence the omega variables route to the kernel-level contest rather than to this constraint's own classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (diaspora communities, rabbinic tradition, liturgical communities) sit near the full-beneficiary end of directionality: the practice subsidizes their religious/cultural continuity and identity coherence, and no party pays a structural cost through the same mechanism. There is deliberately no victim group declared — this is the central structural claim of this reading (per the expected delta): participation is voluntary, exit has always been exercised without punitive structural consequence, and the arrangement's persistence depends on continued voluntary commitment rather than coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining textual/liturgical access to sacred and communal identity absent territorial and political sovereignty — remains live for diaspora communities that continue the practice, though many communities have shifted toward the sibling readings' criteria (native_generation_reading materializing in Israeli Hebrew revival, literary_revival_reading materializing in Haskalah production) or exited into other forms of Jewish identity. This reading does not claim universal persistence; it claims that WHERE the practice continues, it constitutes genuine living-language status by its own criterion, and that criterion has not been mooted by the other readings' emergence. No mandatrophy: the arrangement's function (maintaining the practice as identity infrastructure) is still actively served by those who maintain it, and no one benefits from calling it settled when it is not — the classification structurally supports coordination framing rather than needing corrective downgrade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recitation_vs_generative_competence,
    'Does unbroken liturgical recitation and textual study across diaspora constitute a ''living language'' in the same structural sense as native generative speech, or is it preservation of a symbolic/liturgical register distinct from a living vernacular?',
    'Comparative linguistic analysis of whether diaspora Hebrew users could produce novel utterances beyond memorized/formulaic liturgical and exegetical text, versus documented evidence of generative textual production (responsa literature, halakhic argumentation, poetry) across the diaspora centuries.',
    'If recitation-based continuity is structurally equivalent to living-language status, this reading and the native_generation_reading describe the same underlying phenomenon at different intensities; if not, they are genuinely distinct claims about what ''living'' means, and the kernel''s ambiguity is irreducible rather than resolvable by more data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_vs_generative_competence, conceptual, 'Whether recitation-based continuity satisfies the same criterion as generative native speech for ''living language'' status.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''Hebrew as a living language'' better modeled as three independent constraints (this decomposition) or as one constraint with an internally contested definition of ''living''?',
    'Track whether historical linguists and communities that hold this reading ever treat the sibling readings as falsifying rather than merely supplementing their own claim; convergent treatment would suggest one constraint with contested definition, divergent treatment supports the three-way decomposition already adopted here.',
    'If the three readings should be one constraint, the current decomposition artificially separates ε values that are actually a single contested quantity; if genuinely separable (as authored), each reading''s ε stands independently and the kernel linkage via network.affects_constraints is the correct structural representation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether decomposition into three sibling constraints versus one contested-definition constraint is the right framing.').

omega_variable(
    diaspora_uniformity_assumption,
    'Was liturgical Hebrew continuity genuinely unbroken and uniform across all diaspora communities (Ashkenazi, Sephardi, Yemenite, Ethiopian, Mizrahi), or did some communities experience partial breaks later reconstructed as continuous?',
    'Manuscript and prayer-book tradition comparison across diaspora communities; documented gaps in transmission chains (e.g., communities isolated for centuries) versus continuous documentary record.',
    'If continuity was uneven, the ''unbroken'' premise this reading depends on is itself a retrospective unification of a more fragmented history, which would weaken (though not eliminate) the reading''s claim to describe a single coherent constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_uniformity_assumption, empirical, 'Whether the unbroken-continuity premise holds uniformly across diaspora communities or is a retrospective smoothing of a more fragmented record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t300, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t300, observed).
narrative_ontology:measurement(hebr_tr_t600, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 600, 0.11).
narrative_ontology:measurement_basis(hebr_tr_t600, observed).
narrative_ontology:measurement(hebr_tr_t900, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 900, 0.11).
narrative_ontology:measurement_basis(hebr_tr_t900, observed).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1200, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1200, observed).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1500, observed).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t300, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement_basis(hebr_be_t300, observed).
narrative_ontology:measurement(hebr_be_t600, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 600, 0.07).
narrative_ontology:measurement_basis(hebr_be_t600, observed).
narrative_ontology:measurement(hebr_be_t900, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 900, 0.07).
narrative_ontology:measurement_basis(hebr_be_t900, observed).
narrative_ontology:measurement(hebr_be_t1200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1200, 0.08).
narrative_ontology:measurement_basis(hebr_be_t1200, observed).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(hebr_be_t1500, observed).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement_basis(hebr_be_t1800, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.06).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the hebrew_living_language kernel. The liturgical_continuity_reading (this story) authors low extractiveness and no victim set, treating continuous liturgical/textual practice as sufficient for living-language status. The native_generation_reading authors the criterion of generative native vernacular speech (materialized in the Israeli Hebrew revival) as the threshold, which this reading's persistence structurally influenced by keeping the phonological/lexical/textual substrate available for later generative reactivation. The literary_revival_reading occupies an intermediate position: written generative production without native daily speech. All three share the same underlying kernel object (Hebrew's status as a 'living language') but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different continuity claims — per the ε-invariance principle, they are not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
