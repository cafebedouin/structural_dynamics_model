% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Liturgical Transmission Sufficiency Standard for Hebrew Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the liturgical_preservation_reading of the
 *   living_language_status kernel: the claim that a language's ongoing
 *   recitation, study, and ritual use in a fixed sacred corpus is by itself
 *   sufficient evidence that the language is alive, independent of whether it
 *   is anyone's native vernacular. Historically, this standard was applied to
 *   Hebrew across the diaspora period, when the language survived almost
 *   exclusively in synagogue and study-house contexts. The reading has a
 *   genuine coordination function — it lets a geographically dispersed
 *   community maintain a stable, mutually intelligible liturgical practice
 *   across centuries — but it also structurally benefits the rabbinical
 *   authorities who administer the corpus and interpretive tradition, and it
 *   structurally disadvantages both secular vernacular speakers (whose daily
 *   usage is treated as subordinate to or derivative of sacred usage) and the
 *   Hebraist revival movement (whose deliberate vernacularization work is
 *   rendered redundant if the language was 'never dead' in the relevant
 *   sense). This is a distinct, ε-invariant constraint from the sibling
 *   readings native_generation_reading (which would classify liturgical-only
 *   Hebrew as a preserved-but-dead language, like Latin) and
 *   literary_continuity_reading (which locates vitality in productive new
 *   literary/intellectual output rather than in corpus fidelity). Each
 *   reading has its own beneficiary/victim structure and its own ε; they are
 *   linked here only through the shared kernel, not merged into one
 *   constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.34).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.42).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Transmission Sufficiency Standard for Hebrew Vitality").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6').
narrative_ontology:cs_kernel_codification('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', fixed_text).
narrative_ontology:cs_authority_grounding('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', lineage).
narrative_ontology:cs_interpretation_layer_present('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6').
narrative_ontology:cs_reading_relation('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', foundational, liturgical_recitation_constitutes_sufficient_vitality).
narrative_ontology:cs_axiom_status(liturgical_recitation_constitutes_sufficient_vitality, holdable).
narrative_ontology:cs_axiom_grounding('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', liturgical_recitation_constitutes_sufficient_vitality, conventional).
narrative_ontology:cs_axiom('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', secondary, vernacular_status_is_not_required_for_life).
narrative_ontology:cs_axiom_status(vernacular_status_is_not_required_for_life, holdable).
narrative_ontology:cs_axiom_grounding('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', vernacular_status_is_not_required_for_life, conventional).
narrative_ontology:cs_reference_frame('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', diaspora_liturgical_continuity_norm).
narrative_ontology:cs_drift_state('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', post_hebrew_vernacular_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa0cb655-dbcf-426e-b1cc-bbf66fcfd3d6', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, hebraist_revival_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_continuity_as_sufficient_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the standard by which the language's status is adjudicated within religious life: recitation, study, and ritual use of the fixed liturgical corpus are treated as sufficient proof the language lives. This standard is set and re-affirmed by the same body that holds interpretive authority over the texts being recited, and its own institutional continuity depends on the corpus never being treated as merely dead or merely revivable outside its authority.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Their entire pedagogical apparatus — textual study without communicative competence as a goal, recitation as mastery — is validated by the liturgical-sufficiency standard. If vitality required native generational transmission or literary productivity, the yeshiva model would no longer count as sustaining a living language, undermining the institutions' claim to be custodians of a living tradition rather than a preserved one.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, national).

% Speaks the language in daily secular life — in homes, schools, markets, government — but under this standard their usage is treated as incidental or even suspect relative to the liturgical benchmark; some ultra-orthodox currents historically framed everyday secular use of the sacred tongue as desecration. Their claim to be the living carriers of the language is structurally subordinated to the corpus-recitation criterion administered by an authority they do not answer to.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Worked deliberately to convert the language from a liturgical register into a spoken vernacular for a modern national community. Under the liturgical-sufficiency standard, this labor is rendered analytically unnecessary — the language was never dead, so revival is recast as innovation or even corruption of the sacred register, rather than as the vitality-restoring achievement the movement understands itself to have accomplished.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, hebraist_revival_movement, payer,
    moderate, generational, constrained, national).

% Communities with no native speakers of the language in daily life anywhere, who nonetheless recite, chant, and study it continuously in worship. The standard validates their relationship to the language as sufficient for calling it alive, which sustains communal identity and religious legitimacy — but it also locks them into a static register with no institutional path or incentive toward spoken revival, since the criterion they rely on does not require it.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities, payer).

% Study language vitality and death cross-linguistically and note that liturgical-only preservation (Latin, Sanskrit, Coptic, Ge'ez) is the standard scholarly category for a dead-but-preserved language, distinct from cases of vernacular reactivation. They observe the liturgical-sufficiency reading applied specifically to Hebrew diverges from the comparative pattern used everywhere else.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community's textual and ritual life around a single, stable, uninterrupted corpus — everyone reciting the same fixed prayers and readings across geography and centuries, without requiring vernacular fluency as a precondition for participation in worship.
% TRANSFER_FUNCTION: Moves interpretive and definitional authority over what counts as the language's 'life' from linguists, native speakers, and revivalist institutions to rabbinical authorities who administer the liturgical corpus — and moves legitimacy away from secular and vernacular usage toward corpus-bound recitation.
% ABSENT_VOICES: Secular Hebrew speakers who use the language as an unremarkable daily vernacular are not consulted on whether their usage counts as evidence of vitality; the standard is set by parties whose institutional position depends on the corpus-centric definition, not by sociolinguists or by the speech community itself.
% DISAPPEARANCE_RATIONALE: If the liturgical-sufficiency standard vanished, rabbinical institutions would lose a definitional lever used to frame secular vernacular Hebrew as derivative of or subordinate to sacred usage, and diaspora communities with no vernacular speakers would face harder questions about whether their relationship to the language constitutes 'living' status at all — but ordinary sociolinguistic classification of Hebrew as a revived vernacular would proceed unaffected, since it does not depend on the liturgical criterion. The parties disagree on how much rearranges.
% FOUNDING_PROBLEM: Historically, the problem was maintaining an unbroken chain of ritual and textual transmission for a language no longer spoken natively by most of a dispersed people, across two millennia of diaspora, so that liturgical practice remained mutually intelligible and continuous.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities and yeshiva institutions attest the founding problem (continuity of sacred transmission) remains fully live and central. Comparative linguists, an outside seat, corroborate that liturgical-only transmission genuinely solves a real coordination problem (textual continuity) but note this is analytically distinct from, and does not itself resolve, the separate historical question of vernacular death and revival — meaning the 'liturgical sufficiency proves vitality' inference is not corroborated outside the benefiting institutions.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.34, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.34 at interval end) because the primary function — coordinating liturgical practice across a dispersed community — is genuinely served by corpus stability, and the interpretive-monopoly extraction riding on it is real but modest relative to, e.g., a rent-seeking toll. Suppression (0.42) reflects the active social and institutional pressure historically exerted on framing secular/vernacular Hebrew usage as illegitimate or desecrating, which requires the rabbinical authority to keep asserting the corpus-sufficiency standard against a live, obviously-thriving vernacular counter-fact — this is not automatic; it requires ongoing doctrinal work. Theater ratio is modest and rising (0.15 to 0.28) as the vernacular language visibly flourished over the 20th century, making pure 'liturgical proof of life' claims increasingly performative relative to the much stronger vernacular evidence available.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and yeshiva institutions sit near the beneficiary end: the standard validates and entrenches an interpretive practice they administer and in which their institutional legitimacy is invested. Secular speech communities and the Hebraist revival movement sit near the target end: their achievement (a living vernacular) is definitionally sidelined by a criterion that would have called the language alive regardless of their labor, and their register of use is treated as subordinate. Diaspora liturgical communities are a genuine dual case — real beneficiaries of a standard that legitimizes their relationship to the language, but also payers insofar as the standard gives them no institutional push toward vernacular revival, locking them into a static register.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (stable, continuous liturgical transmission across a dispersed community) is real and should not be waved away as pure cover — Tangled Rope, not Snare, is the correct classification precisely because both a genuine coordination function and an asymmetric extraction (interpretive monopoly benefit to rabbinical authority, delegitimization cost to secular/vernacular speakers) are present simultaneously. Treating this as pure extraction would erase the real value dispersed communities get from a stable, shared liturgical register; treating it as pure coordination would erase the real interpretive-authority stakes in who gets to define 'living.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_vs_evidence_ambiguity,
    'Is liturgical transmission being claimed as SUFFICIENT for living status (this reading''s premise) or merely as ONE PIECE OF EVIDENCE among several indicators of vitality — and does the rabbinical authority''s institutional interest bias which framing is asserted in a given historical moment?',
    'Track whether rabbinical or communal authorities invoke the liturgical criterion ONLY in contexts where vernacular evidence is absent or contested (evidentiary use) versus invoking it even where robust vernacular counter-evidence exists (sufficiency-as-trump use). The latter pattern would indicate the criterion is doing legitimacy work beyond its evidentiary function.',
    'If liturgical transmission is invoked mainly as a trump card against vernacular evidence rather than as a genuine independent indicator, the extraction component is higher than currently authored and the constraint moves closer to snare; if invoked only in the absence of vernacular evidence, the coordination reading is closer to the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_vs_evidence_ambiguity, conceptual, 'Whether liturgical sufficiency functions as genuine evidence or as an authority-preserving trump card.').

omega_variable(
    kernel_framing_under_determination,
    'Is ''living language'' most defensibly a kernel with three genuinely incommensurable readings (liturgical, native-generation, literary-continuity), or is one reading (native-generation) actually the dominant scholarly consensus with the other two functioning as special-interest carve-outs rather than co-equal readings?',
    'Survey comparative sociolinguistics literature on language death/revival criteria to establish whether native-generational transmission is treated as the default operational definition, with liturgical and literary criteria treated as marked exceptions requiring justification.',
    'If native-generation is the disciplinary default, this reading and literary_continuity_reading both carry a heavier justificatory burden than a flat coexists_with framing implies — their beneficiaries would need to argue against a background presumption, not merely present an alternative frame. This does not change this story''s ε, but affects how the kernel family should be read as a whole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three kernel readings are truly co-equal or whether one is the disciplinary default and the others are contested departures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(livi_tr_t60, living_language_status__liturgical_preservation_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(livi_tr_t80, living_language_status__liturgical_preservation_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(livi_tr_t100, living_language_status__liturgical_preservation_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(livi_be_t60, living_language_status__liturgical_preservation_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(livi_be_t80, living_language_status__liturgical_preservation_reading, base_extractiveness, 80, 0.32).
narrative_ontology:measurement(livi_be_t100, living_language_status__liturgical_preservation_reading, base_extractiveness, 100, 0.34).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(living_language_status__liturgical_preservation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposed from the colloquial label 'is Hebrew a living language' per the ε-invariance principle. liturgical_preservation_reading (this file) authors low-moderate ε reflecting a real but modest interpretive-monopoly extraction riding on genuine liturgical coordination. native_generation_reading is expected to author a structurally distinct claim (vernacular transmission as the sole criterion) with different beneficiaries (Hebraist revival institutions, modern Israeli linguistic nationalism) and different victims (diaspora liturgical communities recast as preserving a 'dead' language). literary_continuity_reading locates vitality in productive literary output, with its own beneficiary set (Haskalah literary institutions, modern Hebrew publishing) largely orthogonal to the other two. The three are linked, not merged, because forcing one ε to cover all three readings would violate ε-invariance: the sufficiency criterion each reading proposes produces a materially different extraction profile and a different victim class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
