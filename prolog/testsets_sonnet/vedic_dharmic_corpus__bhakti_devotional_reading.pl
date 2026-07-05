% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading: Devotion Over Birth as Spiritual Authority
 *   domain: religious authority / social stratification / interpretive legitimacy
 *
 * SUMMARY:
 *   This story instantiates the bhakti devotional reading of the
 *   Vedic/dharmic kernel: the claim that sincere devotion, not birth, is the
 *   operative criterion for spiritual authority and divine access.
 *   Historically this reading is associated with poet-saint movements
 *   (Alvars, Nayanars, Kabir, Mirabai, Chaitanya) that explicitly admitted
 *   practitioners across caste lines and, in some traditions, elevated
 *   non-Brahmin and women devotees to positions of scriptural and
 *   institutional authority. The reading functions as a genuine coordination
 *   mechanism for religious participation by populations otherwise excluded
 *   by ritual-literacy and hereditary gatekeeping, but it does not fully
 *   dissolve caste stratification in the surrounding social economy —
 *   devotional equality in worship coexists, historically and presently, with
 *   caste-structured social life outside the temple or congregation. This is
 *   a distinct constraint from the hereditary_monopoly_reading (which grounds
 *   authority in birth-lineage and treats the caste hierarchy as textually
 *   mandated) and from the reformist_egalitarian_reading (which subordinates
 *   textual authority to external constitutional/rational equality
 *   principles). The three readings are not the same constraint measured
 *   differently — they have different beneficiary/victim structures,
 *   different extraction profiles, and different institutional mechanisms,
 *   and are linked here only via network edges, per the ε-invariance
 *   decomposition rule.
 *
 * KEY AGENTS:
 *   - lower_caste_devotees: primary beneficiary (powerless/constrained) — gains devotional standing
 *   - women_practitioners: primary beneficiary (powerless/constrained) — gains devotional standing despite ritual exclusion
 *   - bhakti_lineage_teachers: agenda-setters (moderate/mobile) — administer devotional institutions
 *   - excluded_non_devotional_castes: partial payer (powerless/trapped) — devotional bypass does not fully reach social/economic caste structure
 *   - hereditary_priestly_class: structural payer (organized/constrained) — loses interpretive monopoly and fee income
 *   - temple_institutions: analytical observer (institutional) — mediates the tension administratively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.32).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading: Devotion Over Birth as Spiritual Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious authority / social stratification / interpretive legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '156adb5d-9d20-4247-a5e5-0a0fa122ced9').
narrative_ontology:cs_kernel_codification('156adb5d-9d20-4247-a5e5-0a0fa122ced9', distributed).
narrative_ontology:cs_authority_grounding('156adb5d-9d20-4247-a5e5-0a0fa122ced9', practice).
narrative_ontology:cs_interpretation_layer_present('156adb5d-9d20-4247-a5e5-0a0fa122ced9').
narrative_ontology:cs_reading_relation('156adb5d-9d20-4247-a5e5-0a0fa122ced9', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('156adb5d-9d20-4247-a5e5-0a0fa122ced9', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('156adb5d-9d20-4247-a5e5-0a0fa122ced9', foundational, devotional_sincerity_supersedes_birth_qualification).
narrative_ontology:cs_axiom_status(devotional_sincerity_supersedes_birth_qualification, holdable).
narrative_ontology:cs_axiom_grounding('156adb5d-9d20-4247-a5e5-0a0fa122ced9', devotional_sincerity_supersedes_birth_qualification, theological).
narrative_ontology:cs_axiom('156adb5d-9d20-4247-a5e5-0a0fa122ced9', foundational, direct_unmediated_divine_access_available_to_all_devotees).
narrative_ontology:cs_axiom_status(direct_unmediated_divine_access_available_to_all_devotees, holdable).
narrative_ontology:cs_axiom_grounding('156adb5d-9d20-4247-a5e5-0a0fa122ced9', direct_unmediated_divine_access_available_to_all_devotees, theological).
narrative_ontology:cs_reference_frame('156adb5d-9d20-4247-a5e5-0a0fa122ced9', hereditary_ritual_literacy_gatekeeping).
narrative_ontology:cs_drift_state('156adb5d-9d20-4247-a5e5-0a0fa122ced9', post_bhakti_movement_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('156adb5d-9d20-4247-a5e5-0a0fa122ced9', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, women_practitioners).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_lineage_teachers).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_non_devotional_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priestly_class).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotion_confers_spiritual_standing).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_access_is_not_birth_mediated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice bhakti worship — devotional singing, personal surrender to a chosen deity, pilgrimage, congregational chanting — as a path to direct divine relationship that does not require Sanskrit literacy, ritual purity credentials, or Brahmin intermediation. Gain religious standing and community respect through visible devotion that was previously foreclosed by birth status. Still live inside a broader caste economy that governs marriage, land, and occupation regardless of devotional standing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees, beneficiary,
    powerless, biographical, constrained, regional).

% Excluded from Vedic textual study and priestly ritual roles under the hereditary reading, but bhakti traditions (Mirabai, Andal, and successor lineages) hold that devotional intensity itself is the qualifying credential. Compose and perform devotional literature, lead congregational practice, and are venerated as saints in some lineages, while still lacking formal ritual office in most temple structures.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_practitioners, beneficiary,
    powerless, biographical, constrained, regional).

% Found and lead devotional lineages (sampradayas), teach that surrender and love of the divine supersede ritual birthright, and admit disciples across caste lines. Administer temples, mathas, and pilgrimage circuits organized around devotional practice rather than hereditary priesthood. Their authority rests on charisma and demonstrated devotion, which is more contestable and less institutionally entrenched than hereditary lineage claims.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_lineage_teachers, agenda_setter,
    moderate, generational, mobile, regional).

% Groups whose exclusion was justified on ritual-purity grounds continue to face social exclusion in daily life — commensality, temple entry disputes, marriage — even where a bhakti reading grants them theoretical spiritual equality. Devotional access does not automatically dissolve the caste economy governing land, labor, and social exchange, so the promised bypass is partial: real in worship, incomplete outside it.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_non_devotional_castes, payer,
    powerless, biographical, trapped, local).

% Brahmin ritual specialists whose exclusive claim to interpret scripture and perform certain rites is structurally challenged when devotional sincerity is treated as sufficient qualification. Some accommodate by becoming bhakti teachers themselves; others lose exclusive ritual fee income and interpretive monopoly where devotional communities bypass temple hierarchies entirely.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priestly_class, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priestly_class, excluded).

% Administer physical worship sites and must negotiate between hereditary ritual claims and popular devotional movements demanding broader access. Absorb the tension by creating separate devotional spaces, modifying entry rules, or resisting change depending on local political and economic pressure.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, temple_institutions, observer,
    institutional, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework through which people excluded from formal ritual education or hereditary priesthood can participate in organized religious life, coordinate collective worship (kirtan, bhajan, pilgrimage), and receive social recognition for devotional practice without requiring years of Sanskrit training or birth into a specific lineage.
% TRANSFER_FUNCTION: Redistributes religious authority and social recognition from hereditary ritual specialists toward demonstrated devotional practitioners; moves congregational attention, patronage, and pilgrimage economy activity away from temple-fee structures and toward devotional lineages and their teachers.
% ABSENT_VOICES: The hereditary priestly class experiences this reading as a direct erosion of interpretive monopoly but is not absent from the conversation — they contest it actively. More genuinely absent are the excluded castes' own historical voices in canon formation: bhakti movements were themselves led disproportionately by non-Brahmin poet-saints, but subsequent institutionalization of bhakti lineages has in places re-imported caste distinctions among teachers and disciples.
% DISAPPEARANCE_RATIONALE: If the bhakti reading were withdrawn as a legitimate interpretive option, centuries of devotional literature, congregational worship forms, and lineage institutions (from Alvars and Nayanars to Chaitanya Vaishnavism and Sikh-adjacent devotional movements) would lose their scriptural warrant; millions of practitioners whose religious identity is organized around personal devotion rather than ritual birthright would need to either return to hereditary gatekeeping or seek entirely new legitimating frameworks. Congregational institutions built on this reading would face existential legitimacy challenges.
% FOUNDING_PROBLEM: Sanskrit literacy, ritual purity requirements, and hereditary priestly monopoly excluded the vast majority of the population — by birth, by gender, by lack of formal education — from direct religious authority and meaningful participation in interpreting or accessing the divine.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and inscriptional/literary evidence (poet-saint corpora, temple donation records naming non-Brahmin patrons) corroborate that the founding problem was real and that bhakti movements produced measurable expansions of participation in specific historical periods. However, sociologists of contemporary Indian religion note that caste stratification persists inside many present-day bhakti institutions, so the founding problem is only partially resolved rather than fully live or fully dead — corroboration comes from outside the bhakti lineages themselves, from historical and sociological scholarship rather than lineage self-narration alone.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.40 — lower than the hereditary reading because bhakti practice genuinely opens participation and requires little institutional overhead (chanting, congregational worship, personal devotion), but not negligible, because devotional lineages themselves develop hierarchies (guru succession, disciple ranking, institutional patronage) that can re-import status stratification among practitioners over time, and because the promised bypass of caste does not extend automatically to the non-religious caste economy. Suppression is authored low-moderate (0.32): this reading does not require coercive enforcement against those it includes — its coordination function is largely voluntary and its spread historically occurred through persuasion, poetry, and popular movements rather than imposed compliance. Resistance is authored moderately high (0.55) because hereditary priestly interests have historically and presently resisted bhakti's leveling claims, producing genuine friction rather than passive acceptance. Accessibility collapse is moderate (0.35): the devotional path does not collapse alternatives so much as add one — hereditary and reformist readings remain live for other communities. Theater ratio rises mildly over the interval (0.20 to 0.28) reflecting the historically documented tendency for successful devotional lineages to develop their own institutional hierarchies over generations, some of which perform egalitarian ideals more than they practice them fully.
 *
 * DIRECTIONALITY LOGIC:
 *   Lower-caste devotees and women practitioners are coded as beneficiaries with low directionality because the reading structurally subsidizes their religious participation — it is the specific mechanism that grants them standing they would lack under the hereditary reading. The hereditary priestly class is coded as a payer because the reading directly erodes their interpretive and ritual-fee monopoly; their exit options are constrained rather than trapped because many have historically adapted by becoming bhakti teachers themselves. Excluded non-devotional castes are also coded payer/trapped, distinct from the beneficiary castes, because they capture the specific residual population for whom the devotional bypass is incomplete — this is the 'victim set shrinks but does not eliminate caste hierarchy' delta named in the story brief: some are freed by this reading, others remain within caste constraint despite it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusion from religious authority by birth and ritual-literacy requirements — is genuinely contested rather than cleanly resolved: bhakti movements demonstrably expanded participation in specific historical periods (corroborated by external historical and inscriptional scholarship, not just lineage self-narration), but subsequent institutionalization of devotional lineages has in places recreated internal hierarchy, meaning the mandate is only partially fulfilled. This prevents mislabeling the reading as either a fully resolved coordination success (it is not — caste stratification persists around and sometimes within devotional institutions) or a pure extraction mechanism (it is not — the coordination function of opening religious participation to previously excluded groups is real and well-corroborated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_reading_versus_hereditary_reading_foreclosure,
    'Does the bhakti devotional reading logically foreclose the hereditary monopoly reading, or can both be held simultaneously within different institutional pockets of the same broader tradition?',
    'Examine whether specific devotional lineages historically abolished hereditary ritual offices where they gained institutional control, versus merely operating as a parallel, non-competing track alongside hereditary temple structures.',
    'If bhakti practice historically replaced hereditary gatekeeping wherever it took root, the reading edge to hereditary_monopoly_reading should be authored as foreclosing rather than coexisting; the historical record instead shows parallel coexistence in most regions, supporting the coexists_with declaration used here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_reading_versus_hereditary_reading_foreclosure, conceptual, 'Whether bhakti and hereditary readings are mutually exclusive or coexist within the tradition.').

omega_variable(
    devotional_bypass_completeness,
    'Does devotional sincerity fully substitute for caste standing in practice, or only within the narrow domain of worship while caste continues to govern marriage, commensality, and land relations?',
    'Ethnographic and historical comparison of devotional communities'' internal marriage/commensality practices against their stated theological egalitarianism.',
    'If the bypass is worship-only, the victim set (excluded_non_devotional_castes) should remain non-empty even under this reading, as authored; if the bypass is comprehensive, the victim declaration should be dropped and the reading would shift toward a rope with no residual victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_bypass_completeness, empirical, 'Scope-of-bypass ambiguity: worship-domain equality versus comprehensive social equality.').

omega_variable(
    lineage_institutionalization_drift,
    'Do successful bhakti lineages, once institutionalized across generations, re-import caste-like stratification among their own teachers and disciples, eroding the reading''s original egalitarian function?',
    'Longitudinal study of specific sampradaya institutional records tracking teacher lineage composition and disciple admission patterns over multiple generations.',
    'If drift is substantial, the rising theater_ratio trajectory authored here understates the eventual extent of institutional re-stratification and the constraint would trend toward tangled_rope or piton over a longer interval than modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lineage_institutionalization_drift, empirical, 'Whether institutionalized bhakti lineages recreate the hierarchy they originally bypassed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t20, observed).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(vedi_tr_t40, observed).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(vedi_tr_t60, observed).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(vedi_tr_t80, observed).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(vedi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(vedi_be_t20, observed).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(vedi_be_t40, observed).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement_basis(vedi_be_t60, observed).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement_basis(vedi_be_t80, observed).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement_basis(vedi_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vedic_dharmic_corpus kernel. hereditary_monopoly_reading grounds spiritual authority in birth-lineage with expected high extraction and a concentrated priestly beneficiary class; reformist_egalitarian_reading subordinates textual authority to external constitutional equality principles with expected disruption to all traditional authority claims; this bhakti_devotional_reading grounds authority in devotional sincerity with moderate extraction and no concentrated beneficiary class. Each has its own epsilon and stakeholder structure per the ε-invariance principle; do not average or reconcile across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
