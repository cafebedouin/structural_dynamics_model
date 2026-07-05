% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Generational-Transmission Standard for Linguistic Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONLY the native-generation reading of the 'living
 *   language' kernel: the claim that a language is living if and only if
 *   native speakers transmit it generationally as a mother tongue in ordinary
 *   daily life, and that liturgical recitation without such transmission
 *   constitutes preservation of a corpse rather than genuine vitality. This
 *   reading emerged from and grounds a specific secular nationalist revival
 *   project's legitimacy claim — the language's transformation into a mother
 *   tongue is treated as evidence of successful national
 *   sovereignty-building. The coordination function (allocating revival
 *   resources toward producing native speakers) is real; the extraction lies
 *   in the standard's use to devalue liturgical and diaspora modes of
 *   transmission that persist independently of the nationalist project and
 *   that predate it by centuries.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movement: agenda_setter/beneficiary (institutional/arbitrage) — derives legitimacy from the native-transmission standard
 *   - revival_pedagogy_institutions: beneficiary (organized/mobile) — institutional prestige tied to the standard
 *   - liturgical_only_communities: payer (moderate/constrained) — centuries of liturgical transmission redescribed as non-vital
 *   - diaspora_heritage_speakers: payer (powerless/trapped) — partial fluency classified as deficient
 *   - comparative_linguists: observer (analytical/analytical) — documents the standard's contingent historical origin
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.52).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.58).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generational-Transmission Standard for Linguistic Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '604c6793-efa8-4a8c-bc7b-65e5d5663866').
narrative_ontology:cs_kernel_codification('604c6793-efa8-4a8c-bc7b-65e5d5663866', distributed).
narrative_ontology:cs_authority_grounding('604c6793-efa8-4a8c-bc7b-65e5d5663866', distributed).
narrative_ontology:cs_reading_relation('604c6793-efa8-4a8c-bc7b-65e5d5663866', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('604c6793-efa8-4a8c-bc7b-65e5d5663866', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('604c6793-efa8-4a8c-bc7b-65e5d5663866', foundational, native_acquisition_is_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_acquisition_is_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('604c6793-efa8-4a8c-bc7b-65e5d5663866', native_acquisition_is_necessary_for_vitality, conventional).
narrative_ontology:cs_axiom('604c6793-efa8-4a8c-bc7b-65e5d5663866', secondary, ritual_only_transmission_constitutes_death_not_life).
narrative_ontology:cs_axiom_status(ritual_only_transmission_constitutes_death_not_life, holdable).
narrative_ontology:cs_axiom_grounding('604c6793-efa8-4a8c-bc7b-65e5d5663866', ritual_only_transmission_constitutes_death_not_life, conventional).
narrative_ontology:cs_reference_frame('604c6793-efa8-4a8c-bc7b-65e5d5663866', nationalist_revival_success_criterion).
narrative_ontology:cs_drift_state('604c6793-efa8-4a8c-bc7b-65e5d5663866', contemporary_multilingual_heritage_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('604c6793-efa8-4a8c-bc7b-65e5d5663866', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, revival_pedagogy_institutions).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_heritage_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built schools, kindergartens, and state institutions around the claim that a language is only truly alive when transmitted as a mother tongue in the home. This standard converts revival success into political legitimacy — the nation-building project's authority rests partly on having produced native speakers where none existed a generation prior. Controls curricula, immigration absorption policy, and the definition of linguistic vitality used in state discourse.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_movement, beneficiary).

% Ulpan-style immersion programs, teacher-training bodies, and academies of the revived language derive funding, prestige, and institutional purpose directly from the native-transmission standard. Their entire methodology is validated by the premise that native transmission is the only real form of vitality; a looser standard (literary or liturgical) would reduce their institutional centrality.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, revival_pedagogy_institutions, beneficiary,
    organized, generational, mobile, national).

% Maintain the language through daily prayer, textual study, and ritual recitation across centuries, often as the primary vehicle of religious and communal identity, without raising children who speak it as a mother tongue. Under this standard their entire mode of transmission is redescribed as maintaining a corpse rather than sustaining a living tradition — a framing that devalues their communal practice in comparisons for funding, prestige, and recognition as authentic custodians of the language.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, civilizational, constrained, global).

% Grew up with partial fluency, passive comprehension, or liturgical-register competence outside a nation-building context, without full generational mother-tongue transmission. The standard classifies their relationship to the language as inherently deficient or non-vital regardless of the depth of their textual, cultural, or emotional engagement, and they have no institutional lever to contest the classification.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_heritage_speakers, payer,
    powerless, biographical, trapped, global).

% Study language vitality across traditions and note that the native-transmission standard is itself historically contingent, emerging from a specific 19th-20th century nationalist revival project rather than from a language-neutral linguistic criterion. They can document the standard's political origins without holding power to change its institutional application.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, teachable, measurable criterion for allocating scarce revival resources (curriculum design, teacher training, immigrant absorption programs) toward producing new native speakers rather than diffusing effort across every mode of linguistic engagement.
% TRANSFER_FUNCTION: Moves institutional legitimacy, funding, and the status of 'authentic living language' from communities sustaining the language through liturgy and study toward the nation-building apparatus that produced native child speakers; moves cultural prestige away from diaspora and liturgical speakers toward revival-generation families and institutions.
% ABSENT_VOICES: Liturgical communities who have sustained continuous textual engagement for centuries are rarely invited into the definitional conversation about what counts as 'living' — the standard is set by linguists and nation-builders aligned with the revival project, not by the custodial communities whose practice gets redescribed as non-vital.
% DISAPPEARANCE_RATIONALE: If the native-transmission standard vanished, the nationalist revival narrative would lose its distinguishing claim to uniqueness (many revived languages would then be classed alongside liturgically-preserved and literarily-productive languages as equally 'living'), funding allocations tied to native-speaker-count metrics would need new justification, and liturgical communities would gain equal standing in vitality discourse — a real redistribution of institutional legitimacy, not merely a terminological shift.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, a movement needed to demonstrate that a liturgical-and-literary language could be transformed into a genuine tool of daily national life, to ground claims of national sovereignty in linguistic continuity and distinguish the project from mere religious preservation.
% FOUNDING_PROBLEM_CORROBORATION: The revival institutions themselves attest the standard remains necessary to distinguish genuine vitality from museum preservation. Independent sociolinguists studying language revitalization worldwide (outside the beneficiary set) attest that the founding problem — proving a revived language could function as a national mother tongue — was substantially resolved decades ago, and that the standard now functions primarily to allocate ongoing institutional prestige rather than to solve a live transmission problem.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the standard does not simply forbid liturgical or literary modes of engagement — it coexists with them in discourse — but it does actively devalue them relative to the nationalist project's preferred criterion, redirecting funding, prestige, and definitional authority. Suppression (0.58) reflects the institutional weight behind enforcing the standard in school curricula, immigration absorption policy, and academic linguistics discourse, though it falls short of the total suppression a snare would show since liturgical communities retain full internal practice — they are demoted in prestige, not eliminated. Theater ratio is low-moderate (0.22) and rising slowly: the coordination function (training native speakers) remains substantially real, though its use as a comparative status marker against sibling readings grows more performative as the revival project matures and no longer needs to prove basic viability.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist movement and its pedagogy institutions sit near the beneficiary end: the standard is the metric by which their historical project claims success, and they administer its application in policy and curricula. Liturgical-only communities and diaspora heritage speakers sit near the target end: their mode of engagement with the language is structurally recoded as insufficient by a standard they did not set and cannot easily satisfy, since satisfying it would require abandoning their actual mode of transmission (liturgical/textual) for the nationalist mode (native domestic speech). Diaspora heritage speakers are trapped rather than merely constrained because their partial fluency is a biographical fact they cannot retroactively convert into native mother-tongue status.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare preserves the fact that the native-transmission standard did solve a genuine coordination problem for the revival project — pedagogical resources genuinely needed a target criterion, and 'raise children who speak this as a mother tongue' is a coherent, achievable, resource-allocating goal. Calling it a pure snare would erase that real coordination achievement. But calling it a pure rope (as revival institutions would prefer) would erase the asymmetric cost imposed on liturgical and diaspora communities who are not seeking to build a nation-state around the language and who bear no benefit from being redescribed as custodians of a corpse. The tangled_rope classification holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness_vs_construction,
    'Is ''native generational transmission as mother tongue'' a linguistically neutral criterion for vitality, or is it a criterion selected because it happened to match what the 20th century nationalist revival project could demonstrate, thereby retroactively defining vitality in the terms of that project''s own success metric?',
    'Historical analysis of when and by whom the native-transmission criterion was first proposed as THE definition of linguistic life, cross-referenced against comparative sociolinguistic literature on language vitality assessment (e.g. UNESCO vitality frameworks, which use multi-factor scales rather than a single native-transmission binary) to establish whether the criterion predates or postdates the revival project''s institutional interests.',
    'If the criterion was selected post hoc to validate the revival project''s achievement, this reading is better modeled as a constructed legitimacy claim (raising its true extraction above the authored 0.52); if it reflects independent linguistic consensus that predates the nationalist application, the extraction is closer to a genuine, if contestable, scholarly standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness_vs_construction, conceptual, 'Whether the native-transmission standard is an independent linguistic criterion or a retrofit of the revival project''s own success condition.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the living_language_status kernel disagree — is it about what ''life'' metaphorically means for a language (a conceptual dispute) or about which observable transmission behaviors should count as evidence of that life (an empirical dispute)?',
    'Systematic comparison of the three readings'' underlying premises: literary_continuity_reading locates vitality in productive textual output; liturgical_preservation_reading locates it in continuous ritual use; native_generation_reading locates it in generational native acquisition. Each targets a different observable, but the deeper disagreement concerns whether ''life'' for a language is defined by intergenerational biological-cultural transmission specifically, or by any sustained active use.',
    'If the disagreement is purely conceptual (about the meaning of ''life'' as applied to language), no amount of additional data about transmission patterns would resolve which reading is correct — all three remain permanently coexisting framings. If partly empirical (e.g., whether liturgical-only transmission in fact sustains full linguistic competence across generations, or only fragmentary competence), some empirical findings could shift adherents between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel''s readings differ over the meaning of linguistic life or over empirical transmission facts, and where in the kernel structure this disagreement actually sits.').

omega_variable(
    diaspora_speaker_reclassification_pressure,
    'Could diaspora heritage speakers and liturgical communities organize to contest the native-transmission standard''s application to funding and prestige allocation, given their combined numbers exceed any single revival-generation cohort?',
    'Track whether liturgical and diaspora communities have historically formed coalitions to advocate for multi-criterion vitality standards (e.g., in UNESCO language documentation policy, in diaspora heritage-language funding debates) and whether such coalitions have shifted institutional definitions.',
    'If coalition formation is documented and effective, the powerless/moderate power ratings for these payer groups understate their latent coalition power, which would suggest the constraint is less stable than the current suppression score implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_speaker_reclassification_pressure, empirical, 'Whether liturgical and diaspora communities have latent coalition power against the native-transmission standard that current power ratings do not capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t8, living_language_status__native_generation_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(livi_tr_t16, living_language_status__native_generation_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(livi_tr_t24, living_language_status__native_generation_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(livi_tr_t32, living_language_status__native_generation_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(livi_be_t8, living_language_status__native_generation_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(livi_be_t16, living_language_status__native_generation_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(livi_be_t24, living_language_status__native_generation_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(livi_be_t32, living_language_status__native_generation_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(livi_su_t8, living_language_status__native_generation_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(livi_su_t16, living_language_status__native_generation_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(livi_su_t24, living_language_status__native_generation_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(livi_su_t32, living_language_status__native_generation_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'living language' (kernel_id: living_language_status). Each sibling reading locates vitality in a different observable (native generational acquisition here; continuous liturgical ritual use in the liturgical_preservation_reading sibling; productive literary/intellectual output in the literary_continuity_reading sibling) and each carries a distinct ε, beneficiary set, and victim set — per the ε-invariance principle these are not one constraint measured three ways but three structurally distinct constraints linked through the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
