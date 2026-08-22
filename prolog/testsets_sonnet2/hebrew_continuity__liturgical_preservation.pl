% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew as Living Language via Liturgical/Textual Preservation
 *   domain: sociolinguistics/religious institutions
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'Hebrew continuity'
 *   kernel: the liturgical_preservation reading, which holds that Hebrew's
 *   life is constituted by fidelity of ritual recitation and textual
 *   transmission across diaspora communities, independent of whether anyone
 *   speaks it generatively as a mother tongue. Under this reading, zero
 *   native speakers are required for the language to be considered 'alive' —
 *   what matters is unbroken chains of recitation, correct pronunciation
 *   transmission, and manuscript/textual fidelity, maintained by rabbinic and
 *   educational institutions. The victims this reading identifies are
 *   secularizing forces that threaten to erode textual/ritual transmission,
 *   not linguistic decline in the ordinary sense. This is a distinct
 *   constraint from the sibling readings (native_generative,
 *   bridge_pidginized), which have their own ε values, beneficiary/victim
 *   structures, and classifications in separate story files.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Primary agenda-setter (institutional/identity_locked) — certifies authentic transmission
 *   - orthodox_educational_institutions: Primary beneficiary (organized/constrained) — builds curricula and legitimacy on this framing
 *   - diaspora_congregants: Beneficiary/payer (moderate/constrained) — invests years of study for communal belonging
 *   - secularizing_diaspora_communities: Excluded/framed-as-threat (powerless/mobile)
 *   - linguistic_historians: Analytical observer (analytical/analytical) — assesses whether the claim is linguistically substantive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.28).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.22).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.28).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew as Living Language via Liturgical/Textual Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/religious institutions").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '06cc18b3-1c78-4552-8bfa-67c48c07c12f').
narrative_ontology:cs_kernel_codification('06cc18b3-1c78-4552-8bfa-67c48c07c12f', fixed_text).
narrative_ontology:cs_authority_grounding('06cc18b3-1c78-4552-8bfa-67c48c07c12f', lineage).
narrative_ontology:cs_interpretation_layer_present('06cc18b3-1c78-4552-8bfa-67c48c07c12f').
narrative_ontology:cs_reading_relation('06cc18b3-1c78-4552-8bfa-67c48c07c12f', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('06cc18b3-1c78-4552-8bfa-67c48c07c12f', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('06cc18b3-1c78-4552-8bfa-67c48c07c12f', foundational, textual_fidelity_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(textual_fidelity_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('06cc18b3-1c78-4552-8bfa-67c48c07c12f', textual_fidelity_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('06cc18b3-1c78-4552-8bfa-67c48c07c12f', foundational, native_speaker_competence_not_required_for_continuity).
narrative_ontology:cs_axiom_status(native_speaker_competence_not_required_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('06cc18b3-1c78-4552-8bfa-67c48c07c12f', native_speaker_competence_not_required_for_continuity, conventional).
narrative_ontology:cs_reference_frame('06cc18b3-1c78-4552-8bfa-67c48c07c12f', unbroken_masoretic_recitation_chain).
narrative_ontology:cs_drift_state('06cc18b3-1c78-4552-8bfa-67c48c07c12f', post_israeli_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('06cc18b3-1c78-4552-8bfa-67c48c07c12f', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, orthodox_educational_institutions).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_congregants).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_congregants).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, textual_continuity_constitutes_language_life).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, recitation_fidelity_preserves_linguistic_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine which recitation practices, pronunciation traditions, and textual variants count as authentic transmission. Their institutional legitimacy rests on being the custodians of continuous practice stretching back millennia; they cannot exit this role without dissolving the basis of their own authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Yeshivot and day schools organize curricula around textual and liturgical Hebrew competence. Their institutional funding, prestige, and enrollment depend on the continued belief that this mode of Hebrew constitutes genuine linguistic continuity, independent of conversational fluency.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, orthodox_educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Recite prayers and read scripture in Hebrew often without conversational fluency. They receive a sense of continuity, belonging, and access to tradition, but also bear the cost of years of study devoted to a register they may never use generatively, and can feel alienated when the register does not transfer to actual communication.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_congregants, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_congregants, payer).

% Philologists and textual critics who study manuscript transmission and liturgical variation benefit professionally from the premise that textual fidelity is the primary site of linguistic continuity; their scholarly authority is built on this framing.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_scholars, beneficiary,
    moderate, civilizational, mobile, global).

% Jews drifting from ritual observance toward secular or purely cultural identification are treated within this reading as a threat to textual transmission rather than as a legitimate alternative mode of continuity. Their preference for vernacular or secular Jewish identity is not represented in the kernel's own terms of legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_diaspora_communities, excluded,
    powerless, biographical, mobile, regional).

% Live daily generative Hebrew entirely outside the liturgical frame; from this reading's perspective they are largely irrelevant to what constitutes Hebrew's 'life,' even though they represent the largest population of Hebrew users by far. Their existence is not addressed by this reading's coordination logic.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, israeli_hebrew_speakers, excluded,
    organized, generational, analytical, national).

% Study whether liturgical preservation constitutes 'language continuity' in a meaningful linguistic sense, or whether it is closer to the preservation of a ritual register comparable to Latin in Catholic liturgy. Their analysis can validate or undercut the reading's core claim.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a shared textual and recitational tradition across geographically dispersed communities without requiring universal conversational fluency, allowing continuity of religious practice, scriptural access, and communal identity across two millennia of diaspora.
% TRANSFER_FUNCTION: Moves time, attention, and formative educational years from congregants and students toward mastery of liturgical recitation and textual reading, in exchange for communal belonging, access to tradition, and religious legitimacy; moves authority and interpretive control toward rabbinic institutions that certify correct transmission.
% ABSENT_VOICES: Secularizing diaspora communities who see Jewish identity as sustainable without ritual Hebrew are treated as the threat this reading exists to resist, not as parties with a legitimate competing account. Israeli native speakers, who constitute the overwhelming numerical majority of Hebrew's actual daily use, are also outside this reading's frame of reference entirely.
% DISAPPEARANCE_RATIONALE: If liturgical Hebrew recitation vanished, rabbinic authorities and religious educational institutions would lose a central pillar of their legitimacy and funding model — the world clearly rearranges for them. But proponents of the native_generative reading would argue nothing of linguistic substance was lost, since Hebrew already lives fully through Israeli daily speech; the verdict itself is a site of the kernel contest, not settled fact.
% FOUNDING_PROBLEM: After the practical cessation of Hebrew as a majority vernacular among diaspora Jews (roughly the early centuries CE onward), a mechanism was needed to prevent the language of scripture and prayer from becoming as inaccessible as a dead classical language, while diaspora communities adopted local vernaculars for daily life.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and educational institutions attest the problem remains live: without continued liturgical transmission, textual access to core religious sources would degrade. Independent linguistic historians outside these institutions note that the founding problem was substantially resolved by the 20th-century revival of Hebrew as a spoken vernacular in Israel, and that liturgical preservation now functions primarily as a religious-identity practice rather than a language-continuity mechanism — corroboration for 'dead' status comes from comparative linguists studying liturgical-register-only languages (e.g., Sanskrit, Ge'ez, Church Slavonic) who classify Hebrew's diaspora liturgical use in the same structural category.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, contested).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).
:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because the coordination function is largely genuine: preserving textual/recitational continuity does provide real communal and religious value, and the 'cost' borne by congregants (years of study) is more accurately described as an investment they largely endorse than an extraction. Suppression is low (0.22) because exit from this framework — adopting a secular Jewish identity, or treating Israeli vernacular Hebrew as sufficient — is available and increasingly common, not violently foreclosed, though social and familial pressure exists. Theater ratio rises over the interval (0.2 to 0.4) reflecting a real dynamic: as Israeli vernacular Hebrew becomes globally dominant and accessible, diaspora liturgical Hebrew increasingly persists as performative/identity maintenance rather than functionally necessary transmission — the textual community could access Hebrew texts via Israeli-standard pronunciation and grammar without the specific diaspora liturgical apparatus, but institutions continue to require it as a marker of authenticity.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and educational institutions sit near the beneficiary end: they administer the framework and derive authority/legitimacy/funding from it, with identity-locked or constrained exit (their institutional existence depends on this reading being correct). Diaspora congregants are near-symmetric: real communal benefit balanced against a real time/attention cost, hence dual beneficiary/payer role. Secularizing communities and Israeli native speakers are excluded rather than extracted-from in the technical sense — this reading does not extract from them so much as render them structurally invisible to its own coordination logic, which is why no victims are declared in base_properties despite the story's stated 'victim set' framing being about secularizing forces as a threat to the tradition, not agents the constraint extracts from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing scriptural/liturgical Hebrew from becoming as remote as a dead classical language during centuries of diaspora vernacular adoption — was substantively resolved by the 20th-century Israeli vernacular revival, which restored Hebrew as a living daily language through an entirely different mechanism (native_generative reading). This reading's classification as rope-with-rising-theater rather than piton reflects that its coordination function (communal religious continuity, textual access) remains genuinely valued and voluntarily sustained, even though its ORIGINAL justification (preventing total language death) has been superseded by events outside its own frame. The status is authored 'contested' rather than 'dead' because rabbinic authorities dispute that Israeli vernacular Hebrew substitutes for liturgical fidelity — they hold these are different goods, not that one function has replaced the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_fidelity_vs_generative_life_boundary,
    'Does preserved recitation without native generative competence constitute the language ''living,'' or is this reading better described as preserving a ritual register (structurally comparable to liturgical Latin, Sanskrit, or Ge''ez) rather than preserving Hebrew as a living language in the linguistic sense?',
    'Comparative sociolinguistic analysis against known cases of liturgical-only language persistence (Latin, Sanskrit, Ge''ez, Church Slavonic) to determine whether Hebrew''s diaspora liturgical use follows the same trajectory toward eventual classification as a liturgical/classical rather than living language.',
    'If the comparison holds, this reading''s claimed_type of rope becomes harder to sustain against a piton reading — a coordination function whose original justification (preventing dead-language status) has been overtaken by the Israeli revival, surviving now primarily as institutional/identity maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_fidelity_vs_generative_life_boundary, conceptual, 'Whether liturgical-only transmission is definitionally sufficient for ''language life'' or is a distinct category from generative vernacular life.').

omega_variable(
    kernel_framing_choice_liturgical_vs_native,
    'Is the liturgical_preservation reading the historically dominant framing of Hebrew continuity, or is it increasingly a minority reading eclipsed by the native_generative reading following the 20th-century revival — and does this framing choice change which reading should be treated as the ''default'' constraint when only one story is generated?',
    'Track the relative institutional and demographic weight given to each reading over time: pre-1880s discourse on Hebrew continuity was almost entirely liturgical_preservation-framed; post-revival discourse (especially within Israel) is dominated by native_generative framing, while diaspora religious institutions continue liturgical_preservation framing largely undisturbed.',
    'If liturgical_preservation is now a minority/legacy framing relative to native_generative, this story''s beneficiaries (rabbinic authorities, religious educational institutions) may be read as maintaining a framing whose centrality has been superseded — relevant to whether this reading should be treated as coexisting with or subordinate to the native_generative reading in any aggregate account of ''Hebrew continuity.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_liturgical_vs_native, conceptual, 'Alternative framing under-determination: which reading is treated as primary affects downstream analysis of the kernel family.').

omega_variable(
    secularization_as_victim_or_alternative,
    'Is secularization correctly modeled as a ''threatening force'' (implying this reading has victims it suppresses) or as a legitimate competing account of Jewish continuity that this reading simply does not recognize (implying no victims, only exclusion)?',
    'Examine whether rabbinic/educational institutions actively resist or penalize secularizing exit (suppression) versus merely failing to represent it in their own success metrics (exclusion without suppression).',
    'If active resistance/penalty exists (e.g., social sanction, family estrangement, institutional gatekeeping of resources), the victims array should be populated and the type reconsidered toward tangled_rope; if only non-representation exists, the current victim-free rope framing with an excluded stakeholder is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularization_as_victim_or_alternative, empirical, 'Whether secularizing communities are extracted-from (victims) or merely unrepresented (excluded) by this reading''s coordination logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.25).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.3).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__liturgical_preservation, theater_ratio, 60, 0.33).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__liturgical_preservation, theater_ratio, 80, 0.37).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__liturgical_preservation, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__liturgical_preservation, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_continuity__liturgical_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language claim 'Hebrew lives' per the ε-invariance principle. liturgical_preservation (this file) authors Hebrew's continuity as constituted by ritual recitation and textual fidelity, with rabbinic/educational institutions as beneficiaries and no direct victims (secularizing forces are framed as excluded/threatening rather than extracted-from). native_generative authors continuity as constituted by daily vernacular use among Israeli speakers, structurally independent of liturgical practice, and would likely show near-Mountain or clean Rope characteristics given the revival's broad-based, low-coercion success. bridge_pidginized authors continuity as an incomplete, transitional contact-language use among diaspora communities, likely Scaffold-shaped given its inherently transitional character between liturgical-only and fully native use. Each story carries its own ε and classification; they are linked here rather than merged because measuring 'Hebrew continuity' by recitation fidelity versus generative fluency versus contact-language use yields materially different extraction, suppression, and beneficiary/victim profiles — exactly the decomposition the ε-invariance principle requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
