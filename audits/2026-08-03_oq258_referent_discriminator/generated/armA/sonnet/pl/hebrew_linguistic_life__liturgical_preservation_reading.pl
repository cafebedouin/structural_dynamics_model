% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical Continuity as the Criterion of Hebrew's Life (Sacred-Text Transmission Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   For roughly two thousand years, Hebrew had no population of native
 *   vernacular speakers, yet it was continuously recited in daily and
 *   festival liturgy, studied in yeshivot, and used as the exclusive medium
 *   for an unbroken chain of rabbinic legal and exegetical literature. This
 *   reading holds that this continuous liturgical-scholarly use IS the
 *   criterion by which a language's life should be judged, and that the
 *   19th-century Hebrew revival movement associated with Eliezer Ben-Yehuda
 *   did not resurrect a dead language but instead appropriated a living
 *   sacred register for secular nationalist purposes, extracting from
 *   centuries of transmission to manufacture a modern vernacular that hollows
 *   the sacral function even as it borrows the sacral vocabulary.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_authorities: agenda_setter (institutional/identity_locked) — administer and certify correct transmission
 *   - diaspora_liturgical_communities: beneficiary (organized/identity_locked) — validated as living-language speakers without vernacular requirement
 *   - yeshiva_institutions: beneficiary/agenda_setter (institutional/identity_locked) — transmission infrastructure whose mission depends on this criterion
 *   - sacred_liturgical_tradition_itself: payer, non-agent (moderate/trapped) — the register extracted from by secular revival
 *   - traditionalist_hebrew_readers: payer (moderate/constrained) — marginalized by state-sponsored Modern Hebrew framing them as speaking a ceremonial-only language
 *   - zionist_revivalist_linguists: excluded (organized/mobile) — their entire revival narrative is defined out of the frame as category error
 *   - academic_sociolinguists: observer (analytical/analytical) — apply rival vitality criteria this reading rejects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical Continuity as the Criterion of Hebrew's Life (Sacred-Text Transmission Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '889d4f04-f09b-4c41-a390-6bc8c8afa7af').
narrative_ontology:cs_kernel_codification('889d4f04-f09b-4c41-a390-6bc8c8afa7af', distributed).
narrative_ontology:cs_authority_grounding('889d4f04-f09b-4c41-a390-6bc8c8afa7af', lineage).
narrative_ontology:cs_interpretation_layer_present('889d4f04-f09b-4c41-a390-6bc8c8afa7af').
narrative_ontology:cs_reading_relation('889d4f04-f09b-4c41-a390-6bc8c8afa7af', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('889d4f04-f09b-4c41-a390-6bc8c8afa7af', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('889d4f04-f09b-4c41-a390-6bc8c8afa7af', foundational, unbroken_transmission_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(unbroken_transmission_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('889d4f04-f09b-4c41-a390-6bc8c8afa7af', unbroken_transmission_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('889d4f04-f09b-4c41-a390-6bc8c8afa7af', foundational, vernacular_native_acquisition_is_not_necessary_for_life).
narrative_ontology:cs_axiom_status(vernacular_native_acquisition_is_not_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('889d4f04-f09b-4c41-a390-6bc8c8afa7af', vernacular_native_acquisition_is_not_necessary_for_life, conventional).
narrative_ontology:cs_axiom('889d4f04-f09b-4c41-a390-6bc8c8afa7af', secondary, revival_project_constitutes_appropriation_not_resurrection).
narrative_ontology:cs_axiom_status(revival_project_constitutes_appropriation_not_resurrection, holdable).
narrative_ontology:cs_axiom_grounding('889d4f04-f09b-4c41-a390-6bc8c8afa7af', revival_project_constitutes_appropriation_not_resurrection, conventional).
narrative_ontology:cs_reference_frame('889d4f04-f09b-4c41-a390-6bc8c8afa7af', continuous_liturgical_transmission_as_life).
narrative_ontology:cs_drift_state('889d4f04-f09b-4c41-a390-6bc8c8afa7af', post_1948_israeli_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('889d4f04-f09b-4c41-a390-6bc8c8afa7af', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_hebrew_readers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, premodern_textual_transmission_chain).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_transmission_constitutes_linguistic_life).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sanctity_of_text_independent_of_vernacular_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the standards of correct recitation, study curricula, and transmission chains (masorah) that define what counts as continuous liturgical Hebrew. Their institutional authority and scholarly prestige are constituted by being the certified custodians of this unbroken chain; they set what counts as faithful transmission and enforce it through communal and educational structures.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities, beneficiary).

% Recite and study the sacred texts in synagogue, home, and study-hall contexts for centuries without needing Hebrew as a vernacular. This reading validates their entire linguistic-religious practice as sufficient for the language's life, without requiring them to speak Hebrew day-to-day.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Operate as the transmission infrastructure — training successive generations in textual recitation and study. Their institutional mission and funding model depend on the premise that this study-and-recitation activity IS the language's life, not merely its preservation as an artifact.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions, agenda_setter).

% The register, cadence, and semantic field of liturgical Hebrew — as this reading construes the victim — is treated as raw material for a nationalist vernacular-revival project (Ben-Yehuda's Modern Hebrew) that repurposes sacred vocabulary for secular street use, sports commentary, and bureaucratic administration, hollowing the sacral register even as it claims continuity with it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself, payer,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself).

% Communities (e.g., segments of Haredi and other traditionalist Jewish populations) who maintain that liturgical Hebrew alone constitutes the living language find their standard increasingly marginalized by state-sponsored Modern Hebrew education and media; they bear the cost of being cast as speaking a 'dead' or merely ceremonial language despite continuous transmission.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_hebrew_readers, payer,
    moderate, generational, constrained, national).

% The chain of masoretic and rabbinic transmission that this reading treats as sufficient proof of life is, under the revivalist counter-narrative, recast as a mere museum artifact awaiting resurrection — a framing that devalues the transmission chain's own claim to already constitute living language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, premodern_textual_transmission_chain, payer,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, premodern_textual_transmission_chain).

% Would object strenuously to this reading's premise, holding instead that Hebrew was functionally dead as a vernacular before Ben-Yehuda and required deliberate revival; they are not addressed within this reading's frame because the reading defines their revival narrative as a category error (desecration) rather than a competing empirical claim.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, zionist_revivalist_linguists, excluded,
    organized, generational, mobile, national).

% Study language vitality using criteria (native acquisition, domain coverage, intergenerational transmission of vernacular competence) that this reading explicitly rejects as the sole test of life, positioning them as adjudicators of a dispute the reading itself frames as a category mistake.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, academic_sociolinguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational community of practice around correct recitation, study method, and interpretive tradition — enabling dispersed communities separated by centuries and continents to share a single liturgical-textual register with mutual intelligibility and continuity of meaning.
% TRANSFER_FUNCTION: Moves interpretive authority and communal prestige toward the institutions and scholars certified as custodians of correct transmission, and moves legitimacy away from vernacular-revival narratives that would otherwise claim credit for 'bringing Hebrew back to life.'
% ABSENT_VOICES: Zionist revivalist linguists and the modern Israeli linguistic-nationalist establishment are structurally excluded from this reading's frame — their entire evidentiary basis (absence of native speakers pre-1880s) is treated as irrelevant rather than engaged, because the reading's axiom makes vernacular status a non-criterion.
% DISAPPEARANCE_RATIONALE: If the liturgical-continuity criterion were abandoned overnight, rabbinic and yeshiva institutions would lose their claim to be custodians of a living language rather than a heritage language, and traditionalist communities would lose the linguistic-religious status distinction between their practice and mere historical preservation. Revivalist and nationalist framings would gain uncontested field. Whether 'the world' rearranges depends entirely on whose world: institutional and traditionalist actors say it would; secular nationalist and academic sociolinguistic actors say nothing observable would change since they already operate on the rival criterion.
% FOUNDING_PROBLEM: The problem this criterion was built to address: how to characterize the status of a language with millennia of continuous textual use, recitation, and scholarly transmission but without, for many centuries, a corresponding population of native vernacular speakers — without conceding that such a language was 'dead' in the ordinary sense applied to Latin or Sumerian.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist rabbinic scholars and yeshiva heads attest the criterion still answers a live problem (defending the coherence of unbroken liturgical practice against a 'dead language' framing they regard as insulting and inaccurate). Independent historical sociolinguists outside the beneficiary set (analyzing pre-revival Hebrew's domain restriction to religious/scholarly registers) corroborate that liturgical Hebrew was continuously used but contest that continuous liturgical use is sufficient, on its own terms, to satisfy what most linguists mean by 'living language' — so the corroboration exists but does not resolve the underlying definitional dispute.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that under this reading, the primary harm is the secular nationalist appropriation of sacred vocabulary and register for street, bureaucratic, and military use — a real transfer of semantic and cultural capital away from the tradition that sustained it, intensifying as Modern Hebrew's state-sponsored spread accelerated over the 20th century (hence the rising trajectory to ~1948 and plateauing post-statehood). Suppression (0.62) captures the active marginalization of the traditionalist framing itself in nationalist historiography and education — the 'dead language revived' narrative is taught as settled fact, suppressing the liturgical-continuity framing as a live alternative. Theater ratio is modest (0.28): the custodial and transmission function is genuinely functional, not primarily performative, though certification and credentialing activity has grown somewhat theatrical as the practical stakes (state recognition, institutional funding) have risen. Accessibility collapse is moderate (0.5): the liturgical framing has not vanished — it persists in traditionalist and academic circles — but has been substantially crowded out of mainstream discourse by the revival narrative. Resistance is high (0.72): traditionalist scholars and communities actively contest the 'revival' framing in religious and academic literature.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities, yeshiva institutions, and diaspora liturgical communities are beneficiaries: this reading validates their entire practice as sufficient proof of linguistic life, without requiring them to adopt vernacular Hebrew, so directionality sits near the beneficiary end (d low). The sacred tradition itself and traditionalist readers are victims: their register and status are extracted from and marginalized respectively, so d sits near the target end, amplified by their identity-locked or constrained exit (they cannot simply adopt a different tradition or framing without abandoning constitutive religious commitments). Zionist revivalist linguists are excluded rather than positioned on the beneficiary/victim axis at all — the reading does not coordinate them, it defines their claim as a category mistake, which is why they sit outside base_properties beneficiaries/victims and are marked excluded here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing continuous sacred-textual use from genuine linguistic death (the Latin/Sumerian case) — remains contested rather than resolved: traditionalist and rabbinic seats hold it fully live, while the secular nationalist and academic sociolinguistic seats regard the distinction as no longer doing useful work once a native-speaker vernacular exists (post-1948 Israeli Hebrew). Classifying this as tangled_rope rather than snare acknowledges that the criterion does real coordinating work for dispersed liturgical communities (shared textual register across two millennia and multiple continents) even as it is deployed, under this reading, to delegitimize a rival vernacular-revival narrative that some of the same population also participates in — the same structure that lets Yemenite, Ashkenazi, and Sephardic communities share prayer language also lets rabbinic authorities police what counts as 'real' Hebrew continuity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_vernacular_life_criterion,
    'Is continuous liturgical/scholarly transmission alone sufficient for a language to be ''alive,'' or does linguistic life require native vernacular acquisition and daily secular use (the native_generational_reading), or merely functional inter-communal use (the marketplace_pidgin_reading)?',
    'This is fundamentally a definitional/conceptual dispute rather than an empirically resolvable one — sociolinguistic literature on ''sacred languages,'' ''liturgical languages,'' and language death/revival offers competing typologies (Fishman''s diglossia framework, Spolsky''s domain-based vitality criteria) without consensus. Historical evidence about pre-1880s Hebrew usage (letters, legal documents, maskilic literature) can partially inform but not settle which criterion is correct, since the dispute is about which criterion should govern the label ''alive,'' not about the underlying facts of usage.',
    'If the liturgical criterion is accepted as sufficient, Ben-Yehuda''s project is rightly read as appropriation/desecration of an already-living tradition (this story''s classification). If native vernacular acquisition is required, the same historical facts support a revival/resurrection narrative instead (the native_generational_reading, where Hebrew''s ''death'' as vernacular is the founding problem the revival solved) — a structurally different constraint with different beneficiaries and victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_vs_vernacular_life_criterion, conceptual, 'The kernel''s central definitional contest: what makes a language ''alive'' — and which reading of Hebrew''s status is correct.').

omega_variable(
    sacred_tradition_as_victim_coherence,
    'Can an abstract entity like ''the sacred liturgical tradition'' coherently be named as a victim/payer, or does this personify a normative claim (that sacralization deserves protection) as if it were a party bearing costs?',
    'Compare against how the framework treats other non-agent payers (e.g., environmental commons, future generations) — if those are accepted as payer/victim entities elsewhere in the corpus, the same treatment is defensible here; if not, this entry may need reframing as harm to traditionalist_hebrew_readers and yeshiva_institutions only, with the tradition itself demoted to a vindicated proposition rather than a victim.',
    'If ''sacred_liturgical_tradition_itself'' is not a coherent victim entity, the victim set narrows to the human communities harmed (traditionalist_hebrew_readers), which would somewhat reduce the story''s claimed extraction magnitude but not its tangled_rope classification, since human victims remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_as_victim_coherence, conceptual, 'Whether an abstract tradition can coherently occupy the payer/victim role independent of the human communities that carry it.').

omega_variable(
    revival_desecration_framing_reflexivity,
    'Does the liturgical_preservation_reading''s own beneficiary set (rabbinic authorities, yeshiva institutions) have an independent institutional interest in denying that Hebrew was ever ''dead'' — i.e., is the ''no revival needed, only desecration'' framing itself partly a legitimacy-preservation move by custodial institutions rather than a disinterested linguistic claim?',
    'Examine whether rabbinic/yeshiva institutional statements on this question predate or postdate the rise of competing state-sponsored Modern Hebrew educational authority; a framing that hardens specifically in response to loss of institutional monopoly over Hebrew''s definition would support the self-interest reading.',
    'If the framing is substantially motivated by custodial institutions'' interest in retaining authority over what counts as ''real'' Hebrew, the extractiveness attributed to the revival project should be read partly as inter-institutional competition for legitimacy rather than pure extraction from a passive sacred tradition — which would still support tangled_rope classification but shift the balance of who is doing the extracting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_desecration_framing_reflexivity, empirical, 'Whether the reading''s own beneficiaries have institutional motive to author this framing, independent of its truth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(hebr_tr_t90, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 90, 0.27).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement(hebr_tr_t140, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 140, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hebr_be_t90, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 90, 0.58).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(hebr_be_t140, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 140, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(hebr_su_t90, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 90, 0.62).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.62).
narrative_ontology:measurement(hebr_su_t140, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 140, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'Hebrew language revival/continuity' per the ε-invariance principle: liturgical_preservation_reading (this file, tangled_rope, ε=0.58 — coordination of dispersed liturgical communities plus extraction of sacred register by secular nationalism), native_generational_reading (the standard revivalist historiography, where Hebrew's vernacular death is the founding problem solved by deliberate 20th-century revival — a structurally different constraint with different beneficiaries/victims and likely a different type), and marketplace_pidgin_reading (life = functional inter-communal medium regardless of sacred or native status, likely closer to rope — coordination with minimal extraction). Each reading is generated as its own file with its own stable ε; they are linked here rather than merged because the label 'Hebrew is/was alive' conflates three structurally distinct claims about what 'alive' means and who bears which costs under each definition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
