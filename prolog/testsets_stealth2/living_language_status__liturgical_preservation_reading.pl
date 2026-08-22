% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Liturgical Transmission Sufficiency Criterion for Living-Language Status
 *   domain: sociolinguistics/religious studies/nationalism studies
 *
 * SUMMARY:
 *   This story instantiates the liturgical_preservation_reading of the
 *   contested kernel living_language_status: the claim that a language counts
 *   as living if its sacred texts are continuously recited, studied, and used
 *   in ritual, so that preservation through liturgical transmission suffices.
 *   The paradigm case is Hebrew between the cessation of everyday vernacular
 *   use and the modern revival: for some seventeen centuries the language's
 *   textual and ritual continuity was carried entirely by recitation cycles,
 *   study curricula, and the rabbinic interpretive apparatus, and the
 *   criterion underwrites the verdict that the language never died. The
 *   criterion solves a real coordination problem (dispersed, stateless
 *   communities maintaining an identical corpus) while simultaneously
 *   conferring interpretive monopoly on the custodianship and casting secular
 *   usage as profanation rather than participation. Per the
 *   epsilon-invariance principle, the kernel label decomposes into three
 *   structurally distinct constraints; this file authors only the liturgical
 *   reading, with epsilon indexed to the standing arrangement (the
 *   criterion's operation) as this reading itself assesses it: low,
 *   coordination around a fixed liturgical corpus, conceding the
 *   delegitimization cost the structure imposes on secular users. The claimed
 *   type and the metrics are authored independently: the type comes from the
 *   structure (named coordination function, named beneficiary capturing
 *   authority, named payers, documented enforcement history), not from tuning
 *   to the low reading-indexed epsilon. Interval mapping: T0 = 1780 (Haskalah
 *   onset, when the criterion first faced organized rivals), T240 = 2020.
 *
 * KEY AGENTS:
 *   - - rabbinical_authority: Agenda-setting custodian (institutional/identity_locked) — administers recitation cycles, study standards, and usage norms; collects the interpretive authority the criterion confers
 *   - - observant_liturgical_communities: Primary beneficiary (organized/identity_locked) — their practice constitutes the claimed vitality; receives textual inheritance and standing in return
 *   - - secular_hebrew_speakers: Primary target (moderate/constrained) — usage registered as profanation or irrelevance under the criterion
 *   - - hebrew_revival_activists: Target vanguard (organized/constrained) — revival project assigned zero weight in the liveness ledger
 *   - - modern_hebrew_literati: Secondary target (moderate/constrained) — literary productivity granted no standing in the vitality question
 *   - - women_in_traditional_communities: Excluded voice (powerless/trapped) — transmission labor counted as evidence, definitional voice absent
 *   - - academic_linguists: Analytical observer (analytical/analytical) — rival instruments place the scholarly verdict in standing tension with the criterion's verdict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.24).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Transmission Sufficiency Criterion for Living-Language Status").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious studies/nationalism studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '6c4bb2fa-fb71-4224-abcf-26c23bba999d').
narrative_ontology:cs_kernel_codification('6c4bb2fa-fb71-4224-abcf-26c23bba999d', distributed).
narrative_ontology:cs_authority_grounding('6c4bb2fa-fb71-4224-abcf-26c23bba999d', lineage).
narrative_ontology:cs_interpretation_layer_present('6c4bb2fa-fb71-4224-abcf-26c23bba999d').
narrative_ontology:cs_reading_relation('6c4bb2fa-fb71-4224-abcf-26c23bba999d', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('6c4bb2fa-fb71-4224-abcf-26c23bba999d', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('6c4bb2fa-fb71-4224-abcf-26c23bba999d', foundational, liturgical_use_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6c4bb2fa-fb71-4224-abcf-26c23bba999d', liturgical_use_constitutes_vitality, theological).
narrative_ontology:cs_axiom('6c4bb2fa-fb71-4224-abcf-26c23bba999d', secondary, custodial_chain_bears_language_life).
narrative_ontology:cs_axiom_status(custodial_chain_bears_language_life, holdable).
narrative_ontology:cs_axiom_grounding('6c4bb2fa-fb71-4224-abcf-26c23bba999d', custodial_chain_bears_language_life, conventional).
narrative_ontology:cs_reference_frame('6c4bb2fa-fb71-4224-abcf-26c23bba999d', unbroken_mesorah_transmission).
narrative_ontology:cs_drift_state('6c4bb2fa-fb71-4224-abcf-26c23bba999d', contemporary_post_vernacular_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c4bb2fa-fb71-4224-abcf-26c23bba999d', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, observant_liturgical_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, hebrew_revival_activists).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, modern_hebrew_literati).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_transmission_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, mesorah_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the recitation cycle, the study curriculum, and the standards of correct usage; trains and credentials interpreters through ordination chains; rules on which usages count as the language and which as error or profanation. Collects the deference and decision-rights that flow to recognized custodianship. Its linguistic office is bound to the claim that transmitted ritual use keeps the language alive, so abandoning that claim would dissolve the basis of its own standing.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Recite, chant, and study the fixed corpus on daily, weekly, and annual cycles across dispersed settlements, carrying the transmission labor that the continuity record rests on. In return they receive a shared textual inheritance, portable ritual competence, and the assurance that their practice constitutes the language's continued life rather than its memorial. Leaving the practice would mean leaving the community's self-understanding, not merely dropping a habit.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, observant_liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Use the language for journalism, fiction, scholarship, and everyday speech without ritual framing. Under the criterion their usage registers neither as vitality nor as custodianship, and polemical literature labels it profanation of a holy tongue. They cannot escape the classification by changing how they speak, since the criterion attaches to the language itself; the historical exit was abandoning the language entirely, the path taken by those who assimilated.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers, payer,
    moderate, biographical, constrained, continental).

% Built schools, newspapers, and households that made the language a mother tongue again. The criterion declares their project unnecessary — the language, it says, never died — and their methods profane; they bear the cost of contending against a definition that assigns their achievement zero weight in the vitality question. Exit would mean surrendering the point of the project.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, hebrew_revival_activists, payer,
    organized, generational, constrained, national).

% Produce new poetry, fiction, and criticism in the language. The criterion grants literary productivity no standing in the liveness determination, so their claim to carry the language forward is discounted regardless of output. Many worked bilingually in Russian, German, or Yiddish alongside Hebrew, but their commitment was to Hebrew letters, which is what the criterion devalues.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, modern_hebrew_literati, payer,
    moderate, biographical, constrained, continental).

% Carry home ritual, childhood recitation, and much of the intergenerational repetition on which the transmission record rests, while being barred from the advanced text-study and ordination tracks through which interpretive standing is granted. When transmission is credited, their labor is counted as evidence; when the definition is exercised, they are not in the room.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, women_in_traditional_communities, excluded,
    powerless, biographical, trapped, global).

% Classify languages by acquisition pattern, intergenerational transmission, and domains of use. On those instruments a language sustained solely by ritual recitation is recorded as classical or liturgical rather than living, which places the scholarly verdict in standing tension with the criterion's verdict and supplies the external standard against which the reading's claim is tested.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, academic_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains textual and ritual continuity of a fixed sacred corpus across territorially dispersed communities with no common vernacular or state: synchronized recitation cycles, standardized study curricula, and a shared ritual calendar coordinate millions of practitioners around identical texts across centuries.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to confer or deny living-language status from the broad speech community toward the liturgical custodianship; moves the daily labor of transmission onto practitioner communities; historically moved communal resources — schooling hours, endowments, printing oversight — toward text-study institutions.
% ABSENT_VOICES: Women reciters, whose transmission labor counts as evidence but who were barred from the interpretive apparatus the criterion empowers; secular speakers, present historically mostly as objects of the classification rather than as definers of it; Mizrahi and Sephardi pronunciation traditions, standardized toward Ashkenazi norms in several periods without consent of their speakers.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, the custodianship would lose its charter for linguistic authority, the revival-era polemics would lose their foil, and the classification of Hebrew — along with Aramaic, Ge'ez, Coptic, and ecclesiastical Latin — would shift to whichever rival criterion the disputants adopted; curricula, ordination rhetoric, and heritage politics would all rearrange around the new standard.
% FOUNDING_PROBLEM: After Hebrew ceased to be a daily vernacular (from roughly the third century CE onward), a stateless and territorially dispersed community needed its legal, liturgical, and textual continuity to survive without territorial or vernacular infrastructure; the criterion answered the question 'has the language died?' with a standard under which the transmitted corpus itself counted as the language's ongoing life.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Hebrew language and of post-Temple Jewry — outside the benefiting parties — corroborate the founding problem itself: the vernacular collapse and the functional load carried by liturgy and study are standard findings in the scholarly literature. On current status the corroboration splits: historians of diaspora communities attest the problem remains live where no vernacular took hold, while scholars of the revival and of contemporary Israeli sociolinguistics attest it was superseded by mother-tongue transmission after statehood. No single outside source attests the criterion's continuing necessity.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored low (0.24 at interval end) because the referent is the criterion's operation as this reading assesses it: coordination of dispersed communities around a fixed corpus, with the reading conceding only the delegitimization cost imposed on secular users. The series peaks at T120 (circa 1900), when the criterion was deployed hardest against the organized revival movement and its extractive edge — stripping the revivalists' achievement of standing — bit deepest, then declines as the vernacular revival succeeded and the criterion lost jurisdiction over most actual usage. Suppression_requirement is tracked because this story specifically traces enforcement-capacity change: communal discipline and publication control peaked circa 1820 (0.63) and decayed monotonically as emancipation dissolved communal autonomy and statehood removed jurisdiction, ending at 0.25. Theater_ratio rises across the interval (0.12 to 0.38): the transmission function remains genuinely performed inside practicing communities, but the criterion's classificatory activity — asserting liveness verdicts — became increasingly performative after the revival, asserted in sermons and curricula while no longer governing the language's actual sociolinguistic status. Accessibility_collapse is moderate (0.45): within the criterion's discursive jurisdiction alternatives collapse for adherents, but the rival criteria persisted as live positions, which is precisely why the sibling readings exist. Resistance is substantial (0.6): the maskilic press, the revival movement, and modern literary culture constitute a documented, organized revolt against the criterion's verdicts. All three metric series run on one shared time grid (T0, 40, 80, 120, 160, 200, 240) so every metric is authored at every examined point. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the rabbinical seat the criterion is a self-vindicating charter: it renders the custodianship's own practice constitutive of the language's life, so the arrangement presents as pure coordination the seat itself maintains. From the secular-speaker and revivalist seats the same structure operates as a standing demotion: their usage and labor are real, but the criterion assigns them zero weight in the vitality question while crediting the custodianship. The observant-community seat sits between: net beneficiary, yet bearing the daily labor of transmission that the criterion converts into evidence for the custodianship's claim. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority is declared beneficiary and agenda-setter with identity_locked exit: it derives a directionality near the beneficiary end (very low d), amplified toward capture by its institutional power. Observant liturgical communities are declared beneficiaries with identity_locked exit: low d, slightly above the custodianship because they bear transmission labor without setting the terms. Secular Hebrew speakers, revival activists, and modern literati are declared victims (payers) with constrained exit: high d, pushed toward the full-target end because the criterion attaches to the language itself, so no change of usage escapes its verdict. Women in traditional communities are authored as excluded rather than victims: their absence from the definitional conversation is commentary-grade signal, not a correction-grade structural input. Academic linguists sit at the analytical seat. No directionality overrides are used: the derivation from beneficiary/victim declarations plus exit atoms captures every seat's relationship, including the custodianship's dual position, which is handled by its secondary beneficiary role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining textual, legal, and ritual continuity without territory or vernacular — is contested rather than dead: for diaspora communities where no vernacular took hold, the transmission function remains live, so the arrangement is not an expired mandate kept alive by inertia, and mandatrophy_resolved is deliberately left unset. What has partially outlived its function is the criterion's classificatory jurisdiction: after the vernacular revival, its verdicts no longer determine the language's actual sociolinguistic status, and that residue accumulates as performance, visible in the rising theater_ratio. The classification prevents symmetric mislabelings: reading the arrangement as pure extraction would erase seventeen centuries of genuine diaspora coordination that no alternative mechanism provided; reading it as pure coordination would erase the interpretive-monopoly rent collected by the custodianship and the desecration stigma borne by secular users. The tangled-rope claim holds both halves simultaneously, and the low reading-indexed epsilon records how the arrangement looks from inside the reading rather than flattening that assessment into the type claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel living_language_status; would the native_generation_reading or the literary_continuity_reading of the same kernel assign a different victim set, a different epsilon, and a different computed type to the same historical arrangement?',
    'Generate the sibling stories over the identical historical interval and compare computed per-seat classifications; divergence locates the disagreement in the sufficiency condition (which practice constitutes vitality) rather than in the underlying facts.',
    'If the siblings compute materially different types for the same arrangement, the kernel contest is doing classificatory work and corpus-level aggregation must not average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings; siblings would re-sort beneficiaries, victims, and epsilon over the same referent.').

omega_variable(
    enforcement_decay_trajectory,
    'Is the decay of the criterion''s coercive enforcement (communal bans, publication control, disciplinary exclusion) irreversible, or does revival pressure within traditionalist movements periodically restore it?',
    'Track enforcement incidents and curricular-control intensity in traditionalist education networks across coming decades; compare against the 19th-century enforcement peak documented in the measurement series.',
    'A restored enforcement trajectory would raise suppression_requirement again and push payer-seat classifications back toward harder extraction; permanent decay fixes the current low-suppression profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether the suppression decay traced in the measurements is monotonic or cyclical.').

omega_variable(
    suppression_mechanism_mix,
    'Of the suppression the criterion currently exercises, how much is structural (curricular gatekeeping, credentialing control over who may define the language) versus internalized (practitioners experience the criterion as self-evident, so little force is needed)?',
    'Compare classification conformity inside enclaves with minimal visible enforcement against communities where the criterion arrived late; test whether adherents can articulate the rival criteria as coherent live options rather than errors.',
    'If mostly internalized, the measured suppression understates the criterion''s grip, since exit is cognitively closed even where institutionally open; if mostly structural, dismantling the credentialing institutions would release the classification quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Structural versus internalized share of the criterion''s current suppressive force.').

omega_variable(
    sufficiency_claim_scope,
    'Does the reading assert liturgical transmission is sufficient for liveness as a general sociolinguistic standard, or sufficient only within a tradition that already treats the corpus as normative for law and identity?',
    'Canonical analysis of the reading''s own formulations: determine whether the criterion is offered as a universal test applicable to any language (in which case counterexamples like ecclesiastical Latin, Coptic, and Ge''ez bear directly on it) or as an intra-traditional definition.',
    'As a general standard, the criterion competes openly with the sibling readings and must absorb the full weight of counterexamples; as intra-traditional, it withdraws from the general contest and its extraction footprint shrinks further below the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_claim_scope, conceptual, 'Scope of the sufficiency claim: universal standard or intra-traditional definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(livi_tr_t40, observed).
narrative_ontology:measurement(livi_tr_t80, living_language_status__liturgical_preservation_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement_basis(livi_tr_t80, observed).
narrative_ontology:measurement(livi_tr_t120, living_language_status__liturgical_preservation_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement_basis(livi_tr_t120, observed).
narrative_ontology:measurement(livi_tr_t160, living_language_status__liturgical_preservation_reading, theater_ratio, 160, 0.29).
narrative_ontology:measurement_basis(livi_tr_t160, observed).
narrative_ontology:measurement(livi_tr_t200, living_language_status__liturgical_preservation_reading, theater_ratio, 200, 0.34).
narrative_ontology:measurement_basis(livi_tr_t200, observed).
narrative_ontology:measurement(livi_tr_t240, living_language_status__liturgical_preservation_reading, theater_ratio, 240, 0.38).
narrative_ontology:measurement_basis(livi_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement_basis(livi_be_t40, observed).
narrative_ontology:measurement(livi_be_t80, living_language_status__liturgical_preservation_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement_basis(livi_be_t80, observed).
narrative_ontology:measurement(livi_be_t120, living_language_status__liturgical_preservation_reading, base_extractiveness, 120, 0.33).
narrative_ontology:measurement_basis(livi_be_t120, observed).
narrative_ontology:measurement(livi_be_t160, living_language_status__liturgical_preservation_reading, base_extractiveness, 160, 0.31).
narrative_ontology:measurement_basis(livi_be_t160, observed).
narrative_ontology:measurement(livi_be_t200, living_language_status__liturgical_preservation_reading, base_extractiveness, 200, 0.27).
narrative_ontology:measurement_basis(livi_be_t200, observed).
narrative_ontology:measurement(livi_be_t240, living_language_status__liturgical_preservation_reading, base_extractiveness, 240, 0.24).
narrative_ontology:measurement_basis(livi_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(livi_su_t40, observed).
narrative_ontology:measurement(livi_su_t80, living_language_status__liturgical_preservation_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement_basis(livi_su_t80, observed).
narrative_ontology:measurement(livi_su_t120, living_language_status__liturgical_preservation_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement_basis(livi_su_t120, observed).
narrative_ontology:measurement(livi_su_t160, living_language_status__liturgical_preservation_reading, suppression_requirement, 160, 0.38).
narrative_ontology:measurement_basis(livi_su_t160, observed).
narrative_ontology:measurement(livi_su_t200, living_language_status__liturgical_preservation_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement_basis(livi_su_t200, observed).
narrative_ontology:measurement(livi_su_t240, living_language_status__liturgical_preservation_reading, suppression_requirement, 240, 0.25).
narrative_ontology:measurement_basis(livi_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'is Hebrew (or any sacred-text language) alive?' decomposes per the epsilon-invariance principle into three structurally distinct criteria, each with its own epsilon, beneficiary/victim structure, and classification. This story is the liturgical_preservation_reading (upstream historically: the oldest claim, against which the others defined themselves). The native_generation_reading is its direct logical negation on the paradigm case; the literary_continuity_reading is an independent sufficiency claim that coexists with it. Family members are linked via affects_constraints so contamination and drift propagate across the kernel rather than being absorbed silently within one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
