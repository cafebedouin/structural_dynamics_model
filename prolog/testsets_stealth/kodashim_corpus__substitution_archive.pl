% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Substitution Archive Reading of the Kodashim Corpus
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the rabbinic movement
 *   designated verbal prayer and Torah study as the operative continuation of
 *   the sacrificial order, and preserved the Kodashim corpus (the Mishnaic
 *   and talmudic orders of sacrificial law) as authoritative record of what
 *   had been superseded. This story instantiates the substitution_archive
 *   reading of that arrangement: the corpus is a memorial archive documenting
 *   a replaced practice, not an occupied kernel, and the arrangement claims
 *   continuity with what it replaced while denying restoration. The
 *   extraction the reading registers lives in that continuity claim:
 *   aspirants toward living sacrificial practice are told the practice they
 *   seek has been honorably retired, while the curriculum centrality,
 *   patronage, and interpretive authority the corpus carries flow to the
 *   text-study institutions that administer the substitution. Time units are
 *   approximately two decades each: t0 corresponds to the post-destruction
 *   consolidation era (~70-100 CE), t100 to the present. KEY AGENTS (by
 *   structural relationship): - rabbinic_text_study_institutions:
 *   Agenda-setter and primary beneficiary (institutional/generational,
 *   identity_locked) — administers the substitution and collects the
 *   authority it generates - kodashim_torah_scholars: Secondary beneficiary
 *   (moderate/biographical, identity_locked) — careers constituted by mastery
 *   of the archived corpus - living_sacrifice_practice_seekers: Primary
 *   target (powerless/biographical, trapped) — bears the denial of the
 *   practice they seek - daily_liturgy_reciters: Near-symmetric dual seat
 *   (organized/biographical, constrained) — receives the devotional
 *   structure, supplies the memorial labor - messianic_restorationists:
 *   Excluded seat (powerless/generational, trapped) — would restore the
 *   practice; kept outside the conversation that fixes the corpus's meaning -
 *   comparative_ritual_scholars: Analytical observer
 *   (analytical/civilizational, analytical) — sees the full structure from
 *   outside the authority system
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: agenda-setter and primary beneficiary (institutional/generational, identity_locked) — sets curriculum and liturgy, collects standing from the corpus's centrality
 *   - kodashim_torah_scholars: secondary beneficiary (moderate/biographical, identity_locked) — professional and religious identity fused with sacrificial-law expertise
 *   - living_sacrifice_practice_seekers: primary target (powerless/biographical, trapped) — aspiration redirected into archival channels, no exit that preserves belonging
 *   - daily_liturgy_reciters: dual beneficiary/payer (organized/biographical, constrained) — receives ready-made devotional continuity, supplies daily memorial recitation
 *   - messianic_restorationists: excluded (powerless/generational, trapped) — restoration project classified as premature; no seat in the interpretive conversation
 *   - comparative_ritual_scholars: analytical observer (analytical/civilizational, analytical) — documents the substitution from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.52).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Substitution Archive Reading of the Kodashim Corpus").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'c5e40582-4834-4c9c-96de-f3079ff013ba').
narrative_ontology:cs_kernel_codification('c5e40582-4834-4c9c-96de-f3079ff013ba', fixed_text).
narrative_ontology:cs_authority_grounding('c5e40582-4834-4c9c-96de-f3079ff013ba', lineage).
narrative_ontology:cs_interpretation_layer_present('c5e40582-4834-4c9c-96de-f3079ff013ba').
narrative_ontology:cs_reading_relation('c5e40582-4834-4c9c-96de-f3079ff013ba', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('c5e40582-4834-4c9c-96de-f3079ff013ba', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_axiom('c5e40582-4834-4c9c-96de-f3079ff013ba', foundational, verbal_service_fully_replaces_animal_service).
narrative_ontology:cs_axiom_status(verbal_service_fully_replaces_animal_service, holdable).
narrative_ontology:cs_axiom_grounding('c5e40582-4834-4c9c-96de-f3079ff013ba', verbal_service_fully_replaces_animal_service, theological).
narrative_ontology:cs_axiom('c5e40582-4834-4c9c-96de-f3079ff013ba', foundational, kodashim_memorial_record_not_blueprint).
narrative_ontology:cs_axiom_status(kodashim_memorial_record_not_blueprint, holdable).
narrative_ontology:cs_axiom_grounding('c5e40582-4834-4c9c-96de-f3079ff013ba', kodashim_memorial_record_not_blueprint, conventional).
narrative_ontology:cs_reference_frame('c5e40582-4834-4c9c-96de-f3079ff013ba', completed_substitution_regime).
narrative_ontology:cs_drift_state('c5e40582-4834-4c9c-96de-f3079ff013ba', contemporary_restorationist_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('c5e40582-4834-4c9c-96de-f3079ff013ba', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, living_sacrifice_practice_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, kodashim_torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, daily_liturgy_reciters).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, daily_liturgy_reciters).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, verbal_service_substitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the academies, yeshivot, and liturgical authorities that administer post-temple worship. They designate prayer and Torah study as the operative continuation of the sacrificial order, fix the curriculum in which the Kodashim corpus is taught as authoritative record, and train the teachers who transmit that framing. Enrollment, communal funding, and interpretive standing flow to them for as long as the corpus remains central. Dismantling that role would mean dissolving an institutional identity built over centuries around the corpus's centrality.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% Individual scholars whose expertise in sacrificial law constitutes their professional and religious standing. Livelihood, marriage prospects, and communal rank attach to mastery of tractates whose content has no practical application outside the study hall. Retraining toward other fields would forfeit decades of accumulated distinction and, in their own self-understanding, abandon the portion of the tradition entrusted to them.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, kodashim_torah_scholars, beneficiary,
    moderate, biographical, identity_locked, regional).

% Worshippers who want to carry out the sacrificial commandments in lived practice rather than encounter them as text. No altar exists, halakhic rules bar offering outside the historic sanctuary, and the governing teaching tells them the practice they seek has been honorably concluded. Their aspiration has nowhere to go inside the system; leaving the community would cost them belonging, family, and shared life, so they remain and absorb the denial.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, living_sacrifice_practice_seekers, payer,
    powerless, biographical, trapped, global).

% Ordinary congregants who recite the sacrificial descriptions embedded in the daily liturgy. They receive a ready-made devotional structure connecting them to the historic cult without personal cost or decision. They also supply the daily memorial labor — the recitation itself — that keeps the substituted order visibly continuous with what it replaced, rehearsing a practice they will never perform.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, daily_liturgy_reciters, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, daily_liturgy_reciters, payer).

% Groups, concentrated around the historic temple site, who prepare for and advocate renewed sacrificial practice. The governing institutions classify their project as premature and theologically mistaken, and they hold no seat in the curricular or liturgical conversations that determine what the corpus means. The site itself sits under another polity's administration, and their standing inside the wider community depends on not pressing the point.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restorationists, excluded,
    powerless, generational, trapped, regional).

% Academic historians and ritual theorists who study the post-destruction substitution from outside the tradition's authority structure. They take testimony from every party, publish analyses that no seat controls, and hold no stake in whether the corpus is read as record, rehearsal, or blueprint.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the temple's destruction, the community faced the practical problem of sustaining a covenantal practice built around sacrifice with no altar, no priesthood in office, and no control of the site. The substitution arrangement solved it once, centrally: verbal prayer stands in for the daily offerings, Torah study stands in for the sacrificial rite, and the Kodashim corpus preserves the operative details as authoritative record so the legal system remains complete.
% TRANSFER_FUNCTION: Moves devotional obligation and interpretive authority from the altar and priesthood to the synagogue and study hall. Concretely, it transfers curriculum centrality, communal patronage, and the prestige attached to sacrificial expertise to the text-study institutions, and redirects the practice-seeker's aspiration into channels — recitation, study, memorial — that those institutions administer.
% ABSENT_VOICES: Messianic restorationists are outside the room: the institutions classify their project as premature, and no restorationist seat sits on the bodies that fix liturgy or curriculum. The hereditary priesthood, whose office the substitution absorbed, survives mainly as genealogical honorific. Practitioners drawn to embodied ritual raise the objection that text-only continuity is thin, but they lack a forum inside the halakhic process where that objection could alter the settlement.
% DISAPPEARANCE_RATIONALE: If the substitution arrangement vanished overnight — if prayer and study were no longer counted as continuations and the corpus lost its archival standing — the daily liturgy would lose its sacrificial core, the academy curriculum would lose its most technically demanding tractates, and the institutions' claim to covenantal authority would lose the continuity argument that underwrites it. Worship, education, and institutional standing would all reorganize around whatever replaced the continuity claim.
% FOUNDING_PROBLEM: Preserve covenantal practice and legal continuity after the temple's destruction: how a community whose entire sacrificial code presupposed a functioning altar continues to observe, teach, and transmit that code with no altar, no officiating priesthood, and no jurisdiction over the site.
% FOUNDING_PROBLEM_CORROBORATION: Attestation from outside the benefiting parties: first-century witnesses independent of the rabbinic academies (Josephus on the cessation of the cult), the Samaritan and Karaite communities, which maintained distinct relationships to sacrificial memory without rabbinic patronage, and academic historiography of post-70 Judaism, which documents the substitution as a response to catastrophe rather than its fulfillment. The institutions themselves attest the problem was solved by substitution; the restorationist minority attests it remains open; the corroborating sources confirm the problem's historical reality while leaving its resolution disputed.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.58) is authored for the standing arrangement under contest — the substitution regime in which prayer and study count as continuation and the corpus counts as archive — assessed by this reading's own lights, never for any endorsed alternative. It is moderate rather than high because the arrangement solved a real post-catastrophe problem and imposes no material levy; it is well above negligible because the continuity claim converts one class of adherents' devotional aspiration into institutional standing they do not share in. Suppression (0.52) is a raw structural property, unscaled by power or scope: the arrangement does not imprison anyone, but the teaching that the sought practice is obsolete, combined with halakhic bars on offering outside the sanctuary and the social cost of exit, leaves seekers without a live alternative inside the community. Theater (0.30) reflects the growing share of corpus engagement that is memorial performance — daily recitation of sacrificial descriptions, seder-plate memorials — against the genuinely functional legal-preservation and educational core. Accessibility_collapse (0.45) is deliberately below natural-law levels: the alternative reading (the corpus as blueprint awaiting resumption) remains thinkable — its existence as a sibling story is the evidence. Resistance (0.40) records the restorationist currents and embodied-ritual critics the arrangement must continually answer. Claimed type (tangled_rope) is stated from structure — genuine coordination function, identifiable beneficiaries and victims, active enforcement through curricular and liturgical gatekeeping — independently of the metric values; the engine computes per-seat classifications from the structural data. The measurement series run on one shared eleven-point grid (every tracked metric authored at every point). The extractiveness and theater series show a sawtooth around a rising trend: each restorationist surge (Bar Kokhba-era nostalgia, the Sabbatean episode, the modern Temple-mount movement) raises the salience of the denied practice, prompting institutional hardening of the obsolescence claim (peaks), followed by relaxation (troughs). The oscillation is partly an extraction mechanism in itself — intermittent re-denial renews the institutions' authority as guardians of the settlement — not merely noise. Suppression_requirement is tracked because enforcement capacity genuinely changed over the interval: intensive post-destruction consolidation of the substitution against rival readings decayed into low-cost normalization, a falling trajectory ending at the scalar 0.52.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat should compute differently. From the institutions' position the arrangement is faithful transmission: they preserved a legal system intact through catastrophe and the archive reading is simply what the corpus is. From the seeker's position the same arrangement operates as a standing denial — their object of devotion is classified as finished, and the institutions that classify it collect the authority the classification generates. Daily reciters sit near the middle: they receive a devotional structure they did not have to build and pay for it with memorial labor rehearsing a practice they will never perform. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: rabbinic_text_study_institutions and kodashim_torah_scholars sit near the beneficiary end (low d, subsidized or damped effective extraction); living_sacrifice_practice_seekers sit near the full-target end (high d, amplified). Daily_liturgy_reciters carry dual roles and derive near-symmetric. One directionality override is authored: power_atom powerless mapped to d=0.88. Rationale: the story contains two powerless seats — the declared-victim seekers and the excluded restorationists. The seekers derive a high target-value from the victim declaration plus trapped exit, but the restorationists hold the excluded role with no beneficiary/victim declaration, risking a neutral canonical fallback despite bearing the arrangement's sharpest denial (their project is classified as premature by the very authorities whose legitimacy rests on that classification). Because the override binds by power atom, it covers both powerless seats; this is accurate rather than distorting, since both are genuine targets — the restorationists additionally face spatial concentration (site under another polity's administration) that deepens rather than lightens their targeting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a community whose sacrificial code presupposed a functioning altar continues to observe and transmit that code — remains materially live: the condition that generated it (no altar, no priestly office, no jurisdiction over the site) persists, so no mandatrophy resolution is declared and the arrangement is not running on an expired mandate. The classification prevents two opposite mislabelings. Reading the arrangement as pure coordination ignores the asymmetric transfer: the seeker's aspiration is converted into institutional standing, and the gains concentrate in a seat that also sets the rules. Reading it as pure extraction ignores the genuine post-catastrophe achievement — a dispersed community kept legal and liturgical continuity it otherwise would have lost — and overstates the coercion, since exit from the community is possible though costly and no seat is physically prevented from leaving. The tangled_rope classification holds both facts: real coordination function, real asymmetric extraction, held together by active curricular and liturgical enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (substitution_archive) of the kodashim_corpus kernel. What changes structurally if a sibling reading is instantiated instead — and where exactly does the disagreement sit?',
    'Read against the sibling stories kodashim_corpus__study_as_exercise and kodashim_corpus__performance_only: each assigns its own epsilon, beneficiary set, and victim set to the same corpus. The disagreement is located in the kernel''s status predicate (occupied vs. archived vs. dormant) and in the terminality of the substitution.',
    'Under study_as_exercise the corpus is occupied through engagement and extraction shifts toward whoever defines the exercise; under performance_only the corpus is a blueprint and the victim set becomes everyone invested in the substitution''s permanence. The authored epsilon of 0.58 is valid only for the archive reading and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading-indexed classification: one kernel, three structurally distinct constraints; this file authors only the archive reading.').

omega_variable(
    restoration_denial_vs_daily_petition,
    'This reading denies restoration, yet the canonical liturgy it administers petitions daily for the offerings'' renewal. Is the denial a stable feature of the reading, or does the embedded petition eventually force revision toward the performance_only sibling?',
    'Track liturgical and curricular reform debates over time: if restoration petitions are progressively reinterpreted as pure rhetoric or attenuated, the denial holds; if they regain literal operative force in halakhic argument, the archive reading loses coherence and drifts.',
    'If the petition regains literal force, this reading collapses toward performance_only, extraction redistributes toward whichever seat manages the restoration question, and the constraint family re-aligns around a dormant-kernel structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_denial_vs_daily_petition, empirical, 'Internal tension between the reading''s denial of restoration and the daily liturgical petition for restored offerings.').

omega_variable(
    seeker_harm_artifact,
    'Are living_sacrifice_practice_seekers genuinely harmed by the archive framing, or is the harm an artifact of the analytical frame — do most practitioners experience the substitution as completion rather than loss?',
    'Ethnography and attitude study distinguishing practitioners with concrete restorative intent from practitioners for whom substitution is subjectively complete; measure whether the ''told it is obsolete'' injury is felt or inferred.',
    'If the harm is largely artifactual, effective extraction falls well below the authored 0.58 and the arrangement trends toward a pure coordination reading; if concrete and widespread, the scalar understates the denial''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seeker_harm_artifact, preference, 'Whether the victim class is constituted by the arrangement itself or by the analytical lens applied to it.').

omega_variable(
    archive_or_preparation_pedagogy,
    'Does Kodashim pedagogy function as memorial record, or does it quietly retain conditional-practical framing (''when the temple is rebuilt'') that prepares for resumed practice?',
    'Curriculum and textbook analysis: code instructional framing as retrospective-memorial versus conditional-practical across institutions and generations.',
    'A preparatory function moves this reading toward study_as_exercise and raises the corpus''s operational stakes; a purely memorial function stabilizes the archive reading and its current epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_or_preparation_pedagogy, empirical, 'Functional status of the archive: record of the superseded versus contingency preparation for the restored.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_substitution_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t0, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t10, kodashim_corpus__substitution_archive, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t10, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t20, kodashim_corpus__substitution_archive, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t20, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t30, kodashim_corpus__substitution_archive, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t30, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t40, kodashim_corpus__substitution_archive, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t40, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t50, kodashim_corpus__substitution_archive, theater_ratio, 50, 0.23).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t50, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t60, kodashim_corpus__substitution_archive, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t60, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t70, kodashim_corpus__substitution_archive, theater_ratio, 70, 0.26).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t70, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t80, kodashim_corpus__substitution_archive, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t80, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t90, kodashim_corpus__substitution_archive, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t90, observed).
narrative_ontology:measurement(kodashim_substitution_tr_t100, kodashim_corpus__substitution_archive, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(kodashim_substitution_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(kodashim_substitution_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(kodashim_substitution_be_t0, observed).
narrative_ontology:measurement(kodashim_substitution_be_t10, kodashim_corpus__substitution_archive, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(kodashim_substitution_be_t10, observed).
narrative_ontology:measurement(kodashim_substitution_be_t20, kodashim_corpus__substitution_archive, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(kodashim_substitution_be_t20, observed).
narrative_ontology:measurement(kodashim_substitution_be_t30, kodashim_corpus__substitution_archive, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(kodashim_substitution_be_t30, observed).
narrative_ontology:measurement(kodashim_substitution_be_t40, kodashim_corpus__substitution_archive, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(kodashim_substitution_be_t40, observed).
narrative_ontology:measurement(kodashim_substitution_be_t50, kodashim_corpus__substitution_archive, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(kodashim_substitution_be_t50, observed).
narrative_ontology:measurement(kodashim_substitution_be_t60, kodashim_corpus__substitution_archive, base_extractiveness, 60, 0.49).
narrative_ontology:measurement_basis(kodashim_substitution_be_t60, observed).
narrative_ontology:measurement(kodashim_substitution_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.54).
narrative_ontology:measurement_basis(kodashim_substitution_be_t70, observed).
narrative_ontology:measurement(kodashim_substitution_be_t80, kodashim_corpus__substitution_archive, base_extractiveness, 80, 0.51).
narrative_ontology:measurement_basis(kodashim_substitution_be_t80, observed).
narrative_ontology:measurement(kodashim_substitution_be_t90, kodashim_corpus__substitution_archive, base_extractiveness, 90, 0.56).
narrative_ontology:measurement_basis(kodashim_substitution_be_t90, observed).
narrative_ontology:measurement(kodashim_substitution_be_t100, kodashim_corpus__substitution_archive, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(kodashim_substitution_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_substitution_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(kodashim_substitution_su_t0, observed).
narrative_ontology:measurement(kodashim_substitution_su_t10, kodashim_corpus__substitution_archive, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(kodashim_substitution_su_t10, observed).
narrative_ontology:measurement(kodashim_substitution_su_t20, kodashim_corpus__substitution_archive, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(kodashim_substitution_su_t20, observed).
narrative_ontology:measurement(kodashim_substitution_su_t30, kodashim_corpus__substitution_archive, suppression_requirement, 30, 0.67).
narrative_ontology:measurement_basis(kodashim_substitution_su_t30, observed).
narrative_ontology:measurement(kodashim_substitution_su_t40, kodashim_corpus__substitution_archive, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(kodashim_substitution_su_t40, observed).
narrative_ontology:measurement(kodashim_substitution_su_t50, kodashim_corpus__substitution_archive, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(kodashim_substitution_su_t50, observed).
narrative_ontology:measurement(kodashim_substitution_su_t60, kodashim_corpus__substitution_archive, suppression_requirement, 60, 0.59).
narrative_ontology:measurement_basis(kodashim_substitution_su_t60, observed).
narrative_ontology:measurement(kodashim_substitution_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.58).
narrative_ontology:measurement_basis(kodashim_substitution_su_t70, observed).
narrative_ontology:measurement(kodashim_substitution_su_t80, kodashim_corpus__substitution_archive, suppression_requirement, 80, 0.56).
narrative_ontology:measurement_basis(kodashim_substitution_su_t80, observed).
narrative_ontology:measurement(kodashim_substitution_su_t90, kodashim_corpus__substitution_archive, suppression_requirement, 90, 0.54).
narrative_ontology:measurement_basis(kodashim_substitution_su_t90, observed).
narrative_ontology:measurement(kodashim_substitution_su_t100, kodashim_corpus__substitution_archive, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(kodashim_substitution_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial question 'what happened to sacrifice in rabbinic Judaism' conflates three structurally distinct claims about one kernel (kodashim_corpus). The substitution_archive reading (this file) authors epsilon for the standing substitution arrangement and finds moderate extraction in its continuity claim. The study_as_exercise sibling authors the corpus as occupied-through-engagement; the performance_only sibling authors it as dormant blueprint. The upstream reading (substitution_archive) has the highest empirical entrenchment — it is the operative institutional settlement — and therefore exerts structural pressure on both siblings: curricular and liturgical gatekeeping shapes what resources the other readings can claim. All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
