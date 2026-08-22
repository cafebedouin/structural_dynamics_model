% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Liturgical Preservation Criterion for Language Vitality (Rabbinical Custodial Reading)
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'what makes a
 *   language living': the liturgical_preservation_reading held by the
 *   traditional rabbinical establishment — a language is living if its sacred
 *   texts are continuously recited, studied, and used in ritual; preservation
 *   through liturgical transmission suffices. The standing arrangement under
 *   contest is the custodial regime this criterion grounds: the rabbinate
 *   defines correct usage, adjudicates which uses count as the language's
 *   life, and polices the sacred/profane boundary; the faithful community
 *   receives corpus continuity; the secular speech community's daily
 *   vernacular use is reclassified as non-life-giving and, at the boundary,
 *   as desecration. The epsilon referent is this standing custodial
 *   arrangement as the reading itself sees it — stewardship and preservation,
 *   with a delegitimation cost the reading acknowledges and judges justified
 *   — hence the low authored epsilon (0.28, matching the manifest bin). The
 *   sibling readings are separate constraints with different epsilon over the
 *   same referent: the native_generation_reading authors high epsilon for
 *   this arrangement (embalmment plus suppression of the language's actual
 *   life), and the literary_continuity_reading authors intermediate epsilon
 *   (real preservation, starved productivity). The disagreement is located in
 *   the criterion itself and is routed to omega variables rather than
 *   averaged into this story's epsilon. KEY AGENTS (by structural
 *   relationship): rabbinical_authority — agenda-setter and primary
 *   beneficiary (institutional/identity_locked);
 *   faithful_liturgical_community — coordinating beneficiary
 *   (organized/constrained); secular_speech_community — primary target
 *   (moderate/constrained); maskilic_literati — organized target with
 *   contesting capacity (organized/mobile); women_of_the_liturgical_community
 *   — excluded seat (powerless/constrained); sociolinguistic_scholarship —
 *   analytical observer (analytical/analytical).
 *
 * KEY AGENTS:
 *   - rabbinical_authority: agenda-setter and primary beneficiary — administers the criterion, collects the interpretive monopoly
 *   - faithful_liturgical_community: beneficiary — recites, studies, transmits; collects corpus continuity and ritual unity
 *   - secular_speech_community: primary target — bears delegitimation of its daily usage
 *   - maskilic_literati: organized target — contests the criterion through new literary production; bears the sharpest enforcement
 *   - women_of_the_liturgical_community: excluded — outside the transmission sites the criterion counts
 *   - sociolinguistic_scholarship: analytical observer — adjudicates the vitality criteria comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.28).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.38).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Preservation Criterion for Language Vitality (Rabbinical Custodial Reading)").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '767b201c-56cd-40aa-9f52-917e77f0ef13').
narrative_ontology:cs_kernel_codification('767b201c-56cd-40aa-9f52-917e77f0ef13', fixed_text).
narrative_ontology:cs_authority_grounding('767b201c-56cd-40aa-9f52-917e77f0ef13', lineage).
narrative_ontology:cs_interpretation_layer_present('767b201c-56cd-40aa-9f52-917e77f0ef13').
narrative_ontology:cs_reading_relation('767b201c-56cd-40aa-9f52-917e77f0ef13', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('767b201c-56cd-40aa-9f52-917e77f0ef13', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('767b201c-56cd-40aa-9f52-917e77f0ef13', foundational, liturgical_transmission_confers_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_confers_vitality, holdable).
narrative_ontology:cs_axiom_grounding('767b201c-56cd-40aa-9f52-917e77f0ef13', liturgical_transmission_confers_vitality, theological).
narrative_ontology:cs_axiom('767b201c-56cd-40aa-9f52-917e77f0ef13', secondary, secular_usage_is_profanation).
narrative_ontology:cs_axiom_status(secular_usage_is_profanation, holdable).
narrative_ontology:cs_axiom_grounding('767b201c-56cd-40aa-9f52-917e77f0ef13', secular_usage_is_profanation, theological).
narrative_ontology:cs_reference_frame('767b201c-56cd-40aa-9f52-917e77f0ef13', holy_tongue_liturgical_vitality).
narrative_ontology:cs_drift_state('767b201c-56cd-40aa-9f52-917e77f0ef13', post_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('767b201c-56cd-40aa-9f52-917e77f0ef13', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, faithful_liturgical_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, maskilic_literati).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, holy_tongue_sanctity_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, rabbinical_custodianship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the criterion by which the language counts as alive: rules on correct usage, adjudicates which uses count as the language's life, and disciplines the boundary between sacred and profane use through communal sanction. Grounds its standing in an unbroken chain of transmission from the canonical corpus. Every ruling on usage reinforces its position as the language's sole legitimate custodian; the custodial role is not a position the institution occupies but what the institution is, and relinquishing the criterion would dissolve its standing — which is why exit has historically been taken only through schism.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Recites, studies, and transmits the sacred corpus in daily prayer and ritual. Receives textual continuity, ritual unity, and a shared sacred register spanning geographies and generations. Also submits to the custodians' discipline over its own vernacular impulses. Leaving would mean leaving the practice and the community, not merely the criterion.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, faithful_liturgical_community, beneficiary,
    organized, generational, constrained, global).

% Uses the language daily in vernacular life — home, market, work, and later press, stage, and school. Under the custodial criterion this usage does not count as the language's life, and at the boundary it is named desecration. Its innovations are classed as corruption; its speakers are not recognized as the language's maintainers. Exit has meant abandoning the language entirely or building institutions outside custodial control.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, continental).

% The organized literary wing of the secular claim: produces new poetry, periodicals, and scholarship in the language and argues that this productivity is the language's life. Bears the sharpest sanctions — publication bans, excommunication of its radicals, the desecrator label. Its print networks, patrons, and geographic mobility let it contest the criterion from outside the study house in ways the diffuse vernacular community cannot.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, maskilic_literati, payer,
    organized, biographical, mobile, continental).

% Excluded from the study house and the text-transmission sites where the criterion locates the language's life; their prayer literacy and domestic religious usage were not counted by those who adjudicated vitality. Their objection — that the living language was alive in rooms they could not enter, and that the counting community was selective long before it ruled on secular usage — has held no seat in the adjudication.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, women_of_the_liturgical_community, excluded,
    powerless, generational, constrained, continental).

% Studies language vitality comparatively — diglossia, intergenerational transmission, reversed language shift, international vitality frameworks. Its findings: liturgical transmission demonstrably preserves corpora and registers across generations; it does not by itself produce native vernacular vitality. Collects nothing and pays nothing; its verdicts shift the terms of the contest.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguistic_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem for a stateless, dispersed community: how to maintain continuity with a fixed sacred corpus — shared texts, shared ritual, mutual intelligibility of the sacred register across geographies and generations — without territorial concentration or native transmission.
% TRANSFER_FUNCTION: Moves linguistic authority and standing from the secular speech community to the rabbinical custodian: the criterion transfers the power to say what the language is, and with it the legitimacy of usage, from daily speakers to the administrators of recitation and study.
% ABSENT_VOICES: The secular speech community sat outside the adjudication for most of the interval; its objection (daily use is the life of a language) was answered with the desecration label rather than engaged. Women of the liturgical community were excluded from the transmission sites themselves. Both would contest the criterion's unanimity: the consensus that liturgical transmission suffices was reached in rooms only custodians and the faithful entered. Commentary-grade only — this authored absence must not drive classification overrides.
% DISAPPEARANCE_RATIONALE: The practice would persist overnight — prayer, study, and ritual do not depend on the criterion; they predate it and would outlive it. What rearranges is the authority structure: the custodian's definitional monopoly over the language would lapse, the secular community's delegitimation would lift, and the vitality question would be re-adjudicated under the sibling criteria. The faithful communities attest the rearrangement would be a loss of sanctity and continuity; the secular and scholarly seats attest only the monopoly would fall while the practice continues. The parties dispute which description is true — hence contested rather than world_rearranges or world_unchanged.
% FOUNDING_PROBLEM: A diaspora community without native transmission or territorial concentration needed its sacred language to remain continuous with its textual past; the criterion was built to establish that continuity-without-native-speakers is still life — defending both the language and its custodians against the claim that a language nobody speaks natively is a dead one.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinate attests the problem live, but it is a benefiting party. Outside that set: comparative sociolinguistics (diglossia and reversed-language-shift research; international vitality assessments) corroborates both the problem's historical reality — liturgical transmission did preserve the corpus and register across two millennia without native speakers — and its partial obsolescence where native transmission emerged. Maskilic and revivalist testimony (periodicals, memoirs, the revival generation's own account of acquiring the language) corroborates from the contest's other side. No source outside the dispute attests the problem remains fully live in its original form; the enclave communities' persistence is the strongest surviving evidence for liveness.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored low (0.28) per the reading's own lights and the manifest bin: the reading sees stewardship rather than extraction, and what extraction exists is non-material — standing and legitimacy rather than wealth or labor. Suppression (0.38) is real where the arrangement holds: historical publication bans, excommunication of radicals, and present enclave discipline; it is absent where the arrangement lost reach. Theater (0.30) rises across the interval because the vitality-adjudication grows performative as vernacular life departs the frame — the liturgy itself remains functional, but the claim that the liturgy IS the language's life is increasingly maintained against the evidence of native speech elsewhere. Accessibility_collapse (0.30) is low: understanding the criterion does not collapse the sibling criteria — they remain live positions held by other parties, and the constraint competes socially even where it forecloses logically. Resistance (0.60) is high: the Haskalah and the revival were organized, multi-generational resistance that ultimately produced a rival vitality regime. Suppression here is both structural (communal sanction, publication bans) and partly internalized (secular speakers' own deference before the holy tongue — the maskilim's ambivalence is the historical trace); the omega internalized_delegitimization carries that ambiguity. The measurement series share one grid (1780/1830/1880/1920/1970/2020): the extractiveness hump tracks the contest's intensity (rising as the secular claim grows, falling as the secular community exits the frame), the suppression series tracks the enforcement arc (rise, peak at the counter-Haskalah bans, partial decay into enclave discipline), and theater rises monotonically.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is stewardship: the criterion protects a sacred inheritance and its costs are justified boundary-keeping. From the payer seats the same structure is a self-serving monopoly: the definition of vitality is drawn exactly where the custodian's authority ends, and the secular community's life with the language is ruled out of count. The engine computes this divergence from the structural data — the rabbinate's beneficiary declaration and identity-locked exit place it near the subsidy end of directionality, the secular community's victim declaration and constrained exit place it near the full-target end — and the divergence is the measurement, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinate is declared beneficiary and holds the agenda-setter role: the criterion is the instrument of its interpretive monopoly, so its directionality sits near the beneficiary end and effective extraction inverts toward subsidy. The faithful liturgical community is a genuine beneficiary — corpus continuity, ritual unity, a shared sacred register — though it also bears custodial discipline, which keeps its directionality slightly above the rabbinate's. The secular speech community and the maskilic literati are declared victims: the criterion takes their standing as the language's maintainers and pays it to the custodian; the literati's print mobility damps their effective extraction relative to the diffuse community's constrained exit — a same-position, different-exit differentiation at roughly comparable social level. Women of the community hold an excluded seat — outside both benefit and conversation — and are commentary-grade, not a directionality input. Identity-lock on the rabbinate is institutional: the custodial role is not a position it occupies but what it is; adopting the native-generation criterion would dissolve its standing, which is why exit was historically taken only at the price of schism (religious Zionism as the partial-exit case). Coalition note: the powerless excluded seat could in principle align with the payer seats; historically the coalition that formed was literati plus secular community plus revivalists, and it won — the enforcement decay series is the trace of that coalition's success.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. As pure rope, the delegitimation of the secular speech community would vanish into 'preservation' — but the criterion was drawn by the party it entrenches, and its boundary (secular usage does not count) is exactly the custodian's monopoly edge. As pure snare, the corpus continuity would vanish into 'cover story' — but the coordination was real: the sacred corpus demonstrably survived two millennia of stateless diaspora through this structure, and the community that recites it genuinely collects that good. Mandatrophy: the founding problem (continuity without native transmission) is contested — live for the enclave communities that still have no other transmission path, dead for the revived vernacular that no longer needs the criterion. The arrangement persists past the general resolution of its problem, visible in the rising theater series; the inertial-drift risk is confined to the diaspora frame, where maintenance is increasingly performance around a settled question. The R5 mismatch consumer reads status=contested against verdict=contested, so no zombie flag fires — the theater trajectory is the drift signal to watch instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (liturgical_preservation_reading) of the kernel living_language_status; the sibling readings (native_generation_reading, literary_continuity_reading) instantiate different criteria — what structurally changes if a community adopts a sibling, and where exactly is the disagreement located?',
    'Cross-reading comparison of the three stories'' beneficiary/victim sets and epsilon values. The disagreement is located in the vitality criterion itself — whether life is conferred by liturgical use, native generational transmission, or literary productivity — and each criterion redraws the custodian/claimant boundary differently.',
    'Under the native_generation reading this same custodial arrangement computes high extraction (embalmment plus suppression of the language''s actual life); under the literary_continuity reading, intermediate. The low epsilon authored here is reading-indexed to the liturgical reading''s own lights, not topic-invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of the living-language kernel; sibling readings change the victim set and the effective extraction over the same standing arrangement.').

omega_variable(
    custodianship_vs_monopoly,
    'Is the rabbinate''s interpretive authority over the sacred language stewardship the community freely values, or an interpretive monopoly maintained against rival claimants?',
    'Compare vitality outcomes in communities where rival custodial claims were tolerated (Karaite, Reform, secular-nationalist Hebrew institutions): if corpus continuity and register preservation hold under plural custodianship, the monopoly component is separable from the stewardship component.',
    'If the monopoly is separable, the extraction component of this tangled rope is larger than the reading''s self-assessment concedes and the low epsilon is an artifact of the reading''s own seat; if inseparable, the coordination reading holds and the authored epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodianship_vs_monopoly, empirical, 'Whether custodial authority is stewardship or monopoly rent riding on a genuine coordination function.').

omega_variable(
    internalized_delegitimization,
    'Is the secular speech community''s deference to the liturgical criterion structural (enforced by communal sanction and publication bans) or internalized (speakers themselves accept their usage as lesser or profane)?',
    'Post-exit trajectory: examine secular speakers and writers who left the traditional community — does the sense of profanation persist after sanction disappears? The maskilim''s and revivalists'' documented ambivalence toward the holy tongue is the historical trace to read.',
    'If substantially internalized, the arrangement''s effective suppression persists after enforcement reach collapsed and the victim seats'' effective extraction stays elevated even outside the enclaves; if structural, the suppression decay series is the true picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_delegitimization, empirical, 'Structural versus internalized mechanism of the delegitimation cost.').

omega_variable(
    founding_problem_liveness,
    'Is the founding problem — maintaining sacred-language continuity without native transmission — still live for any existing community, or has native transmission made the liturgical criterion''s problem generally obsolete?',
    'Demographic and practice data on communities whose only transmission path remains liturgical (traditional diaspora enclaves), and on whether the criterion still governs their linguistic self-understanding in practice rather than in formula.',
    'If live for those communities, the arrangement remains an operating tangled rope for them; if dead everywhere, it drifts toward inertial maintenance — a settled question kept open theatrically — and the rising theater series is the leading indicator.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the founding problem persists for any living community or the arrangement outlived it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 1780, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_liturgical_tr_t1780, living_language_status__liturgical_preservation_reading, theater_ratio, 1780, 0.12).
narrative_ontology:measurement_basis(lls_liturgical_tr_t1780, observed).
narrative_ontology:measurement(lls_liturgical_tr_t1830, living_language_status__liturgical_preservation_reading, theater_ratio, 1830, 0.15).
narrative_ontology:measurement_basis(lls_liturgical_tr_t1830, observed).
narrative_ontology:measurement(lls_liturgical_tr_t1880, living_language_status__liturgical_preservation_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement_basis(lls_liturgical_tr_t1880, observed).
narrative_ontology:measurement(lls_liturgical_tr_t1920, living_language_status__liturgical_preservation_reading, theater_ratio, 1920, 0.26).
narrative_ontology:measurement_basis(lls_liturgical_tr_t1920, observed).
narrative_ontology:measurement(lls_liturgical_tr_t1970, living_language_status__liturgical_preservation_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement_basis(lls_liturgical_tr_t1970, observed).
narrative_ontology:measurement(lls_liturgical_tr_t2020, living_language_status__liturgical_preservation_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(lls_liturgical_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(lls_liturgical_be_t1780, living_language_status__liturgical_preservation_reading, base_extractiveness, 1780, 0.22).
narrative_ontology:measurement_basis(lls_liturgical_be_t1780, observed).
narrative_ontology:measurement(lls_liturgical_be_t1830, living_language_status__liturgical_preservation_reading, base_extractiveness, 1830, 0.28).
narrative_ontology:measurement_basis(lls_liturgical_be_t1830, observed).
narrative_ontology:measurement(lls_liturgical_be_t1880, living_language_status__liturgical_preservation_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement_basis(lls_liturgical_be_t1880, observed).
narrative_ontology:measurement(lls_liturgical_be_t1920, living_language_status__liturgical_preservation_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement_basis(lls_liturgical_be_t1920, observed).
narrative_ontology:measurement(lls_liturgical_be_t1970, living_language_status__liturgical_preservation_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement_basis(lls_liturgical_be_t1970, observed).
narrative_ontology:measurement(lls_liturgical_be_t2020, living_language_status__liturgical_preservation_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement_basis(lls_liturgical_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(lls_liturgical_su_t1780, living_language_status__liturgical_preservation_reading, suppression_requirement, 1780, 0.35).
narrative_ontology:measurement_basis(lls_liturgical_su_t1780, observed).
narrative_ontology:measurement(lls_liturgical_su_t1830, living_language_status__liturgical_preservation_reading, suppression_requirement, 1830, 0.45).
narrative_ontology:measurement_basis(lls_liturgical_su_t1830, observed).
narrative_ontology:measurement(lls_liturgical_su_t1880, living_language_status__liturgical_preservation_reading, suppression_requirement, 1880, 0.55).
narrative_ontology:measurement_basis(lls_liturgical_su_t1880, observed).
narrative_ontology:measurement(lls_liturgical_su_t1920, living_language_status__liturgical_preservation_reading, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement_basis(lls_liturgical_su_t1920, observed).
narrative_ontology:measurement(lls_liturgical_su_t1970, living_language_status__liturgical_preservation_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement_basis(lls_liturgical_su_t1970, observed).
narrative_ontology:measurement(lls_liturgical_su_t2020, living_language_status__liturgical_preservation_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(lls_liturgical_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'is the language living?' decomposes into three readings of the living_language_status kernel, each a separate story with its own epsilon, beneficiary/victim structure, and type. This reading (liturgical_preservation) stands upstream of literary_continuity in one causal sense — the liturgically preserved corpus and the custodially trained literate class are the resource base the literary reading draws on — and is logically incompatible with native_generation within any single framework (liturgical transmission cannot both confer vitality and preserve a corpse). The epsilon divergence across the family (low here by the reading's own lights; intermediate under literary_continuity; high under native_generation for the same standing arrangement) is the measurement of the kernel contest, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
