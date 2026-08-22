% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive and Identity Maintenance
 *   domain: religious/textual
 *
 * SUMMARY:
 *   Under this reading, Kodashim (the Talmudic tractate on Temple sacrifices)
 *   is studied as a historical archive and anchor for communal identity
 *   rather than as binding law or cosmic performance. The Temple's
 *   destruction made the law non-performable ~2,000 years ago, yet the
 *   rabbinic community maintained the obligation to study it. This reading
 *   interprets that obligation as serving identity preservation—demonstrating
 *   continuity with the Temple period and justifying rabbinic authority as
 *   guardian of the textual tradition. The constraint extracts intellectual
 *   resources from applied law into archival study and legitimizes that
 *   extraction through the narrative of unbroken continuity. The claim/metric
 *   gap is intentional: Kodashim study is claimed as fulfilling an obligation
 *   (tangled_rope: coordination + enforcement); the metrics reflect that
 *   extraction increasingly outweighs coordination as the archive
 *   justification becomes explicit and the performance/preparation
 *   justifications recede.
 *
 * KEY AGENTS:
 *   - Rabbinic interpreters: maintain the study obligation and justify it through archive/identity framing; exercise interpretive authority over the tradition
 *   - Jewish communal identity: benefits from the persistent engagement with ancient texts; anchors self-understanding to unbroken lineage
 *   - Applied legal study: bears the opportunity cost of diverted intellectual resources; receives less rabbinic attention because Kodashim commands it
 *   - Intellectual resources: abstract victim class representing the scholarly effort devoted to a defunct legal system
 *   - Temple restoration advocates: excluded minority who read the same obligation as preparation for literal restoration
 *   - Secular scholars: analytical observers who study the texts without accepting the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.58).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.42).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '59c38537-3474-4571-8e09-3fae538f445b').
narrative_ontology:cs_kernel_codification('59c38537-3474-4571-8e09-3fae538f445b', fixed_text).
narrative_ontology:cs_authority_grounding('59c38537-3474-4571-8e09-3fae538f445b', lineage).
narrative_ontology:cs_interpretation_layer_present('59c38537-3474-4571-8e09-3fae538f445b').
narrative_ontology:cs_reading_relation('59c38537-3474-4571-8e09-3fae538f445b', kodashim_obligation__study_as_performance, influences).
narrative_ontology:cs_reading_relation('59c38537-3474-4571-8e09-3fae538f445b', kodashim_obligation__study_as_preparation, influences).
narrative_ontology:cs_axiom('59c38537-3474-4571-8e09-3fae538f445b', foundational, kodashim_as_historical_archive).
narrative_ontology:cs_axiom_status(kodashim_as_historical_archive, holdable).
narrative_ontology:cs_axiom_grounding('59c38537-3474-4571-8e09-3fae538f445b', kodashim_as_historical_archive, conventional).
narrative_ontology:cs_axiom('59c38537-3474-4571-8e09-3fae538f445b', secondary, identity_continuity_over_functional_obligation).
narrative_ontology:cs_axiom_status(identity_continuity_over_functional_obligation, holdable).
narrative_ontology:cs_axiom_grounding('59c38537-3474-4571-8e09-3fae538f445b', identity_continuity_over_functional_obligation, instrumental).
narrative_ontology:cs_reference_frame('59c38537-3474-4571-8e09-3fae538f445b', archive_maintenance_as_continuity_anchor).
narrative_ontology:cs_drift_state('59c38537-3474-4571-8e09-3fae538f445b', contemporary_secular_jewish_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59c38537-3474-4571-8e09-3fae538f445b', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, rabbinic_authority).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, applied_legal_study).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, textual_continuity_as_identity_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the obligation to study sacrificial law (Kodashim) despite the Temple's destruction ~2,000 years ago. They administer study curricula, interpret the texts, and enforce the expectation that learned Jews engage this material. They justify the obligation through multiple readings: cosmic efficacy, messianic preparation, or (in this reading) historical preservation. Their authority derives from lineage—the unbroken transmission of rabbinic interpretation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, rabbinic_interpreters, agenda_setter,
    organized, generational, identity_locked, global).

% Gains continuity and legitimacy from the persistent engagement with Kodashim despite its legal irrelevance. Study of the defunct system anchors Jewish identity to an unbroken textual lineage stretching from the Temple period through diaspora to the present. The community's self-understanding as 'the people of the book' depends partly on maintaining this kind of archival study even when the law cannot be performed.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_communal_identity, beneficiary,
    institutional, civilizational, arbitrage, global).

% Bears the opportunity cost of Kodashim study within Jewish legal education. Finite time in yeshiva curricula, finite intellectual energy among scholars, finite publication resources—all are diverted to mastering a legal corpus with no performable output rather than deepening the law that actually governs Jewish practice (civil law, family law, kashrut, Shabbat). Applied law gets less scholarly attention because Kodashim commands study through obligation rather than necessity.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, applied_legal_study, payer,
    moderate, biographical, constrained, regional).

% Abstract victim class representing the scholarly attention, textual analysis, and interpretive energy devoted to a system with no real-world legal application. These resources—commentaries written, debates conducted, intellectual effort expended—could in principle go to applicable law, but the obligation to study Kodashim captures them for archival and identity purposes instead. No individual chooses this; it is structural to the community's educational and intellectual mandate.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, intellectual_resources, payer,
    powerless, biographical, trapped, global).

% Would argue that study of Kodashim should be preparation for literal Temple restoration (the study_as_preparation reading), and that the archive reading diminishes that expectation. They are marginalized in mainstream rabbinic discourse but not entirely absent; their position contests the reading this constraint instantiates.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, temple_restoration_advocates, excluded,
    moderate, civilizational, identity_locked, national).

% Study Kodashim as historical document without accepting the obligation. They see the texts as archives of Second Temple Judaism and resources for understanding rabbinic development, not as binding law or cosmic performance. Their participation is voluntary and framed as scholarship rather than obligation; they remain outside the obligation structure itself.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, secular_jewish_scholars, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, rabbinic_interpreters).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual and intellectual continuity across a 2,000-year diaspora: a shared engagement with the same corpus that the Second Temple generation studied anchors Jewish identity and demonstrates unbroken transmission of the tradition despite institutional rupture.
% TRANSFER_FUNCTION: Moves scholarly attention and intellectual resources from applied Jewish law into the study and interpretation of a defunct legal system. Study obligation transfers legitimacy to the rabbinic community as keepers of the textual tradition and benefits communal identity through the maintenance of archival knowledge.
% ABSENT_VOICES: Temple restoration advocates who hold study_as_preparation reading would object that this reading diminishes the expectation of literal restoration. Secular scholars external to the obligation would note that study-as-archive is an invented post-hoc justification rather than the original function. Non-Jewish scholars of Jewish law would observe that the obligation extracts intellectual labor without producing applicable output.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim disappeared, the Jewish community would not reorganize legally or institutionally—no law would fail, no practice would become impossible. But Jewish identity would lose one anchor to textual continuity and one mechanism for demonstrating unbroken tradition. The rabbinic establishment would lose interpretive authority tied to this corpus. Different constituencies would assess the rearrangement differently: identity-centered communities see loss; legally practical communities see efficiency gain; restoration-minded advocates see a reconfiguration of messianic expectation.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, Jewish legal obligation regarding sacrifices became impossible to perform but was not repealed by rabbinic authority. The community needed a way to maintain the tradition without abandoning either the obligation or the claim to continuity. Study was posited as a substitute for performance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts (Talmudic sources, later responsa) acknowledge that sacrificial law cannot be performed and that study is a substitute. The consensus position among academic historians of Judaism (Neusner, Boyarin, Brettler, and others writing from outside the obligation-accepting community) is that study-as-substitute was an invented solution to a problem created by political rupture, not a cosmic necessity. The founding problem—how to maintain the tradition after Temple destruction—was solved by the early rabbinic innovation of study-as-performance or study-as-preparation; the archive reading treats those innovations as their own residue.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint captures real scholarly resources for archive purposes but does not impose coercive penalties on those who study less—the obligation is socially enforced rather than legally enforced in the modern diaspora context. Suppression is lower (0.42) because alternatives exist: one can be Jewish without mastering Kodashim, can study it without accepting the obligation framing, or can relocate that study to secular academic contexts. Theater ratio is high (0.68) at the interval endpoint because the justification for the obligation has shifted from performance/preparation (early rabbinic period) to identity/archive (modern period), and this shift itself is partly performative—the community enacts continuity by studying even though the law is inoperative. Accessibility collapse is moderate (0.45): the alternatives to engagement are available but carry identity costs (one loses connection to a marker of Jewish continuity if one doesn't study). Resistance is moderate-high (0.52) because a significant scholarly tradition contests the archive reading (both the performance and preparation readings mount real resistance). The time series shows extractiveness rising as the archive justification becomes dominant and theater rising as performance/preparation justifications decline in the mainstream—early rabbinic period had lower theater (the justifications were more actively contested) and higher suppression requirement (the obligation needed more active enforcement to hold community participation).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic and communal-identity seats should compute very differently from the applied-law and intellectual-resource seats. The engine's per-seat computation captures this: from the rabbinic/identity perspective the obligation is genuine coordination (preserving tradition, maintaining continuity); from the applied-law perspective it is extraction (resources diverted from what actually governs practice). The academic observer seat notes the historical truth: the founding problem was the Temple's destruction, which created a coordination need (how to maintain the tradition); the archive reading then treats the early rabbinic innovations (study-as-performance, study-as-preparation) as solved problems whose solutions have become the new norm, extracting resources not for coordination but for the continuation of those solutions as identity markers.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic interpreters are near the beneficiary end (d ~0.2): they gain interpretive authority and legitimacy from maintaining the obligation. Jewish communal identity is the structural beneficiary (d ~0.15): gains continuity and identity anchors. Applied legal study and intellectual resources are targets (d ~0.85 and d ~0.9): bear the opportunity cost without collecting benefit. The applied law victim is moderately powerful (organized scholarship) but constrained by the cultural obligation; intellectual resources are powerless (abstract) and trapped. Temple restoration advocates are partially excluded (d undefined in this reading, but their resistance to the archive framing matters for the R5 mismatch analysis).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids misclassifying extraction as pure coordination by explicitly naming the archive function. Pure rope would claim the study genuinely solves a live coordination problem; pure snare would hide the coordination function and claim only efficiency or necessity. Tangled rope fits: there IS a real coordination function (preserving textual tradition, maintaining communal continuity after Temple destruction), AND there IS asymmetric extraction (applied law bears costs for identity benefits). The R5 mismatch (founding_problem_status = dead, disappearance_verdict = contested) flags mandatrophy: the founding problem was solved by the early rabbinic innovations, but the obligation persists because it now serves identity rather than continuity-urgency. The archive reading is honest about this—it names the shifted function explicitly—whereas the performance and preparation readings treat the obligation as perpetually tied to the original founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_obligation_ambiguity,
    'Is Kodashim study genuinely obligatory in this reading, or is it a voluntary identity practice that has been reframed as obligation to maintain rabbinic authority?',
    'Examine contemporary rabbinic rulings on the Kodashim obligation: if halakhic authorities describe it as binding (deoraita or derabbanan), the obligation is real; if they describe it as aspirational or identity-marking, the reframing is explicit.',
    'If genuinely obligatory, the constraint is Tangled Rope as classified. If the obligation is performative (maintained for identity and authority, not for law), the constraint shifts toward Snare (pure extraction via authority maintenance, with archive as cover story).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archive_vs_obligation_ambiguity, empirical, 'Whether the obligation to study Kodashim is a binding legal requirement or an identity practice maintained through obligatory framing.').

omega_variable(
    performance_vs_archive_precedence,
    'Did the performance/preparation readings emerge as authentic early rabbinic interpretations, or are they retroactive justifications for an obligation originally instituted for archive purposes?',
    'Textual analysis of Talmudic sources: if early rabbinic discussions treat study as genuine substitute for sacrifice (functionally equivalent), performance is primary; if they treat study as preserving knowledge while acknowledging non-performance, archive is primary.',
    'If performance is primary, the archive reading is a late innovation that deprioritizes the original function—classification remains Tangled Rope but extractiveness should be higher (the function shift is itself extractive). If archive is primary, the early justifications were cover stories and the constraint is primarily extractive from its founding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_archive_precedence, empirical, 'Historical priority of performance/preparation vs. archive justifications in early rabbinic literature.').

omega_variable(
    identity_lock_mechanism,
    'For rabbinic interpreters, how much of their identity-lock to this obligation comes from genuine belief in the obligation''s legitimacy, and how much comes from institutional dependence on their role as interpreters of the tradition?',
    'Ethnographic study of rabbinic decision-making and curriculum design; interviews with interpreters about their reasons for maintaining the obligation.',
    'If identity-lock is primarily belief-based, their exit is identity_locked (genuine integration of the obligation into self-concept). If primarily institutional, their exit is constrained (they could exit but their role depends on not doing so). Different exit classifications affect the directionality derivation and the structural stability of the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Source of identity-lock for rabbinic interpreters: belief or institutional dependence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t250, kodashim_obligation__study_as_archive, theater_ratio, 250, 0.28).
narrative_ontology:measurement_basis(koda_tr_t250, projected).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_archive, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(koda_tr_t500, projected).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_archive, theater_ratio, 1000, 0.58).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_archive, theater_ratio, 1500, 0.65).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_obligation__study_as_archive, theater_ratio, 2000, 0.68).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t250, kodashim_obligation__study_as_archive, base_extractiveness, 250, 0.42).
narrative_ontology:measurement_basis(koda_be_t250, projected).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_archive, base_extractiveness, 500, 0.48).
narrative_ontology:measurement_basis(koda_be_t500, projected).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_archive, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_archive, base_extractiveness, 1500, 0.57).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_obligation__study_as_archive, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(koda_su_t0, projected).
narrative_ontology:measurement(koda_su_t250, kodashim_obligation__study_as_archive, suppression_requirement, 250, 0.65).
narrative_ontology:measurement_basis(koda_su_t250, projected).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_archive, suppression_requirement, 500, 0.58).
narrative_ontology:measurement_basis(koda_su_t500, projected).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_archive, suppression_requirement, 1000, 0.48).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_archive, suppression_requirement, 1500, 0.44).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_obligation__study_as_archive, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(koda_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.12).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the same obligatory practice. The archive reading treats Kodashim study as historical preservation and identity-maintenance; the performance reading treats it as cosmic efficacy despite Temple absence; the preparation reading treats it as binding knowledge-keeping for messianic restoration. Each has distinct ε, distinct victim/beneficiary sets, and distinct mandatrophy status. The archive reading influences the other two by making the archival function explicit, which undermines the urgency of performance/preparation justifications but does not logically foreclose them. All three remain live in different sectors of Jewish practice and interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
