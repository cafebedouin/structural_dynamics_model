% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates the Hanafi reading of the usul al-fiqh kernel: a
 *   legal-theoretical commitment to methodological hierarchy among sources of
 *   Islamic law. Where textual sources (Quran, hadith) are silent, this
 *   reading grants qiyas (analogical reasoning) expansive applicability;
 *   where qiyas reaches its limits, ra'y (independent reasoned opinion)
 *   supplements; and istihsan (juristic preference) authorizes departure from
 *   the 'obvious' analogical result when a jurist judges public interest
 *   requires it. The reading concentrates interpretive discretion in a
 *   specially trained jurist class and correspondingly narrows the
 *   evidentiary weight given to hadith-based textualism. Sibling readings
 *   (Maliki, Shafi'i, Hanbali) are NOT part of this constraint — they are
 *   separate constraint stories linked by network edges, each with its own
 *   epsilon and stakeholder structure.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurist_class: Primary beneficiary and agenda-setter (institutional/arbitrage) — administers the expansive methodology and captures the interpretive-authority premium it creates
 *   - hanafi_court_administrators: Secondary beneficiary (institutional/mobile) — gains administrative flexibility from istihsan discretion
 *   - textualist_hadith_scholars: Primary payer (organized/constrained) — loses evidentiary weight whenever qiyas or istihsan displaces literal hadith rulings
 *   - lay_litigants_seeking_predictable_rulings: Diffuse payer (powerless/trapped) — bears unpredictability cost of judicial discretion
 *   - rival_school_jurists: Excluded voice (organized/constrained) — objects from outside the adjudicating forum
 *   - comparative_legal_historians: Analytical observer (analytical/analytical) — traces doctrinal divergence across schools and centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.33).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '13533e18-2dc0-4809-a830-f4af474856c3').
narrative_ontology:cs_kernel_codification('13533e18-2dc0-4809-a830-f4af474856c3', distributed).
narrative_ontology:cs_authority_grounding('13533e18-2dc0-4809-a830-f4af474856c3', practice).
narrative_ontology:cs_interpretation_layer_present('13533e18-2dc0-4809-a830-f4af474856c3').
narrative_ontology:cs_reading_relation('13533e18-2dc0-4809-a830-f4af474856c3', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('13533e18-2dc0-4809-a830-f4af474856c3', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('13533e18-2dc0-4809-a830-f4af474856c3', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('13533e18-2dc0-4809-a830-f4af474856c3', foundational, juristic_analogy_expansively_valid_absent_text).
narrative_ontology:cs_axiom_status(juristic_analogy_expansively_valid_absent_text, holdable).
narrative_ontology:cs_axiom_grounding('13533e18-2dc0-4809-a830-f4af474856c3', juristic_analogy_expansively_valid_absent_text, conventional).
narrative_ontology:cs_axiom('13533e18-2dc0-4809-a830-f4af474856c3', foundational, public_interest_overrides_strict_analogical_result).
narrative_ontology:cs_axiom_status(public_interest_overrides_strict_analogical_result, holdable).
narrative_ontology:cs_axiom_grounding('13533e18-2dc0-4809-a830-f4af474856c3', public_interest_overrides_strict_analogical_result, instrumental).
narrative_ontology:cs_reference_frame('13533e18-2dc0-4809-a830-f4af474856c3', kufan_ray_tradition).
narrative_ontology:cs_drift_state('13533e18-2dc0-4809-a830-f4af474856c3', post_hadith_compilation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('13533e18-2dc0-4809-a830-f4af474856c3', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_court_administrators).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_litigants_seeking_predictable_rulings).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, juristic_reasoning_can_extend_law_beyond_explicit_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in qiyas, ra'y, and istihsan methodology, this class administers courts, issues fatwas, and adjudicates novel cases where text is silent. Their specialized reasoning skill is the scarce resource the whole system prices; the wider the domain of textual silence they can claim, the more indispensable their interpretive labor becomes. They can move between jurisdictions and schools with relative ease, arbitraging their training against demand.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, beneficiary).

% State-appointed qadis operating under Hanafi doctrine benefit from a methodology flexible enough to accommodate administrative convenience, evolving commercial practice, and state interest, while still claiming continuity with revealed sources. Istihsan gives them discretion that other schools' judges lack.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_court_administrators, beneficiary,
    institutional, generational, mobile, continental).

% Scholars whose authority rests on hadith transmission and verification find their evidentiary weight diminished whenever qiyas or istihsan is invoked to depart from a literal or narrowly-attested textual ruling. They can contest rulings within Hanafi forums but cannot exit the shared legal-theological space without abandoning influence over the dominant school of their region.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars, payer,
    organized, civilizational, constrained, continental).

% Ordinary petitioners bringing disputes to Hanafi courts cannot predict outcomes as reliably as they could under a more restrictive textual regime, because istihsan permits the judge to depart from the analogically 'obvious' ruling when the judge deems public interest requires it. They have no practical alternative forum within the jurisdiction.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_litigants_seeking_predictable_rulings, payer,
    powerless, immediate, trapped, local).

% Shafi'i, Maliki, and Hanbali jurists operating in the same broader legal-theological universe would object that expansive qiyas and istihsan erode textual discipline, but within Hanafi-administered courts and territories their objections carry no binding force; they are heard, if at all, only in inter-school polemical literature, not in the adjudicating forum itself.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rival_school_jurists, excluded,
    organized, civilizational, constrained, continental).

% Study the four readings comparatively, tracing how each school's source-hierarchy commitments produced divergent doctrines on commercial contracts, criminal liability, and family law across empires and centuries.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable method for extending revealed law to novel cases the founding texts do not directly address — essential for governing a rapidly expanding, commercially and administratively complex empire where textual sources alone could not answer every emerging question.
% TRANSFER_FUNCTION: Moves interpretive authority and its attendant social and economic capital toward jurists skilled in analogical and discretionary reasoning, and away from jurists whose authority rests primarily on hadith transmission; also shifts predictability costs onto litigants, who bear the uncertainty that judicial discretion introduces.
% ABSENT_VOICES: Textualist hadith scholars and rival-school jurists would object that istihsan's 'public interest' departure from qiyas is a license for arbitrary judicial preference dressed in juristic vocabulary; within Hanafi-administered fora their objections are recorded in inter-school treatises but do not bind rulings.
% DISAPPEARANCE_RATIONALE: If expansive qiyas, ra'y, and istihsan were withdrawn overnight, Hanafi courts would lose their primary mechanism for ruling on unprecedented commercial, administrative, and social questions; rulings would either freeze at existing precedent or the school would have to import a Shafi'i- or Hanbali-style restrictive methodology, materially changing outcomes for merchants, administrators, and litigants across the affected territories.
% FOUNDING_PROBLEM: Early Islamic legal practice in Kufa and the broader Abbasid administrative sphere confronted vast numbers of commercial, criminal, and civil questions on which the Quran and available hadith were silent or thinly attested; a method was needed to derive rulings without either paralysis or arbitrary fiat.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the problem remains permanently live, since textual silence recurs with every new social and commercial arrangement. Rival-school jurists and modern comparative legal historians attest that much of the original problem (governing a rapidly expanding early empire with limited hadith transmission networks) has receded, and that expansive istihsan now functions substantially to preserve interpretive discretion and institutional position for the jurist class rather than to solve an ongoing textual gap.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).
:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the coordination function (resolving textual silence) is genuine and load-bearing, but istihsan's public-interest override channel concentrates discretionary power in a jurist class whose training becomes indispensable precisely because the domain of 'textual silence' is elastic under this reading. Suppression (0.33) is comparatively low because exit within the broader Islamic legal tradition exists (litigants and scholars can appeal to or migrate toward other schools where politically and geographically possible), but it is nonzero because within a given Hanafi-administered jurisdiction, forum choice is often unavailable to ordinary litigants. Theater ratio (0.22) is low-to-moderate: the methodology does real interpretive work, though a growing share of istihsan invocations in later centuries functioned more to ratify administrative convenience than to solve genuine textual gaps — hence the mild upward drift over the interval. Accessibility collapse (0.48) reflects that alternative source-hierarchies (Shafi'i, Hanbali methodologies) remain visible and articulated in comparative literature, so the Hanafi reading has not fully foreclosed awareness of alternatives, even though switching costs for litigants and scholars embedded in a Hanafi-administered polity are high. Resistance (0.58) is substantial: textualist scholars have persistently and organizedly contested the expansive use of ra'y and istihsan since at least the early controversies between ahl al-ra'y and ahl al-hadith.
 *
 * PERSPECTIVAL GAP:
 *   From the jurist class's seat, the methodology is coordination: a principled, source-respecting way to answer questions the revealed texts do not address, preserving legal coherence across a vast and changing empire. From the textualist scholar's seat, the same methodology is extraction of interpretive authority: istihsan in particular is read as a jurist substituting personal judgment for textual constraint under the cover of 'preference,' with the departure from qiyas functioning as a license rather than a discipline. The engine computes these as different seat-level types from the same structural data — the divergence is expected and is not an error in either seat's classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The rationalist-trained jurist class and court administrators are declared beneficiaries: their specialized training is the resource the system prices, and expansive textual silence increases demand for their interpretive labor. Textualist hadith scholars are declared victims: their comparative advantage (hadith transmission and verification) is structurally devalued whenever qiyas or istihsan displaces a hadith-grounded literal ruling. Lay litigants are also victims by directionality, though for a different reason — not loss of professional standing but loss of outcome predictability, since istihsan-based departures are by design less mechanically foreseeable than strict qiyas. Rival-school jurists are excluded rather than coordinated or extracted-from directly by this constraint; their stake is in a different (sibling) reading's success, so their directionality here is best captured by exclusion rather than victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing a fast-expanding early Islamic polity where textual sources were often silent) was genuinely live at the methodology's formation. Whether it remains live today, when hadith corpora are far more extensively compiled and cross-referenced than in the Kufan period, is contested — hence founding_problem_status is authored as 'contested' rather than 'dead,' preventing this story from either over-crediting the methodology as eternally necessary coordination or dismissing it as pure legacy extraction. The tangled_rope classification captures this directly: a genuine coordination function (resolving textual silence) persists alongside asymmetric extraction (concentration of interpretive authority and its attendant social capital in a specific, trained jurist class) enforced through the same court structures that also perform the coordination work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    istihsan_discipline_vs_arbitrary_preference,
    'Is istihsan a disciplined juristic method with identifiable, replicable criteria for departing from qiyas, or is it functionally equivalent to unconstrained judicial discretion dressed in technical vocabulary?',
    'Systematic review of documented istihsan rulings across centuries of Hanafi case law, checking whether departures from qiyas cluster around identifiable, consistent criteria (e.g. necessity, custom, avoidance of hardship) versus varying unpredictably with the individual jurist and case.',
    'If istihsan shows consistent, replicable criteria, the coordination function is stronger than the extraction reading suggests, pulling the classification toward rope. If departures are idiosyncratic and jurist-dependent, the extraction reading is strengthened, pulling toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_discipline_vs_arbitrary_preference, empirical, 'Whether istihsan operates as disciplined method or discretionary license.').

omega_variable(
    founding_problem_persistence,
    'Does the original justification for expansive qiyas and istihsan — governing an empire with limited hadith transmission — still hold now that hadith corpora are comprehensively compiled and cross-verified, or has the methodology''s justification shifted to preserving jurist-class authority?',
    'Compare the rate and substantive content of istihsan invocations before and after major hadith compilation efforts (e.g. the canonical six-book period) to see whether reliance on discretionary reasoning tracked genuine textual gaps or continued independent of textual availability.',
    'If reliance on istihsan did not decline as textual availability increased, this supports reading the methodology''s persistence as institutional-inertia/interest-preservation rather than ongoing genuine coordination need — strengthening the tangled_rope (or even drift toward snare) classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding coordination problem has substantially resolved while the methodology persists.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the Hanafi reading''s characterization here (expansive qiyas/ra''y/istihsan as the school''s defining structural feature) itself shaped by polemical framing from rival schools, or is it an accurate structural account independent of inter-school rivalry?',
    'Cross-check primary Hanafi usul al-fiqh treatises (e.g. al-Sarakhsi, al-Bazdawi) against rival-school polemical characterizations of Hanafi method to see whether the ''expansiveness'' framing originates within the Hanafi tradition''s own self-description or is primarily a rival-school critique adopted uncritically here.',
    'If the framing is substantially rival-school polemic rather than Hanafi self-understanding, the beneficiary/victim structure and epsilon value authored here may overstate the reading''s extractiveness relative to how the tradition understands its own methodological discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether this story''s characterization of Hanafi method reflects the tradition''s self-understanding or rival-school framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanafi_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__hanafi_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__hanafi_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 80, 0.32).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 100, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings decomposing the natural-language concept 'usul al-fiqh method' per the epsilon-invariance principle. Each reading assigns different weights to qiyas, ra'y, istihsan, hadith authentication, ijma scope, and textual restrictiveness, producing structurally distinct beneficiary/victim sets and likely distinct epsilon values. The Hanafi reading is expected to show the highest extractiveness toward textualist scholars and the strongest beneficiary concentration in the rationalist jurist class; the Hanbali reading is expected to show the inverse pattern. All four readings should be treated as a linked constraint family, not as a single constraint with an averaged epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
