% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Methodological Restrictiveness in Islamic Jurisprudence
 *   domain: legal/theological
 *
 * SUMMARY:
 *   The Hanbali reading of usul al-fiqh (Islamic jurisprudential methodology)
 *   is one of four primary schools' interpretations of how Islamic law ought
 *   to be derived from textual sources. This reading instantiates the most
 *   textually restrictive approach: Quranic text and authenticated hadith are
 *   treated as supreme and exhaustive authorities; qiyas (analogical
 *   reasoning) is minimized to cases where textual sources are definitively
 *   silent; weak hadith is preferred to rationalist analogy; and sadd
 *   al-dhara'i (blocking of innovations for the sake of preserving textual
 *   fidelity) is the protective mechanism that suppresses alternative
 *   methodological paths. The constraint extracts authority from rationalist
 *   jurists and customary-law practitioners, concentrating it in the hands of
 *   textualist scholars and hadith authenticators. The claim and metrics are
 *   intentionally divergent: this reading is CLAIMED as a tangled_rope
 *   (genuine coordination of a unified jurisprudential method PLUS asymmetric
 *   extraction of authority from other schools), while the metrics reflect
 *   high suppression and moderate-to-high extractiveness, signaling that the
 *   coordination function is intertwined with institutional dominance. The
 *   reading does not exist in isolation—it coexists with Hanafi, Maliki, and
 *   Shafi'i readings, all of which have historical standing and contemporary
 *   adherents.
 *
 * KEY AGENTS:
 *   - Textualist legal scholars (Hanbali tradition): maintain methodological authority; benefit from constraint; set the agenda for what counts as valid legal derivation
 *   - Rationalist jurists (Hanafi, Maliki, Shafi'i schools): pay the cost of reduced institutional standing; their juristic methodologies are suppressed as bid'a within the textualist frame
 *   - Customary-law practitioners: bear the suppressive cost of sadd al-dhara'i; their established norms lose institutional legitimacy
 *   - Hadith authenticators: gain gatekeeping authority; their determinations of textual authenticity directly control what legal sources are available
 *   - Adaptive legal development (as a functional capacity): treated as a victim because the constraint blocks the institutional pathways (qiyas, maslaha, istihsan) through which it occurs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.72).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Methodological Restrictiveness in Islamic Jurisprudence").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "legal/theological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'a5f3666f-d5ac-47d0-8370-af95bb86882c').
narrative_ontology:cs_kernel_codification('a5f3666f-d5ac-47d0-8370-af95bb86882c', fixed_text).
narrative_ontology:cs_authority_grounding('a5f3666f-d5ac-47d0-8370-af95bb86882c', lineage).
narrative_ontology:cs_interpretation_layer_present('a5f3666f-d5ac-47d0-8370-af95bb86882c').
narrative_ontology:cs_reading_relation('a5f3666f-d5ac-47d0-8370-af95bb86882c', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5f3666f-d5ac-47d0-8370-af95bb86882c', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5f3666f-d5ac-47d0-8370-af95bb86882c', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('a5f3666f-d5ac-47d0-8370-af95bb86882c', foundational, textual_supremacy_and_completeness).
narrative_ontology:cs_axiom_status(textual_supremacy_and_completeness, holdable).
narrative_ontology:cs_axiom_grounding('a5f3666f-d5ac-47d0-8370-af95bb86882c', textual_supremacy_and_completeness, deontological).
narrative_ontology:cs_axiom('a5f3666f-d5ac-47d0-8370-af95bb86882c', foundational, rationalist_expansion_is_bid_a).
narrative_ontology:cs_axiom_status(rationalist_expansion_is_bid_a, holdable).
narrative_ontology:cs_axiom_grounding('a5f3666f-d5ac-47d0-8370-af95bb86882c', rationalist_expansion_is_bid_a, empirically_contingent).
narrative_ontology:cs_reference_frame('a5f3666f-d5ac-47d0-8370-af95bb86882c', quranic_hadith_fidelity_framework).
narrative_ontology:cs_drift_state('a5f3666f-d5ac-47d0-8370-af95bb86882c', contemporary_global_islamic_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5f3666f-d5ac-47d0-8370-af95bb86882c', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_legal_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, adaptive_legal_development).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, adaptive_legal_development).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, weak_hadith_collectors).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, innovation_prevention_advocates).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hadith_authenticators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hanbali jurists and their intellectual descendants maintain institutional authority to adjudicate legal methodology. They define Quranic text and authenticated hadith as supreme sources, restrict qiyas to textually silent cases, and use sadd al-dhara'i to block alternative methodological paths. Their authority rests on a claim to textual fidelity as the binding standard. They set the agenda for what methodologies are legitimate within textualist communities and face no structural exit—leaving the textualist frame means abandoning a core part of their professional and intellectual identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Hanafi, Maliki, and Shafi'i jurists employ expansive qiyas, istihsan, maslaha mursala, and other reasoning-based methods that the Hanbali reading suppresses as bid'a. They pay the cost of reduced standing within textualist institutions and must operate in separate jurisprudential spheres. They retain institutional power in their own schools but are excluded from the authority-setting process within the textualist frame. Exit would require abandoning their jurisprudential methods and intellectual inheritance.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    institutional, generational, constrained, global).

% Communities with established 'urf (customary practices) that lack explicit Quranic or authenticated hadith warrant find their norms delegitimized by the Hanbali constraint. Sadd al-dhara'i blocks their practices as dangers to textual fidelity. They are trapped because abandoning custom means disrupting community cohesion, and conforming to textualist standards often means abandoning practices embedded in regional identity and long tradition. They have no structural exit from the constraint's suppression.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners, payer,
    moderate, biographical, trapped, regional).

% The institutional capacity to derive new rulings for novel circumstances through qiyas, maslaha, istihsan, and customary integration is suppressed. Legal scholars addressing emerging issues (technology governance, climate law, bioethics) must fit problems into the narrow textual frame or declare them forbidden. The constraint preserves institutional predictability (beneficiary element) but stifles responsive legal evolution (payer element). Scholars continue adaptive work in separate institutional channels but with reduced legitimacy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, adaptive_legal_development, payer,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, adaptive_legal_development, beneficiary).

% Collectors and transmitters of hadith with sub-optimal isnads (transmission chains) gain evidentiary standing under the Hanbali preference for weak hadith over qiyas. Their transmitted reports carry legal weight even when hadith critics would challenge their authenticity. They benefit from the constraint's elevation of their role relative to jurists who would bridge textual gaps via analogy. They have mobility—they can shift to other jurisprudential schools if the Hanbali frame becomes disadvantageous.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, weak_hadith_collectors, beneficiary,
    organized, biographical, mobile, global).

% Scholars and communities committed to preventing bid'a (unlawful religious innovations) find the Hanbali constraint's blocking mechanism (sadd al-dhara'i) as their institutional tool. The constraint legitimizes suppression of practices that might lead to deviation from textual warrant. They benefit from the conservative posture toward legal and customary change. They have mobility—they can operate across jurisprudential schools insofar as each school incorporates some bid'a-prevention mechanisms.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, innovation_prevention_advocates, beneficiary,
    organized, generational, mobile, global).

% Scholars of hadith authentication (ilm al-hadith) who establish criteria for textual authenticity hold gatekeeping authority under the Hanbali reading. Their determinations of 'authenticated hadith' versus 'weak hadith' directly shape what legal sources are available to jurists. The constraint elevates their institutional position and makes legal derivation depend on their verdicts. They have institutional power and mobility—their expertise is valuable across all jurisprudential schools, and their standards influence even schools that apply qiyas more expansively.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hadith_authenticators, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hadith_authenticators, beneficiary).

% Scholars of comparative law and Islamic jurisprudential pluralism examine how the Hanbali reading's methodological stance shapes legal pluralism, institutional authority, and the feasibility of frameworks accommodating multiple jurisprudential traditions. They take an external analytical position and do not collect from or bear costs to the constraint. Their role is to measure the constraint's structural impact on jurisprudential diversity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_legal_systems, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, textualist_legal_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, coherent legal methodology that coordinates Islamic jurisprudence around a single hierarchy of sources: Quranic text takes absolute precedence, authenticated hadith supplies the second tier, weak hadith may fill narrow gaps where text is silent, and qiyas (analogical reasoning) applies only where textual sources definitively provide no guidance. This creates a predictable, standardized approach to legal derivation that prevents the proliferation of divergent methodological standards.
% TRANSFER_FUNCTION: Transfers interpretive authority from rationalist jurists and customary-law practitioners to textualist scholars and hadith authenticators. Communities relying on customary norms, public-interest reasoning, or juristic preference lose standing and must conform their practices to the textualist reading or face delegitimization. Customary and adaptive legal development are blocked (sadd al-dhara'i) to preserve textual fidelity as the supreme good.
% ABSENT_VOICES: Rationalist jurists of the Hanafi, Maliki, and Shafi'i traditions, contemporary legal innovators addressing novel circumstances, customary-law practitioners embedded in regional traditions, and communities whose established practices lack textual warrant are structurally excluded from participation in the authority-setting process. They are characterized within the constraint's frame as deviants (mubtadi'un) or violators of textual fidelity; their objections are pre-classified as bid'a rather than heard as claims to legitimacy.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist constraint disappeared, Islamic jurisprudence would immediately diversify: Hanafi, Maliki, and Shafi'i methodologies would reassert as equally legitimate; customary practices would regain institutional standing; rationalist juristic preference and public-interest reasoning would re-emerge as valid sources; and legal scholars would again have authoritative scope to derive new rulings for novel circumstances via analogy, public interest, and reasoned opinion. The suppressed institutional capacity for adaptive legal development would reactivate, and multiple jurisprudential schools would coexist without the textualist constraint's hierarchical subordination.
% FOUNDING_PROBLEM: The founding problem was the perceived danger of unlimited rationalist expansion (ra'y, istihsan, maslaha mursala without textual grounding) leading to divergence from the Prophet Muhammad's transmitted teachings and the proliferation of unlawful innovations (bid'a) that corrupted Islamic practice. Early Hanbali jurists, particularly Ahmad ibn Hanbal and his direct students, faced a period of intense theological contestation (the Mihna, 9th-century CE) where rationalist theologians and jurists were exercising broad interpretive authority. The constraint was built to reassert the supremacy of textual sources and block the institutional pathways through which rationalist innovation could occur.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars attest the founding problem remains live: rationalist expansion, bid'a, and deviation from textual warrant are ongoing dangers requiring textual restrictiveness. Rationalist jurists (Hanafi and Maliki schools, contemporary Islamic legal scholars) attest the founding problem is substantially resolved: Islamic jurisprudence has matured through 14 centuries of developed schools, and the constraint's extreme textual restrictiveness now prevents legitimate legal adaptation to novel circumstances without textual precedent. Islamic legal historians document that rationalist methodologies produced stable, sophisticated jurisprudential systems rather than chaotic innovation, and that Hanbali textual restrictiveness emerged as one response to theological conflict, not as a solution to an enduring pathology. Legislative testimony from contemporary Muslim-majority jurisdictions demonstrates that legal systems require reasoning-based adaptation capacities that the Hanbali constraint severely limits.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises moderately over the interval (0.55 to 0.68) because the constraint's application accumulates: as textualist authority is asserted in specific legal domains, the range of issues that rationalist jurists can address shrinks, and the cost of operating outside the textualist frame increases. Suppression is high and stable (0.65–0.72) because the constraint's persistence depends on actively blocking the institutional mechanisms through which rationalist and customary legal development would otherwise occur—sadd al-dhara'i is a suppressive mechanism by design, not a byproduct. Theater is low (0.18–0.28) because the constraint's justification (textual fidelity, prevention of bid'a) is genuinely functional: the coordination role (unified methodology, reduced jurisprudential fragmentation) is real, even though it rides on extraction. The metrics do not swing upward to snare-like territory because the constraint does maintain a coordination function—it is not pure extraction masquerading as something else. Accessibility collapse is high (0.79) because once the textualist frame is accepted, alternative methodological paths appear as illegitimate deviations; for rationalist jurists, the collapse is lower (constrained rather than identity-locked), giving them some notional exit, but the exit carries the cost of reduced institutional legitimacy. Resistance is moderate (0.61) because rationalist jurists and adaptive legal development actively resist the constraint—they continue to publish jurisprudential works using expanded qiyas, maslaha, and istihsan—but their resistance operates in separate institutional channels rather than directly challenging the textualist frame's authority.
 *
 * PERSPECTIVAL GAP:
 *   The textualist scholars experience the constraint as a genuine coordination mechanism that prevents jurisprudential chaos and preserves fidelity to the Prophet's teachings. From their position, the constraint is a rope—beneficial coordination that all legitimate actors should accept. Rationalist jurists and contemporary Islamic legal scholars experience the same constraint as extraction: their methodological tools are delegitimized, their authority is concentrated in the hands of textualist gatekeepers, and their capacity to adapt law to novel circumstances is suppressed. From their position, the constraint looks like a snare wearing a coordination mask. The engine computes per-seat types from the structural data: the textualist agenda-setter will compute closer to rope (beneficiary, identity-locked, mobile exit relative to institutional alternatives); rationalist jurists will compute closer to snare or tangled_rope (victims, constrained exit, institutional power but severely curtailed). The perspectival gap is the core feature of a tangled_rope—real coordination machinery and real extraction operating through the same structure, experienced differently by different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for textualist scholars: low d (~0.2), they are beneficiaries who gain authority and institutional standing from the constraint. Directionality for rationalist jurists: high d (~0.75), they are victims whose methodological authority is suppressed and who must operate outside the textualist frame. Directionality for customary-law practitioners: high d (~0.8), their established practices are blocked by sadd al-dhara'i and have no institutional legitimacy. Directionality for hadith authenticators: low-to-moderate d (~0.3), they benefit from gatekeeping authority but are also constrained by the narrow scope for their findings (weak hadith can only fill specific gaps). The asymmetry is structural: textualist scholars control the frame and benefit from it; everyone else operates within constraints set by that frame or outside it with reduced legitimacy. No override is needed; the beneficiary/victim declarations and exit-option modulation produce appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (threat of rationalist expansion and bid'a) is contested in status: textualist scholars attest it is live; rationalist jurists and legal historians attest it is substantially dead. The disappearance verdict is world_rearranges: if the constraint vanished, Islamic jurisprudence would immediately reorganize around multiple methodological schools with restored standing. The mismatch (status=contested + verdict=world_rearranges) suggests the constraint may be a zombie—the founding problem it was built to solve is no longer recognized outside textualist communities, yet the constraint persists through institutional inertia and textualist agenda-setting authority, not because the underlying problem is still active. The theater_ratio rises gradually (0.18 to 0.28) as enforcement focuses more on excluding alternative methodologies than on preventing genuinely dangerous innovations—a Goodhart drift. The constraint is not a piton (theater is too low; the coordination function is genuinely maintained); it is a tangled_rope where the founding problem's obsolescence is contested and the extraction component is increasingly performative relative to the real coordination need. Mandatrophy is NOT resolved—the constraint persists despite the founding problem's contested/dead status, which is exactly the condition mandatrophy names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (threat of rationalist expansion leading to bid''a and deviation from textual warrant) still live, or has it been substantially addressed by 14 centuries of jurisprudential development and institutional stability?',
    'Historical analysis of whether rationalist legal methodologies (qiyas, istihsan, maslaha) in Hanafi, Maliki, and Shafi''i traditions have produced systematic legal chaos, bid''a, or stable jurisprudence. Comparative survey of contemporary Muslim-majority jurisdictions: do those using broader methodologies exhibit worse legal outcomes or bid''a-related instability?',
    'If the founding problem is dead or substantially resolved, the constraint becomes a zombie maintained by institutional inertia rather than genuine coordination need. This would reclassify it from tangled_rope to piton (performance-driven persistence without founding justification). If the problem is live, the constraint''s classification holds—tangled_rope with justified suppressive machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the original justification for textual restrictiveness remains applicable.').

omega_variable(
    methodological_foreclosure_vs_coexistence,
    'Is the Hanbali reading logically incompatible with rationalist methodologies (forecloses the Hanafi/Maliki/Shafi''i readings), or do the readings coexist as different institutional choices for different jurisprudential communities?',
    'Formal examination of the axioms: does ''textual sources are maximally restrictive and supreme'' logically contradict ''qiyas can be applied expansively'' in such a way that no framework could hold both? Or are these competing methodological choices that different communities can hold simultaneously?',
    'If foreclosure: the readings partition Islamic jurisprudence into incompatible frameworks, and the Hanbali reading''s dominance is a matter of institutional power, not logical necessity. If coexistence: the readings are live alternatives, and the constraint''s suppression of rationalist methods is a power play (extraction) rather than logical necessity. Current reading: coexists_with (coexistence is the structural relationship), which keeps extraction as the primary dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_foreclosure_vs_coexistence, conceptual, 'Whether Hanbali and rationalist methodologies are logically incompatible or institutionally coexistent.').

omega_variable(
    customary_practice_legitimacy,
    'Does Islamic jurisprudential tradition permit ''urf (customary practice) as an independent evidentiary source alongside textual warrant, or is custom legitimate only when it reinforces or fills gaps in textual sources?',
    'Close reading of classical Islamic jurisprudential texts and contemporary scholarly consensus on the status of ''urf. Analysis of whether Maliki incorporation of Medinan custom and Hanafi recognition of customary usage represent legitimate jurisprudential positions or deviations from sound methodology.',
    'If custom is a legitimate independent source, the Hanbali blocking of custom (sadd al-dhara''i against customary innovations) is a methodological choice that suppresses a recognized legitimate source—extraction. If custom is only legitimate when textually grounded, the Hanbali position is defending textual purity against illegitimate expansion—coordination. The suppression mechanism differs in character depending on this answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_practice_legitimacy, conceptual, 'Whether customary practice is an independent or derivative evidentiary source in Islamic jurisprudence.').

omega_variable(
    weak_hadith_evidentiary_status,
    'Is the Hanbali preference for weak hadith over qiyas justified by the weakness of analogical reasoning as a source, or by institutional commitments to hadith collection even when authentication standards are not met?',
    'Comparative analysis of qiyas reliability (how often analogical reasoning produces jurisprudential consensus vs. divergence) versus weak hadith reliability (how often weak hadith reports are later confirmed by stronger authentication or textual parallels). Survey of contemporary hadith scholarship on whether weak hadith preferences serve legal stability or create opacity.',
    'If weak hadith is more reliable than qiyas, the Hanbali preference is a sound methodological choice that improves legal precision—coordination with extraction side-effect. If qiyas is more reliable and weak hadith preference reflects institutional attachment to hadith sources over rational inference, the preference is an extraction mechanism that favors hadith specialists'' authority over jurists'' reasoning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weak_hadith_evidentiary_status, empirical, 'Whether weak hadith or qiyas produces more reliable legal derivations.').

omega_variable(
    sadd_al_dhara_i_scope_interpretation,
    'Is sadd al-dhara''i (blocking of innovations for the sake of preserving textual fidelity) a mechanism for preventing genuine risks to Islamic practice, or a catch-all suppressive tool for any practice that lacks textual precedent?',
    'Analysis of how sadd al-dhara''i has been applied historically: does it target practices that demonstrably lead to bid''a and doctrinal corruption, or does it function as a blanket block on customary, adaptive, and reasoned legal development? Examine cases where sadd al-dhara''i was invoked: what proportion involved clear pathways to harm versus mere novelty without precedent?',
    'If sadd al-dhara''i targets genuine risks: it is a legitimate protective mechanism, and suppression is justified coordination cost. If sadd al-dhara''i is applied broadly to suppress innovation regardless of risk: it is a pure extraction mechanism disguised as protective—reclassifies the constraint toward snare. Current metrics reflect uncertainty (moderate theater_ratio, high suppression): this omega would resolve whether the suppression is justified or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_scope_interpretation, empirical, 'Whether sadd al-dhara''i targets genuine risks or functions as blanket suppression of novelty.').

omega_variable(
    reading_identity_fusion_for_textualists,
    'For scholars identity-locked into the Hanbali reading, is the identity-lock a result of genuine methodological conviction or of institutional inheritance and socialization into a textualist framework that makes exit cognitively unthinkable?',
    'Ethnographic study of Hanbali scholar training and identity formation. Analysis of how scholars transition between jurisprudential schools or adopt rationalist methods: is transition experienced as intellectual conversion (change of conviction) or as identity betrayal (departure from core self)? Examine cases of scholars who moved from Hanbali to Hanafi or vice versa to understand the psychological and institutional costs.',
    'If identity-lock is primarily institutional/socialization-based: the constraint''s suppression is reinforced by identity fusion that persists even after institutional pressure is removed (internalized suppression). This deepens the extraction by making exit psychologically costly beyond institutional costs. If identity-lock is genuine methodological conviction: the constraint is held by choice, not just by power, and the extraction component is reduced (beneficiaries are holding the constraint through reasoned belief, not just institutional dominance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_for_textualists, empirical, 'Whether textualist identity-lock is institutional or conviction-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(usul_tr_t2, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2, 0.21).
narrative_ontology:measurement(usul_tr_t4, usul_al_fiqh_method__hanbali_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(usul_tr_t7, usul_al_fiqh_method__hanbali_reading, theater_ratio, 7, 0.26).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanbali_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(usul_tr_t14, usul_al_fiqh_method__hanbali_reading, theater_ratio, 14, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(usul_be_t2, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2, 0.59).
narrative_ontology:measurement(usul_be_t4, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(usul_be_t7, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(usul_be_t14, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 14, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(usul_su_t2, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(usul_su_t4, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(usul_su_t7, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 7, 0.71).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(usul_su_t14, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 14, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The 'usul_al_fiqh_method' kernel is decomposed into four constraint stories, one per Sunni school reading. Each reading has the same referent (the fundamental principles governing Islamic jurisprudential methodology) but different ε values based on the reading's own assessment of how restrictive, extractive, and suppressive the constraint is. The Hanbali reading (this file) has the highest ε (0.68) because it is the most textually restrictive and most suppressive of alternative methodologies. The Hanafi reading would have the lowest ε (highest openness to rationalist reasoning). Maliki and Shafi'i readings fall between. Each story declares its sibling readings and relationship type via cs_structure.reading_relations. Network links enable contamination propagation: if the Hanbali constraint's authority erodes in one institution, downstream pressure may affect how Shafi'i textual authentication standards are maintained (the Shafi'i reading influences Hanbali via hadith-rigor mechanics). No single reading is 'correct'—each is a structural account of how one jurisprudential school reads the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
