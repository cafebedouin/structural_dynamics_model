% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Reading of Usul al-Fiqh: Authenticated-Hadith Gate on Legal Derivation
 *   domain: religious/legal_theoretical/comparative_law
 *
 * SUMMARY:
 *   In the late eighth and early ninth century, Islamic law had fragmented
 *   into divergent regional traditions — Medinan practice-centered
 *   derivation, Iraqi opinion-centered derivation — while reports attributed
 *   to the Prophet circulated with invented, truncated, or unverifiable
 *   chains of transmission. Al-Shafi'i (d. 820) answered with a formal
 *   meta-discipline, usul al-fiqh, codified in al-Risala: no legal derivation
 *   without an authenticated hadith; analogy (qiyas) admissible only where
 *   authenticated text is silent; consensus (ijma) restricted to the
 *   Companions' generation; the sources ranked in a fixed hierarchy. The same
 *   structure that solved the coordination problem also redistributed
 *   authority: certification of reports became the gate through which all
 *   legal legitimacy passed, and the specialists who ran the certification
 *   apparatus collected standing, students, and endowed posts, while jurists
 *   whose authority rested on trained personal judgment saw their traditional
 *   instruments (ra'y, istihsan, expansive qiyas) condemned as illegitimate.
 *   This file instantiates ONE reading of the usul_al_fiqh_method kernel —
 *   the shafii_reading — as a clean, epsilon-invariant constraint; the
 *   hanafi, maliki, and hanbali readings are separate constraints in separate
 *   files. The claimed type (tangled_rope) and the metrics are authored
 *   independently: the claim states what I believe is structurally true
 *   (genuine coordination function plus asymmetric extraction under active
 *   enforcement), the metrics state what I believe is descriptively true of
 *   the arrangement's operation.
 *
 * KEY AGENTS:
 *   - - hadith_transmission_specialists: Primary beneficiary and de facto administrator (organized/identity_locked) — runs the certification apparatus that gates all legal derivation; collects standing, students, and endowed posts
 *   - - rationalist_jurists: Primary target (organized/constrained) — bears the loss of methodological legitimacy; retention of standing requires retraining and textual anchoring of conclusions
 *   - - shafii_method_jurists: Secondary beneficiary (organized/constrained) — holds a portable methodological credential whose value depends on the gate staying closed
 *   - - abbasid_administration: Institutional beneficiary (institutional/mobile) — gains judicial legibility and predictable rulings across provinces through patronage rather than doctrinal commitment
 *   - - lay_muslim_communities: Diffuse payer-beneficiary (powerless/trapped) — receives uniform, textually anchored law; pays through loss of custom's standing and total dependence on credentialed specialists
 *   - - unauthenticated_report_transmitters: Excluded seat (moderate/constrained) — barred from legal argument by the certification requirement with no seat in the councils that set admission standards
 *   - - comparative_legal_historians: Analytical observer (analytical/analytical) — reconstructs the formation and competition of the four methodological orders from surviving polemics and records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.58).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.55).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Reading of Usul al-Fiqh: Authenticated-Hadith Gate on Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious/legal_theoretical/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '4e72cce3-d6f6-49c9-95bd-70391856aaae').
narrative_ontology:cs_kernel_codification('4e72cce3-d6f6-49c9-95bd-70391856aaae', formalized).
narrative_ontology:cs_authority_grounding('4e72cce3-d6f6-49c9-95bd-70391856aaae', lineage).
narrative_ontology:cs_interpretation_layer_present('4e72cce3-d6f6-49c9-95bd-70391856aaae').
narrative_ontology:cs_reading_relation('4e72cce3-d6f6-49c9-95bd-70391856aaae', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('4e72cce3-d6f6-49c9-95bd-70391856aaae', usul_al_fiqh_method__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('4e72cce3-d6f6-49c9-95bd-70391856aaae', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('4e72cce3-d6f6-49c9-95bd-70391856aaae', foundational, authenticated_sunna_prerequisite_for_derivation).
narrative_ontology:cs_axiom_status(authenticated_sunna_prerequisite_for_derivation, holdable).
narrative_ontology:cs_axiom_grounding('4e72cce3-d6f6-49c9-95bd-70391856aaae', authenticated_sunna_prerequisite_for_derivation, deontological).
narrative_ontology:cs_axiom('4e72cce3-d6f6-49c9-95bd-70391856aaae', foundational, qiyas_only_in_textual_absence).
narrative_ontology:cs_axiom_status(qiyas_only_in_textual_absence, holdable).
narrative_ontology:cs_axiom_grounding('4e72cce3-d6f6-49c9-95bd-70391856aaae', qiyas_only_in_textual_absence, deontological).
narrative_ontology:cs_reference_frame('4e72cce3-d6f6-49c9-95bd-70391856aaae', authenticated_prophetic_precedence).
narrative_ontology:cs_drift_state('4e72cce3-d6f6-49c9-95bd-70391856aaae', classical_consolidation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('4e72cce3-d6f6-49c9-95bd-70391856aaae', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_method_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, abbasid_administration).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, lay_muslim_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, lay_muslim_communities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, isnad_reliability_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, formal_source_hierarchy_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach, transmit, and verify reports about the Prophet's words and deeds through chains of named transmitters; certify which reports may be cited in legal argument; train students in transmitter biography and chain criticism. Their scholarly standing, livelihoods, and networks are built on the certification craft, and their lineages of teacher-to-student authorization span generations; leaving the craft would dissolve the expertise that constitutes them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter).

% Decide cases and teach law in the Iraqi tradition, deriving rulings from reasoned opinion, juristic preference, and analogy where reports are absent or thin. Under the new methodological order their traditional instruments are condemned as illegitimate; retaining standing requires retraining in report criticism and accepting that their conclusions carry weight only when textually anchored. Many hold judgeships and endowed teaching posts they would risk by open defiance.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    organized, biographical, constrained, continental).

% Practice law through the new sequence of sources, gaining a portable methodological credential recognized across regions and a decisive answer in disputes with older schools. They invest heavily in mastering both report criticism and derivation technique; their advantage depends on the continued scarcity of that combined competence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_method_jurists, beneficiary,
    organized, biographical, constrained, continental).

% Appoints judges and oversees courts across the provinces. A shared derivation method makes judicial appointments legible, rulings more predictable across regions, and religious legitimacy cheaper to secure; the dynasty patronizes scholars of the new methodology without committing to its internal disputes, and can shift patronage if the landscape changes.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, abbasid_administration, beneficiary,
    institutional, generational, mobile, continental).

% Live under rules derived by whichever school holds their region's courts. They receive more uniform and textually anchored law than under fragmented regional custom, and are shielded from rules traced to invented reports; they pay through the loss of local custom's standing, dependence on credentialed specialists for any religious-legal question, and the long training pipelines funded by their endowments and taxes. There is no exit from the legal order governing their communities.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, lay_muslim_communities, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, lay_muslim_communities, beneficiary).

% Carry and circulate reports without complete chains or with gaps and disputed links — popular preachers, storytellers, and compilers of edifying material. Their wares are barred from legal argument by the certification requirement; they have no seat in the methodological councils that set admission standards and no appeal from a failed chain.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, unauthenticated_report_transmitters, excluded,
    moderate, biographical, constrained, continental).

% Reconstruct how the methodological orders of the four schools formed, competed, and borrowed from one another, using surviving polemics, court records, and curriculum lists; they take no side in the disputes and hold no stake in any school's standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: jurists across a vast empire needed a shared rule for which materials ground law and which yield when sources conflict. The ordered hierarchy (Quran, then authenticated reports, then the Companions' consensus, then analogy) gives every trained jurist the same decision procedure, filters reports traceable to invention, and lets courts in different provinces reach recognizably consistent results.
% TRANSFER_FUNCTION: Moves legal authority — the recognized capacity to ground binding rulings — from jurists exercising trained personal judgment toward specialists who certify transmission chains; and moves deference away from living regional practice toward fixed textual precedent authenticated by past generations.
% ABSENT_VOICES: Non-elite Muslims whose customary law lost standing had no seat in the methodological councils; women, whose access to legal knowledge ran through male specialist networks, were absent from the standard-setting debates; transmitters with defective chains could not contest their exclusion. Among the literate elite, Iraqi rationalist jurists were present but progressively outnumbered in the curricular canon that later generations inherited.
% DISAPPEARANCE_RATIONALE: Without the authentication prerequisite and the ordered hierarchy, legal derivation reverts to regional practice and personal judgment: the Iraqi and Medinan styles resume unconstrained expansion, courts in different provinces issue incompatible rulings, and reports of unknown origin re-enter legal argument. Every school's curriculum, every judge's credential, and the relative standing of transmission specialists versus reasoning jurists would reorganize within a generation.
% FOUNDING_PROBLEM: By the late eighth century, Islamic law had fragmented into divergent regional traditions applying different sources, while reports attributed to the Prophet circulated with invented, truncated, or unverifiable chains; no principled procedure existed for deciding which materials bound and which yielded on conflict.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of early Islamic law working outside any madhhab document both the fabrication problem and the regional divergence from independent manuscript and prosopographical evidence; reformist Muslim jurists critical of the received corpus attest that source-conflict and reliability questions remain unresolved, while disputing whether this reading's solution is the correct one. The beneficiary seats themselves obviously attest the problem's liveness; the external corroboration is what licenses treating that attestation as more than cover.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (endpoint of the series): the certification gate levies a real authority transfer — the right to ground binding rulings moved from reasoning jurists to transmission certifiers — but the transfer is bounded because the filter addressed a documented fabrication problem and because rival methodological orders survived alongside it. Suppression is authored at 0.55 as a raw structural property (unscaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, inside the engine): polemical condemnation of istihsan, reputational sanction, curricular dominance, and eventually method-aligned judicial appointments; partial, because the Hanafi and Maliki orders persisted for centuries. Theater_ratio 0.25: isnad criticism was substantially functional scholarship; performative rigor grew with status competition but never dominated. Accessibility_collapse 0.40: alternatives did not collapse — three sibling readings remained live, which is the signature of a hybrid rather than a pure-extraction structure. Resistance 0.60: sustained inter-school polemic across the whole interval. All three tracked series run on one shared time grid (800/820/840/860/890/920/950) so no metric row borrows another's endpoints; suppression_requirement is tracked because the story specifically traces enforcement-intensity change (machinery maturing from polemic toward institutionalized curricular and appointment leverage), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the transmission specialists' position the arrangement is epistemic hygiene they are uniquely qualified to perform, and the authority that flows to them is deserved payment for a service everyone needs. From the rationalist jurists' position the same structure is dispossession: instruments refined over generations ruled inadmissible by a standard their rivals happen to monopolize. From the lay communities' position the visible surface is almost entirely coordination — more uniform law, fewer invented rules — with the costs diffuse and invisible. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. hadith_transmission_specialists sit nearest the beneficiary end: they collect the certification premium and control the admission standard (d near 0.0, amplified by identity_locked exit — their scholarly selves are constituted by the chain-criticism craft). rationalist_jurists sit near the target end: they pay in methodological legitimacy, with constrained rather than trapped exit because conversion to the hadith sciences was costly but possible (many took it, blurring the boundary over generations). shafii_method_jurists are beneficiaries with skin in the game — their credential's scarcity value depends on the gate. abbasid_administration is a low-d beneficiary collecting administrative legibility without administering the standard itself. lay_muslim_communities derive near symmetric: genuine uniform-law benefit against diffuse mediation costs. unauthenticated_report_transmitters stand outside the coordinated set entirely; their exclusion is the enforcement object, not a position within the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this arrangement as pure coordination misses the authority transfer that rode on it; reading it as pure extraction misses the documented fabrication problem it filtered and the unification it achieved across a continental judiciary. The tangled_rope claim holds both facts in one structure. On obsolescence: the founding problems (regional divergence, unreliable reports) remain live in recognizable form, so no mandatrophy is declared — but the trajectory is worth watching. If the report corpus were ever frozen and universally accepted, or fabrication risk collapsed, the gate's coordination value would decay while the certification premium persisted; the slowly rising theater_ratio series is the early indicator to monitor. The classification prevents mislabeling in both directions: it blocks the beneficiary seat's self-description (pure service) and blocks the payer seat's grievance-narrative (pure theft) from becoming the verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the shafii_reading of the usul_al_fiqh_method kernel; how would classification shift under the sibling readings (hanafi, maliki, hanbali), which relocate the beneficiary and victim seats?',
    'Generate the three sibling stories with their own epsilon, beneficiary/victim declarations, and stakeholders; compare computed per-seat classifications across the family.',
    'Under the hanafi reading the extraction direction reverses (reasoning jurists collect, transmission-only specialists lose standing); under the maliki reading Medinan practice-holders join the beneficiaries; under the hanbali reading the reliability threshold tightens further, shrinking the certified-corpus gate and the premiums attached to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Classification is indexed to one reading of a four-reading kernel; sibling files carry the others.').

omega_variable(
    authentication_necessity_vs_gatekeeping,
    'Was the authentication prerequisite proportionate to a real fabrication threat, or did the certification standard exceed what filtering required, converting necessary hygiene into gatekeeping premium?',
    'Compare the volume of demonstrably fabricated reports circulating before the standard with the strictness of the admission criteria the specialists enforced; test whether materially equivalent filtering was achievable under laxer admission rules.',
    'If a laxer filter would have caught the fabricators, the excess strictness is extractive overlay and effective extraction rises; if the threat demanded the strict standard, a larger share of measured extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_necessity_vs_gatekeeping, empirical, 'Whether the certification gate''s strictness tracks the fabrication threat or specialist self-interest.').

omega_variable(
    companion_ijma_restriction_motivation,
    'Does restricting consensus to the Companions'' generation reflect an epistemic argument (that generation''s unique proximity guarantees correctness) or an authority-management move (preventing living jurists from forming consensus that could bypass the textual gate)?',
    'Close reading of the arguments offered in al-Risala and its early commentary tradition against the alternative definitions of consensus proposed by rival schools.',
    'If epistemic, the restriction belongs to the coordination function; if authority-management, it is an extraction-preserving device and the payer seats'' burden is higher than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companion_ijma_restriction_motivation, conceptual, 'Motivation behind the Companions-only consensus rule.').

omega_variable(
    enforcement_channel_composition,
    'Was the arrangement''s enforcement carried mainly by scholarly polemic and reputational sanction, or by state leverage (judicial appointments, patronage) — and did the mix shift over the interval?',
    'Track appointment patterns of chief judges, endowment charters funding method-aligned teaching posts, and the intensity of the polemical literature across the interval.',
    'A rising state share pushes measured suppression upward over time and strengthens the actively-enforced character of the arrangement; a purely scholarly channel caps suppression near reputational limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_channel_composition, empirical, 'Composition of the enforcement machinery behind the source hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 800, 950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__shafii_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(usul_tr_t800, observed).
narrative_ontology:measurement(usul_tr_t820, usul_al_fiqh_method__shafii_reading, theater_ratio, 820, 0.13).
narrative_ontology:measurement_basis(usul_tr_t820, observed).
narrative_ontology:measurement(usul_tr_t840, usul_al_fiqh_method__shafii_reading, theater_ratio, 840, 0.16).
narrative_ontology:measurement_basis(usul_tr_t840, observed).
narrative_ontology:measurement(usul_tr_t860, usul_al_fiqh_method__shafii_reading, theater_ratio, 860, 0.19).
narrative_ontology:measurement_basis(usul_tr_t860, observed).
narrative_ontology:measurement(usul_tr_t890, usul_al_fiqh_method__shafii_reading, theater_ratio, 890, 0.22).
narrative_ontology:measurement_basis(usul_tr_t890, observed).
narrative_ontology:measurement(usul_tr_t920, usul_al_fiqh_method__shafii_reading, theater_ratio, 920, 0.24).
narrative_ontology:measurement_basis(usul_tr_t920, observed).
narrative_ontology:measurement(usul_tr_t950, usul_al_fiqh_method__shafii_reading, theater_ratio, 950, 0.25).
narrative_ontology:measurement_basis(usul_tr_t950, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__shafii_reading, base_extractiveness, 800, 0.34).
narrative_ontology:measurement_basis(usul_be_t800, observed).
narrative_ontology:measurement(usul_be_t820, usul_al_fiqh_method__shafii_reading, base_extractiveness, 820, 0.41).
narrative_ontology:measurement_basis(usul_be_t820, observed).
narrative_ontology:measurement(usul_be_t840, usul_al_fiqh_method__shafii_reading, base_extractiveness, 840, 0.47).
narrative_ontology:measurement_basis(usul_be_t840, observed).
narrative_ontology:measurement(usul_be_t860, usul_al_fiqh_method__shafii_reading, base_extractiveness, 860, 0.51).
narrative_ontology:measurement_basis(usul_be_t860, observed).
narrative_ontology:measurement(usul_be_t890, usul_al_fiqh_method__shafii_reading, base_extractiveness, 890, 0.54).
narrative_ontology:measurement_basis(usul_be_t890, observed).
narrative_ontology:measurement(usul_be_t920, usul_al_fiqh_method__shafii_reading, base_extractiveness, 920, 0.56).
narrative_ontology:measurement_basis(usul_be_t920, observed).
narrative_ontology:measurement(usul_be_t950, usul_al_fiqh_method__shafii_reading, base_extractiveness, 950, 0.58).
narrative_ontology:measurement_basis(usul_be_t950, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__shafii_reading, suppression_requirement, 800, 0.32).
narrative_ontology:measurement_basis(usul_su_t800, observed).
narrative_ontology:measurement(usul_su_t820, usul_al_fiqh_method__shafii_reading, suppression_requirement, 820, 0.4).
narrative_ontology:measurement_basis(usul_su_t820, observed).
narrative_ontology:measurement(usul_su_t840, usul_al_fiqh_method__shafii_reading, suppression_requirement, 840, 0.46).
narrative_ontology:measurement_basis(usul_su_t840, observed).
narrative_ontology:measurement(usul_su_t860, usul_al_fiqh_method__shafii_reading, suppression_requirement, 860, 0.5).
narrative_ontology:measurement_basis(usul_su_t860, observed).
narrative_ontology:measurement(usul_su_t890, usul_al_fiqh_method__shafii_reading, suppression_requirement, 890, 0.53).
narrative_ontology:measurement_basis(usul_su_t890, observed).
narrative_ontology:measurement(usul_su_t920, usul_al_fiqh_method__shafii_reading, suppression_requirement, 920, 0.54).
narrative_ontology:measurement_basis(usul_su_t920, observed).
narrative_ontology:measurement(usul_su_t950, usul_al_fiqh_method__shafii_reading, suppression_requirement, 950, 0.55).
narrative_ontology:measurement_basis(usul_su_t950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'usul al-fiqh' conflates four structurally distinct methodological orders (per the epsilon-invariance principle, one label covering multiple claims means multiple constraints). This family decomposes the label into four reading-stories sharing the kernel: the Shafi'i reading (this file, authentication-gated derivation), the Hanafi reading (reasoning-expanded derivation), the Maliki reading (community-practice-inclusive derivation), and the Hanbali reading (threshold-tightened textual derivation). Each carries its own epsilon, beneficiary/victim structure, and stakeholder set; the upstream Shafi'i systematization influenced the others' operating environment (all later schools argued within an authentication-conscious discourse al-Shafi'i established), which is why this file links outward to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
