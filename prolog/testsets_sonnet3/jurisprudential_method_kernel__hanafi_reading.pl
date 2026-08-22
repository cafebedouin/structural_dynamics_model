% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Reading: Qiyas and Istihsan as Legitimate Extension of Divine Intent
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This story authors the Hanafi reading of the jurisprudential method
 *   kernel: law derives from Qur'an and Hadith but is extended through
 *   structured analogical reasoning (qiyas) and juristic discretion in
 *   service of equity (istihsan), treating human reason as a legitimate
 *   instrument for realizing divine intent in cases the revealed text does
 *   not explicitly cover. This reading grew from the Kufan school's practical
 *   needs in a distant provincial setting and became, through Abbasid state
 *   adoption, the administratively dominant method across a large share of
 *   the historical Islamic world. It is authored here as its own constraint,
 *   not as a stand-in for 'Islamic law' generally — the kernel is contested,
 *   and the sibling readings (Maliki, Shafi'i, Hanbali) authorize different
 *   sources and different degrees of interpretive latitude, producing
 *   different beneficiary/victim structures and different epsilon on novel
 *   cases. This reading's expected structural delta is a comparatively high
 *   epsilon specifically concentrated on novel-case adjudication, where
 *   trained reasoning does the most work and where the gap between rival
 *   jurists' analogical outcomes is largest.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurists: institutional beneficiaries who hold near-exclusive capacity to perform qiyas/istihsan
 *   - hanafi_court_administrators: institutional beneficiaries who operationalize the method across a state court system
 *   - abbasid_imperial_bureaucracy: institutional beneficiary favoring flexible law for imperial administration
 *   - textualist_scholars_of_authenticity: organized payers whose claim to exclusive authenticity is marginalized
 *   - litigants_facing_novel_rulings and unlettered_petitioners_without_juristic_access: powerless payers bearing the outcome variance of discretionary reasoning
 *   - hadith_transmission_specialists: excluded specialists whose authentication work is subordinated to jurist discretion
 *   - later_comparative_jurists: analytical observers tracing the four schools' methodological divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Reading: Qiyas and Istihsan as Legitimate Extension of Divine Intent").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'eaaf2840-b363-4ace-b9cd-e83e46e236dd').
narrative_ontology:cs_kernel_codification('eaaf2840-b363-4ace-b9cd-e83e46e236dd', formalized).
narrative_ontology:cs_authority_grounding('eaaf2840-b363-4ace-b9cd-e83e46e236dd', lineage).
narrative_ontology:cs_interpretation_layer_present('eaaf2840-b363-4ace-b9cd-e83e46e236dd').
narrative_ontology:cs_reading_relation('eaaf2840-b363-4ace-b9cd-e83e46e236dd', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('eaaf2840-b363-4ace-b9cd-e83e46e236dd', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('eaaf2840-b363-4ace-b9cd-e83e46e236dd', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('eaaf2840-b363-4ace-b9cd-e83e46e236dd', foundational, reason_is_legitimate_vehicle_for_divine_intent).
narrative_ontology:cs_axiom_status(reason_is_legitimate_vehicle_for_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('eaaf2840-b363-4ace-b9cd-e83e46e236dd', reason_is_legitimate_vehicle_for_divine_intent, deontological).
narrative_ontology:cs_axiom('eaaf2840-b363-4ace-b9cd-e83e46e236dd', secondary, juristic_preference_may_override_strict_analogy_for_equity).
narrative_ontology:cs_axiom_status(juristic_preference_may_override_strict_analogy_for_equity, holdable).
narrative_ontology:cs_axiom_grounding('eaaf2840-b363-4ace-b9cd-e83e46e236dd', juristic_preference_may_override_strict_analogy_for_equity, instrumental).
narrative_ontology:cs_reference_frame('eaaf2840-b363-4ace-b9cd-e83e46e236dd', kufan_provincial_juristic_practice).
narrative_ontology:cs_drift_state('eaaf2840-b363-4ace-b9cd-e83e46e236dd', post_shafii_methodological_standardization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eaaf2840-b363-4ace-b9cd-e83e46e236dd', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_court_administrators).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, abbasid_imperial_bureaucracy).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars_of_authenticity).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, litigants_facing_novel_rulings).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, unlettered_petitioners_without_juristic_access).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_as_legitimate_extension_of_divine_intent).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, juristic_discretion_serves_public_welfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained for years in qiyas and istihsan methodology, they hold near-exclusive capacity to extend rulings to novel cases arising from trade, administration, and urban life across a vast and diverse empire. Their specialized reasoning skill is what the school's authority is built on; they administer courts, issue fatwas, and train successors, capturing prestige, judicial appointments, and patronage that a purely textualist method could not generate because it cannot answer questions the text does not address.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, agenda_setter).

% Operate the qadi courts across Abbasid and later Ottoman territories using Hanafi method as the administrative default. Qiyas and istihsan give them a mechanism to resolve disputes text does not directly cover, letting the state's legal apparatus function continuously rather than stalling at every unprecedented case.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_court_administrators, beneficiary,
    institutional, generational, mobile, continental).

% Adopted Hanafi method as state-favored school partly because its flexibility suits governing a multi-ethnic empire with commercial, agricultural, and administrative questions unaddressed by seventh-century Arabian text. Benefits from a legal system that can be extended by trained jurists rather than one that freezes at the boundary of explicit revelation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, abbasid_imperial_bureaucracy, beneficiary,
    institutional, generational, arbitrage, continental).

% Hold that only what is explicitly transmitted from the Prophet and his Companions carries certain authority; every act of qiyas or istihsan is, in their view, a human insertion into a divine chain that dilutes the claim that the law is purely God's. They cannot exit the shared religious and legal field they contest within, and Hanafi dominance in imperial courts marginalizes their claim to exclusive textual authenticity, costing them institutional standing and interpretive authority even where their transmission chains are stronger.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars_of_authenticity, payer,
    organized, civilizational, constrained, continental).

% Bring disputes — commercial, marital, property — that fall outside explicit textual coverage. Their outcomes depend on a jurist's analogical reasoning or discretionary preference, which can vary by school, by individual jurist, and by which precedent is chosen as the base case for analogy. They have no capacity to contest the reasoning method itself; they can only accept the ruling issued under it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, litigants_facing_novel_rulings, payer,
    powerless, immediate, trapped, local).

% Lack the education to evaluate whether a given qiyas is sound or whether istihsan has been invoked to reach a preferred outcome rather than a textually compelled one. They must accept the jurist's specialized reasoning on faith, paying an epistemic cost that mirrors the economic and social costs of losing a case they cannot independently assess.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, unlettered_petitioners_without_juristic_access, payer,
    powerless, immediate, trapped, local).

% Specialists in isnad criticism and hadith authentication whose work would, in a stricter methodological hierarchy, constrain or override analogical reasoning. Under Hanafi method their painstaking authentication work is one input among several rather than the controlling arbiter, diminishing their institutional leverage relative to the jurists who apply qiyas and istihsan.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hadith_transmission_specialists, excluded,
    organized, civilizational, constrained, continental).

% Scholars across centuries who compare the four Sunni schools' methodological commitments, tracing how each school's founding premises about the legitimacy of reason, local practice, or strict textual hierarchy produced diverging law on the same source material.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, later_comparative_jurists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable method for extending a fixed body of revealed text and prophetic reports to the enormous range of novel commercial, administrative, and social situations an expanding empire generates, without requiring new revelation or halting legal function at the edge of explicit text.
% TRANSFER_FUNCTION: Moves interpretive authority and its attendant prestige, judicial appointment, and patronage from those whose claim rests purely on textual transmission fidelity toward those trained in analogical and discretionary reasoning; moves practical certainty away from litigants and toward the jurist class empowered to decide what counts as a valid analogy.
% ABSENT_VOICES: Textualist scholars whose entire epistemic project is threatened by treating human reasoning as continuous with revealed authority are present as interlocutors in juristic debate but structurally cannot dislodge the method once it is embedded in imperial court administration. Unlettered petitioners with no legal training have no voice in the debate over method at all.
% DISAPPEARANCE_RATIONALE: If qiyas and istihsan were withdrawn as legitimate tools overnight, Hanafi courts would lose their primary mechanism for resolving the large fraction of cases unaddressed by explicit text; rulings on commercial contracts, novel property arrangements, and administrative disputes would either stall or require wholesale adoption of a rival school's method — the entire apparatus of Hanafi-administered law across its historical territories depends on this reasoning method remaining authoritative.
% FOUNDING_PROBLEM: Early Islamic communities, especially in Kufa and other garrison cities far from Medina, faced legal questions — commercial transactions, administrative disputes, novel social arrangements under expanding conquest — that neither Qur'an nor the available hadith corpus addressed explicitly, and waiting for unanimous consensus or a Medinan analogue was not always practically available to distant provincial courts.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the founding problem as ongoing: novel cases continually arise that revealed text cannot enumerate in advance. Textualist and Hanbali critics, from outside the beneficiary group, corroborate that the original problem (absence of textual guidance in distant provinces) was real but argue the solution has since become a vehicle for jurist discretion untethered from what they consider sufficiently rigorous textual constraint — they attest the problem's originating shape while disputing that the current method still tracks it faithfully.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58) and concentrated on novel-case adjudication rather than on settled textual matters, where this reading tracks the sibling readings closely. Suppression is moderate (0.42): the method does not physically coerce compliance, but imperial court adoption structurally forecloses rival methodological claims from equal administrative standing. Theater ratio is kept low-moderate (0.28) because the coordination function (resolving genuinely novel disputes) is real and substantial, not merely performative — the method solves an actual administrative problem an empire faced. Accessibility collapse is authored at 0.5: alternatives (stricter textualism, Medinan practice) remained genuinely live throughout history, so collapse is partial, not near-total. Resistance is authored moderate-high (0.55) reflecting sustained textualist and Hanbali critique across centuries.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist-trained jurists and court administrators sit near the beneficiary end: their specialized skill is exactly what the method rewards, and imperial patronage flows to them. Textualist scholars sit near the target end: the more qiyas/istihsan is treated as legitimate, the more their claim to sole custodianship of authentic transmission is structurally devalued, even though they cannot exit the shared religious-legal field they contest within. Litigants and unlettered petitioners are powerless payers with trapped exit options — they bear the practical variance of discretionary reasoning without capacity to contest the method.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — provincial courts lacking explicit textual guidance for novel cases — was real at founding and Hanafi jurists still attest it as live (empire-scale legal systems continually generate unprecedented cases). But the method's persistence as an imperially favored default, centuries after its founding emergency, is corroborated as contested by outside critics who argue the solution has drifted from emergency interpretive necessity into an entrenched apparatus of jurist discretion and institutional prestige. This is not a resolved mandatrophy — status is authored 'contested' rather than 'dead' precisely because the coordination function (novel-case resolution) remains genuinely needed, distinguishing this from a pure zombie institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanafi_reading_vs_kernel_contest,
    'Is the legitimacy of qiyas/istihsan as tools for extending divine intent a settled methodological fact within Islamic jurisprudence, or is it one contested reading among several live, mutually irreconcilable positions on how law may be derived from revealed sources?',
    'There is no external empirical resolution available; the question is intrinsically a matter of contested legal theology. The corpus models this by treating each school''s position as a separate constraint (hanafi_reading, maliki_reading, shafii_reading, hanbali_reading) linked by network edges rather than adjudicating a single truth value.',
    'If treated as settled fact, this reading''s high epsilon on novel-case reasoning would appear as the single correct account of Islamic law''s operation; treated as one contested reading among several, the same epsilon is instead a structural fact about this particular methodological commitment, coexisting with siblings that produce materially different beneficiary/victim structures on the same source material.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_reading_vs_kernel_contest, conceptual, 'Whether the Hanafi methodological stance is a settled fact or one reading among contested siblings within the jurisprudential method kernel.').

omega_variable(
    qiyas_soundness_verification,
    'Can the soundness of a given analogical extension (qiyas) or discretionary preference (istihsan) be verified by criteria external to the trained jurist class that performs it, or is soundness ultimately self-certifying within the guild of rationalist-trained jurists?',
    'Historical and comparative jurisprudential analysis of documented disputes over specific qiyas rulings — cases where later jurists overturned earlier analogies — would show whether external verification criteria (textual fidelity, outcome consistency, cross-school convergence) meaningfully constrain jurist discretion or whether disagreement is resolved purely by institutional authority.',
    'If soundness is largely self-certifying within the jurist guild, the beneficiary concentration (rationalist_trained_jurists) is more structurally significant and the extraction from powerless litigants is less constrained by any external check; if genuinely externally verifiable, extraction is correspondingly lower and the method''s coordination function is stronger relative to its extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_soundness_verification, empirical, 'Whether qiyas/istihsan soundness is externally checkable or self-certifying within the beneficiary jurist class.').

omega_variable(
    istihsan_discretion_boundary,
    'Where does legitimate juristic preference (istihsan) for equity end and unconstrained judicial discretion — indistinguishable from arbitrary preference — begin?',
    'Comparative analysis of istihsan''s invocation across documented Hanafi rulings against the stated criteria (avoiding hardship, serving clear public welfare) versus outcomes that appear to primarily serve jurist or state administrative convenience.',
    'A narrow, well-policed boundary supports the coordination reading (istihsan solves genuine equity problems text cannot anticipate); a porous boundary supports a higher-extraction reading in which istihsan functions as a discretionary override mechanism benefiting whichever outcome the jurist or the state prefers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_discretion_boundary, conceptual, 'Whether istihsan has a principled boundary or functions as unconstrained discretion in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(juri_tr_t0, projected).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(juri_tr_t200, projected).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(juri_tr_t400, projected).
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 700, 0.25).
narrative_ontology:measurement_basis(juri_tr_t700, projected).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1000, 0.27).
narrative_ontology:measurement_basis(juri_tr_t1000, projected).
narrative_ontology:measurement(juri_tr_t1300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1300, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1300, projected).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(juri_be_t0, projected).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement_basis(juri_be_t200, projected).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement_basis(juri_be_t400, projected).
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 700, 0.55).
narrative_ontology:measurement_basis(juri_be_t700, projected).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1000, 0.57).
narrative_ontology:measurement_basis(juri_be_t1000, projected).
narrative_ontology:measurement(juri_be_t1300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1300, 0.58).
narrative_ontology:measurement_basis(juri_be_t1300, projected).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(juri_su_t0, projected).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 200, 0.34).
narrative_ontology:measurement_basis(juri_su_t200, projected).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 400, 0.38).
narrative_ontology:measurement_basis(juri_su_t400, projected).
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement_basis(juri_su_t700, projected).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1000, 0.41).
narrative_ontology:measurement_basis(juri_su_t1000, projected).
narrative_ontology:measurement(juri_su_t1300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1300, 0.42).
narrative_ontology:measurement_basis(juri_su_t1300, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'Islamic jurisprudential method' into structurally distinct readings of a single contested kernel (jurisprudential_method_kernel). Each reading authors a different source hierarchy and a different scope for human reasoning, producing different epsilon values, different beneficiary/victim structures, and different classifications. The Hanafi reading here is authored as tangled_rope (genuine novel-case coordination function plus asymmetric extraction concentrated on the jurist-trained beneficiary class); other readings may compute differently given their different structural commitments. All four are linked bidirectionally via affects_constraints so contamination/coupling analysis can trace how developments in one reading's legitimacy pressure the others (e.g., a rise in textualist critique of qiyas pressures both the hanafi_reading and, differently, the shafii_reading's more constrained qiyas usage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
