% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Harm-Balancing Speech Restriction Framework
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   The harm-balancing reading of the speech-harm boundary kernel
 *   instantiates a constitutional framework where speech enjoys a strong
 *   presumptive protection that yields only when a specific, demonstrated
 *   harm is proven and the restriction survives proportionality review. This
 *   is the dominant framework in Canadian, European, and many Commonwealth
 *   jurisdictions, and increasingly influential in international human rights
 *   law. The constraint is claimed as tangled_rope: it performs genuine
 *   coordination (mediating speech/harm conflicts through law rather than
 *   violence) but extracts asymmetrically (speakers bear restriction costs,
 *   protected groups collect protection benefits). The structural delta from
 *   the kernel's other readings is broader unprotected categories (hate
 *   speech, group libel, harassment recognized as restrictable) and moderate
 *   extraction concentrated on speakers when harm is demonstrated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Harm-Balancing Speech Restriction Framework").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '8c2b762d-d51a-49e5-a947-5e2115dad772').
narrative_ontology:cs_kernel_codification('8c2b762d-d51a-49e5-a947-5e2115dad772', formalized).
narrative_ontology:cs_authority_grounding('8c2b762d-d51a-49e5-a947-5e2115dad772', lineage).
narrative_ontology:cs_interpretation_layer_present('8c2b762d-d51a-49e5-a947-5e2115dad772').
narrative_ontology:cs_reading_relation('8c2b762d-d51a-49e5-a947-5e2115dad772', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c2b762d-d51a-49e5-a947-5e2115dad772', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('8c2b762d-d51a-49e5-a947-5e2115dad772', foundational, harm_demonstration_required).
narrative_ontology:cs_axiom_status(harm_demonstration_required, holdable).
narrative_ontology:cs_axiom_grounding('8c2b762d-d51a-49e5-a947-5e2115dad772', harm_demonstration_required, empirically_contingent).
narrative_ontology:cs_axiom('8c2b762d-d51a-49e5-a947-5e2115dad772', foundational, proportionality_balancing).
narrative_ontology:cs_axiom_status(proportionality_balancing, holdable).
narrative_ontology:cs_axiom_grounding('8c2b762d-d51a-49e5-a947-5e2115dad772', proportionality_balancing, conventional).
narrative_ontology:cs_axiom('8c2b762d-d51a-49e5-a947-5e2115dad772', secondary, speech_presumption).
narrative_ontology:cs_axiom_status(speech_presumption, holdable).
narrative_ontology:cs_axiom_grounding('8c2b762d-d51a-49e5-a947-5e2115dad772', speech_presumption, conventional).
narrative_ontology:cs_reference_frame('8c2b762d-d51a-49e5-a947-5e2115dad772', postwar_constitutional_compromise).
narrative_ontology:cs_drift_state('8c2b762d-d51a-49e5-a947-5e2115dad772', digital_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c2b762d-d51a-49e5-a947-5e2115dad772', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, protected_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_restricted).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, controversial_publishers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, academic_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, legislatures).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, harassment_targets).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, harm_prevention_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, demonstrated_harm_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically targeted by hate speech, group libel, and harassment (racial minorities, religious minorities, LGBTQ+ communities, women). They benefit from legal restrictions that suppress speech deemed to cause demonstrable harm. Their exit options are constrained — they cannot individually opt out of the social environment where such speech circulates, but they can organize politically to strengthen protections.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, protected_groups, beneficiary,
    organized, generational, constrained, national).

% Individuals disproportionately affected by targeted harassment, doxxing, and coordinated abuse campaigns (journalists, activists, public figures from marginalized backgrounds). They gain protection from the balancing test when harm is demonstrated. Their exit from the constraint's effects is constrained — they cannot avoid being targets without withdrawing from public life.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_populations, beneficiary,
    moderate, biographical, constrained, national).

% Individuals experiencing ongoing, personally targeted harassment that meets the demonstrated-harm threshold. They benefit from takedown orders and restraining provisions. They also bear costs — the legal process to prove harm is burdensome, and the publicity of proceedings can amplify the harassment. Exit is nearly impossible: they are already in the harm's grip.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harassment_targets, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, harassment_targets, payer).

% Speakers whose expression is restricted after a court or tribunal finds demonstrated harm and applies proportionality balancing. This includes political provocateurs, satirists, academics studying extremism, and ordinary citizens whose speech crosses the evolving line. They bear the cost of self-censorship, legal defense, and lost platforms. Exit is constrained — they can speak in other jurisdictions or private settings, but the national public sphere is partially closed to them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_restricted, payer,
    moderate, biographical, constrained, national).

% Media outlets and platforms that publish edge-case content (investigative journalism on hate groups, philosophical debates on identity, satire targeting protected groups). They face takedown demands, fines, and regulatory pressure. Their power and global reach give them mobile exit options — they can relocate servers, use decentralized distribution, or geofence content — but compliance costs are significant.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, controversial_publishers, payer,
    powerful, biographical, mobile, global).

% Scholars studying extremism, propaganda, hate speech, and historical atrocities who must quote or analyze restricted content. They are payers when their work is flagged or restricted; they are beneficiaries when the framework protects them from harassment. Exit is constrained — academic freedom norms provide some shelter, but institutional risk-aversion chills research.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, academic_researchers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, academic_researchers, beneficiary).

% High courts and constitutional tribunals that articulate and apply the proportionality balancing test. They set the agenda by defining what counts as 'demonstrated harm,' how the presumption of protection is rebutted, and what proportionality requires in each case. They are not directly subject to the speech restrictions they adjudicate; their exit is analytical — they observe the constraint's operation from the adjudicative seat.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Legislative bodies that enact hate-speech laws, harassment statutes, and platform-regulation frameworks implementing the harm-balancing approach. They benefit politically from responding to constituent demands for protection. They have arbitrage-grade exit — they can amend, repeal, or replace the legal framework, and they control the legislative calendar.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislatures, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, legislatures, beneficiary).

% Civil-liberties organizations and legal scholars who argue for near-absolute speech protection (the absolutist_reading position). They are structurally excluded from the balancing framework's internal logic — the framework presupposes that harm can justify restriction, which their position denies. They cannot 'exit' the constraint's effects because the legal system operates on the balancing premise; they can only litigate to narrow its application.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_advocates, excluded,
    organized, generational, trapped, national).

% Human-rights organizations and theorists who argue speech protection is subordinate to human dignity (the dignity_reading position). They view the harm-balancing framework as insufficiently protective — it requires individualized harm demonstration rather than recognizing categorical dignity violations. Like absolutist advocates, they are excluded from the framework's operating logic and trapped within a system that does not adopt their premise.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, dignity_advocates, excluded,
    organized, generational, trapped, national).

% Academic observers who analyze the balancing test's doctrinal evolution, empirical effects, and theoretical coherence across jurisdictions. They neither collect rents nor bear the restriction costs directly. Their exit is analytical — they can study other frameworks, other kernels, or withdraw from the discourse entirely.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured legal mechanism to resolve conflicts between speech and protection from harm: a presumption favoring speech that can be overcome only by demonstrated, specific harm assessed through proportionality balancing. This coordinates the pluralistic society's need for both open discourse and protection of vulnerable members without either value categorically trumping the other.
% TRANSFER_FUNCTION: Transfers speech opportunities from speakers whose expression is found to cause demonstrable harm (after proportionality balancing) to the protected groups and individuals who would otherwise suffer that harm. The transfer is not monetary but capacitive — the constraint reallocates the 'space to speak' and the 'space to be free from targeted harm.'
% ABSENT_VOICES: Future generations who will inherit the precedential landscape shaped by today's balancing decisions; speakers in jurisdictions without harm-balancing frameworks who cannot participate in the doctrinal conversation; and the 'chilled speakers' — those who self-censor below the litigation threshold and therefore never appear in the case law. The excluded stakeholders (absolutist_advocates, dignity_advocates) are present in the discourse but excluded from the framework's internal logic.
% DISAPPEARANCE_RATIONALE: If the harm-balancing framework vanished overnight, jurisdictions would revert to either near-absolute speech protection (absolutist default) or categorical dignity-based restrictions (dignity default), or legislative majorities would enact ad hoc speech restrictions without proportionality discipline. The legal architecture for calibrating speech vs. harm would disappear, causing immediate doctrinal chaos and a scramble to replace the balancing test with something cruder.
% FOUNDING_PROBLEM: Post-war constitutional orders needed to reconcile two commitments: (1) the liberal-democratic premise that free speech is essential to self-government and truth-discovery, and (2) the historical lesson that unchecked propaganda and hate speech can enable authoritarianism and genocide. The harm-balancing framework was built to avoid both the Weimar failure (too little restriction) and the Soviet model (too much restriction).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the constitutional framers' records (e.g., German Basic Law drafters, Canadian Charter architects, European Convention preparatory works) — sources outside the current beneficiary set. However, contemporary beneficiaries (protected_groups, vulnerable_populations) argue the founding problem is live and intensifying (digital amplification of hate). Absolutist_advocates argue the founding problem was misdiagnosed — the Weimar lesson is a categorical error. Dignity_advocates argue the founding problem was incompletely solved — dignity, not harm-balancing, is the proper foundation. No single corroborator speaks for all parties.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate because the constraint reallocates speech opportunities from a defined set of speakers to protected beneficiaries, but the presumption of protection and proportionality requirement limit the transfer's scope. Suppression (0.48) reflects active enforcement through courts, tribunals, and platform regulation — the constraint does not self-enforce. Theater ratio (0.28) is low-moderate: the balancing test is a real doctrinal structure with genuine case-by-case application, but a growing share of enforcement targets low-value speech (memes, satire, academic quotation) where the harm demonstration is thin, suggesting some performative expansion. Accessibility collapse (0.58) is moderate: speakers can often reframe or relocate, but the national public sphere is partially closed. Resistance (0.45) is moderate: speakers litigate, platforms resist, and political movements contest the framework's expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the protected_groups seat, the constraint is a rope — it coordinates a pluralistic society's competing claims with minimal coercion. From the speakers_restricted seat, it is a snare — the 'demonstrated harm' threshold drifts downward, proportionality becomes a rubber stamp, and the constraint extracts speech opportunities without accountable limitation. From the constitutional_courts seat, it is a scaffold — a transitional framework meant to stabilize democratic discourse until social norms internalize mutual respect (but the sunset never arrives). The engine computes these divergent seat classifications from the single structural dataset.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected groups, vulnerable populations, and harassment targets are structural beneficiaries (d near 0.0-0.2): the constraint subsidizes their protection. Speakers_restricted, controversial_publishers, and academic_researchers are structural payers (d near 0.7-0.9): they bear the restriction costs. Constitutional courts and legislatures are agenda_setters (d near 0.5 analytically, but they administer the constraint). Absolutist and dignity advocates are excluded (d not computable — they reject the framework's premise). The engine derives directionality from these structural positions plus exit options: harassment_targets are trapped (d→1.0), controversial_publishers are mobile (d→0.5), academic_researchers are constrained (d→0.7).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling free speech with protection from hate-fueled authoritarianism) is contested: some say it's live (digital hate amplifies the threat), some say it's dead (Weimar analogy is inapposite), some say it's solved but the framework persists as rent-collection for censorious interests. The mandate has not atrophied into pure inertia (piton) because the balancing test still decides live cases daily, but the theater_ratio creep suggests coordination function is degrading into extraction maintenance. The constraint is not a false summit (mountain) — it is openly a human-made legal framework, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the harm-balancing reading a genuine coordination mechanism for pluralistic societies, or an extraction mechanism that expands restrictable speech categories under the cover of balancing?',
    'Longitudinal study of restriction outcomes: track whether proportionality balancing actually narrows over time (coordination) or expands categories of restricted speech without corresponding harm reduction (extraction). Compare jurisdictions with harm-balancing vs. absolutist vs. dignity frameworks on speech-chilling metrics and harm incidence.',
    'If extraction-dominant, the constraint reclassifies toward snare; if coordination-dominant, it remains tangled_rope or trends toward rope. The kernel''s other readings would gain/lose structural legitimacy accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the harm-balancing reading''s structural function is coordination or extraction').

omega_variable(
    harm_demonstration_operationalization,
    'What counts as ''demonstrated harm'' in practice, and does the operational threshold drift downward over time?',
    'Content analysis of judicial opinions over the interval: code the evidentiary standard for harm demonstration, the types of harm recognized, and the proportionality analysis rigor. Test for systematic drift toward lower thresholds.',
    'Downward drift would increase extractiveness and suppression, pushing classification toward snare. Stable or rising thresholds would support the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstration_operationalization, empirical, 'Whether the harm threshold is stable or drifting').

omega_variable(
    proportionality_balancing_workability,
    'Is proportionality balancing a cognitively tractable judicial methodology, or does it inevitably collapse into judicial preference?',
    'Inter-judge reliability studies: present identical fact patterns to multiple judges and measure variance in balancing outcomes. High variance suggests the test is not a constraint but a delegation.',
    'If balancing collapses to preference, the constraint''s coordination function is illusory — it is a snare with a coordination veneer. If reliable, the coordination claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_balancing_workability, conceptual, 'Whether the core methodological tool of this reading is structurally coherent').

omega_variable(
    cs_framing_ambiguity,
    'Is the kernel best framed as a constitutional text (formalized), a dignity principle (distributed), or a legislative balancing practice (implicit)?',
    'Comparative constitutional analysis: trace whether the harm-balancing reading''s authority derives from textual interpretation (formalized), from a freestanding dignity principle (distributed), or from legislative enactment practice (implicit). The framing changes which authority_grounding and interpretation_layer_present values are warranted.',
    'Different framings yield different cs_structure classifications and different drift_state assessments. A formalized framing with lineage authority makes axiom_overriding drift structurally significant; a distributed framing makes practice_drift the primary vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_ambiguity, conceptual, 'Whether the kernel''s codification and authority structure are as declared').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t1950, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t1970, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t1990, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t2005, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t2015, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t2025, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t1950, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t1970, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t1990, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t2005, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t2015, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t2025, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t1950, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t1970, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t1990, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t2005, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t2015, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t2025, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.1).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, platform_content_moderation_regimes).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, hate_speech_legislation_family).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the speech_harm_boundary kernel. The absolutist_reading (near-absolute protection) and dignity_reading (categorical dignity-based restriction) are sibling constraints. All three share the kernel commitment that speech-harm relations are legally regulable but differ on the structural form of regulation. This reading's ε (0.52) is moderate — higher than the absolutist_reading's (near 0) but lower than the dignity_reading's (higher, because categorical restriction extracts more). The network edges reflect doctrinal influence: this reading's balancing test shapes platform moderation regimes and hate-speech legislation; the absolutist_reading influences First Amendment jurisdictions; the dignity_reading influences European and Latin American constitutional courts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
