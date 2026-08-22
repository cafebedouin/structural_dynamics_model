% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Balancing Reading of Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the balancing reading of the speech
 *   protection boundary: the view that what speech is constitutionally
 *   protected depends on case-by-case judicial weighing of First Amendment
 *   interests against other constitutional values (equal protection, due
 *   process, personal safety) and demonstrated harms. This reading contrasts
 *   with the absolutist reading (near-absolute protection absent imminent
 *   lawless action) and the harm-limited reading (harm to dignity and
 *   equality justifies suppression). The balancing reading distributes
 *   gatekeeper authority across the judiciary through intermediate-scrutiny
 *   frameworks rather than centralizing it in categorical rules. The reading
 *   is one of three constitutive interpretations of the same contested
 *   kernel—the speech protection boundary itself is not contested; the
 *   reading of what draws it is. The story models this reading alone, not the
 *   contest.
 *
 * KEY AGENTS:
 *   - Judiciary as institutional gatekeeper: conducts case-by-case balancing, sets intermediate-scrutiny standards, distributes authority across courts
 *   - Speakers of disfavored content: bear uncertainty about ex-ante protection, face identity-locked exit, pay through litigation burden and self-censorship
 *   - Targets of systemic harmful speech: gain standing to present harm evidence, depend on judicial acceptance of their framing, benefit from courts weighing harms
 *   - Lower court judges: gain discretionary authority, pay through fact-intensive burden and appellate vulnerability
 *   - Absolutist and harm-limited interpreters: structurally excluded from setting the framework, must operate as advocacy positions within it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.62).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Balancing Reading of Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'aefd2829-32be-4836-a4be-afd6adeed62a').
narrative_ontology:cs_kernel_codification('aefd2829-32be-4836-a4be-afd6adeed62a', formalized).
narrative_ontology:cs_authority_grounding('aefd2829-32be-4836-a4be-afd6adeed62a', lineage).
narrative_ontology:cs_interpretation_layer_present('aefd2829-32be-4836-a4be-afd6adeed62a').
narrative_ontology:cs_reading_relation('aefd2829-32be-4836-a4be-afd6adeed62a', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aefd2829-32be-4836-a4be-afd6adeed62a', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('aefd2829-32be-4836-a4be-afd6adeed62a', foundational, context_determines_protection).
narrative_ontology:cs_axiom_status(context_determines_protection, holdable).
narrative_ontology:cs_axiom_grounding('aefd2829-32be-4836-a4be-afd6adeed62a', context_determines_protection, instrumental).
narrative_ontology:cs_axiom('aefd2829-32be-4836-a4be-afd6adeed62a', foundational, competing_constitutional_values_are_incommensurable).
narrative_ontology:cs_axiom_status(competing_constitutional_values_are_incommensurable, holdable).
narrative_ontology:cs_axiom_grounding('aefd2829-32be-4836-a4be-afd6adeed62a', competing_constitutional_values_are_incommensurable, deontological).
narrative_ontology:cs_reference_frame('aefd2829-32be-4836-a4be-afd6adeed62a', first_amendment_doctrine_via_intermediate_scrutiny).
narrative_ontology:cs_drift_state('aefd2829-32be-4836-a4be-afd6adeed62a', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aefd2829-32be-4836-a4be-afd6adeed62a', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary_as_gatekeeper).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, speech_in_relational_context).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_of_disfavored_content).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, targets_of_systemic_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, targets_of_systemic_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, lower_court_judges).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, lower_court_judges).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, intermediate_scrutiny_framework).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, contextual_harm_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts conduct case-by-case balancing of First Amendment interests against competing constitutional values (equal protection, due process, personal safety) and demonstrated harms. Judges weigh the speaker's expression interest against the magnitude and immediacy of harm to others, the availability of narrower alternatives, and systemic context. The judiciary's role is to adjudicate which speech receives protection through standards that apply intermediate scrutiny to some categories and strict scrutiny to others depending on context and demonstrated harm. This distributes the gatekeeper function across a network of courts rather than centralizing it in a single authority.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary_as_gatekeeper, agenda_setter,
    institutional, generational, analytical, national).

% Face uncertainty about what speech will survive judicial scrutiny because the boundary shifts with factual findings about context and harm. Speakers committed to expressing ideas that courts may find harmful (conspiracy theories, incitement-adjacent critique, identity-targeted rhetoric) cannot know ex ante whether their speech will be protected; they must litigate or self-censor. The balancing framework's flexibility means the same speech may be protected in one jurisdiction and punished in another depending on how courts weigh competing interests. Identity fusion occurs when the speaker's self-conception depends on expressing exactly this disfavored content.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_of_disfavored_content, payer,
    powerless, biographical, identity_locked, national).

% Under the balancing reading, courts are authorized to weigh the harm to targets — dignity violations, equal protection harms, documented patterns of harassment or violence triggered by speech — as a legitimate consideration in determining protection boundaries. Groups repeatedly targeted by disfavored speech can present evidence that the speech creates systemic harm (threat escalation, market exclusion, violence correlation) and ask courts to account for these harms in the protection calculus. This gives targets standing to contest protection claims, though they remain dependent on judges accepting their framing of harm as constitutionally cognizable.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, targets_of_systemic_harmful_speech, beneficiary,
    organized, biographical, constrained, national).

% Gain interpretive authority to develop context-sensitive doctrines that account for demonstrated harms and local conditions, but also face the burden of conducting fact-intensive balancing in every case and risk reversal by appellate courts if their weighting diverges from higher authority. They benefit from the intermediate-scrutiny framework because it allows judicial discretion; they pay through increased litigation burden and appellate vulnerability.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, lower_court_judges, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, lower_court_judges, payer).

% Judges and jurisprudential voices committed to categorical speech protection are structurally sidelined by the balancing reading: their core claim (speech receives near-absolute protection absent imminent lawless action) is treated as one interest among many rather than a governing principle. They cannot exit or retreat to a parallel interpretation regime within the same constitutional system; they must argue their reading is the correct one through appellate channels or legislative action.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_interpreters, excluded,
    institutional, generational, trapped, national).

% Judges and voices committed to prioritizing harm prevention over speech protection are also structurally positioned outside the balancing framework: their core claim (speech should yield when significant dignity or equality harms are demonstrated) is not adopted as governing doctrine. Like absolutist voices, they must operate within the balancing framework as one advocacy position rather than as controlling authority.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, harm_limited_interpreters, excluded,
    institutional, generational, trapped, national).

% Processes all speech protection disputes through the balancing framework, generating case law that iteratively refines what contexts warrant what degrees of scrutiny and how to assess harm. The system itself functions as a measuring and distributing mechanism for speech protection boundaries.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, litigation_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, judiciary_as_gatekeeper).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of drawing constitutional boundaries between protected and unprotected speech in a heterogeneous polity where different communities experience speech harm differently. Rather than applying a single categorical rule (speech is protected / speech is not protected), the balancing reading coordinates judicial authority around a principle: weigh the speaker's expression interest against competing constitutional values and demonstrated harms on a case-by-case basis.
% TRANSFER_FUNCTION: Moves interpretive authority from categorical rules toward discretionary judicial balancing. Speakers lose the certainty of bright-line protection in exchange for courts taking harm to others into account. Targets of harmful speech gain standing to present evidence of harm and request judicial consideration of systemic consequences, but remain dependent on judges accepting their framing. Appellate courts retain power to overturn lower courts' balancing judgments.
% ABSENT_VOICES: Voices committed to absolutist speech protection (Brandenburg standard) and voices committed to harm-prevention-first readings (dignity and equality harms as governing) are structurally excluded from setting the framework even though they would argue for different boundaries. They can litigate individual cases and advocate for doctrinal change but cannot operate a parallel interpretation regime within the same constitutional system. Legal realists, free-speech absolutists, and radical equality advocates would all object to the exclusion of their frameworks from governing doctrine.
% DISAPPEARANCE_RATIONALE: If the balancing reading disappeared and either the absolutist or harm-limited reading took its place, the set of protected and unprotected speech would shift significantly. Under an absolutist reading, much speech the balancing reading subjects to intermediate scrutiny would gain near-absolute protection. Under a harm-limited reading, much speech the balancing reading protects would be unprotected because demonstrated harm to dignity or equality would suffice to justify suppression. The boundary itself—what speakers can do and what listeners are protected from—would reorganize, and litigation patterns would change as different strategic victories became available.
% FOUNDING_PROBLEM: Early First Amendment doctrine applied categorical rules (clear and present danger, then Brandenburg) that either protected nearly all speech (absolutist) or protected speech with no systematic way to account for context-specific harms. By the mid-20th century, courts faced cases where categorical rules seemed inadequate: speech that did not meet Brandenburg's 'imminent lawless action' standard but caused documented systemic harm (cross-burning, harassment campaigns, conspiracy-theory driven violence) or speech that occurred in specialized contexts (schools, workplaces, broadcast media) where harms seemed more immediate. The balancing reading emerged as a framework that could weight multiple constitutional interests rather than apply a single rule.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary itself attests the founding problem through case law beginning with intermediate-scrutiny doctrines (O'Brien, Turner, Ginsberg) and extending through cases like Morse v. Frederick and subsequent decisions. The balancing reading's supporters point to this doctrinal history as evidence the categorical rules proved insufficient. Absolutist interpreters attest the founding problem is *mischaracterized*—they argue that Brandenburg was never applied properly and that courts inventing new categories (hate speech, incitement-adjacent rhetoric) proved the rule inadequate rather than proving the need for balancing. Harm-limited interpreters attest that balancing itself has proven inadequate because courts systematically weigh speaker interests more heavily than target harms. Independent constitutional scholars (across the ideological spectrum) acknowledge the founding problem and dispute whether balancing is the right solution.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the balancing framework systematically privileges judicial discretion over speaker certainty and because speakers cannot know ex ante what will be protected. Extractiveness initially low (0.42) reflects that the framework began as a coordination solution to categorical-rule inadequacy, but accumulates over time (peaking at 0.63 by t=32) as courts develop intermediate-scrutiny doctrines that effectively narrow protection for categories of speech (coded speech, context-dependent rhetoric, systemic-coordination speech). The partial decline at t=40 reflects judicial recognition that over-extension of balancing can become pretextual suppression, prompting some push-back and doctrinal boundary-setting. Suppression is high (0.62 at interval end) because the framework requires active judicial gatekeeping and fact-finding to maintain the boundary; speakers must litigate or self-censor because the rule is not bright-line. Theater ratio rises then plateaus (peaking at 0.43) because much judicial balancing activity becomes ritualistic—courts perform detailed harm assessment while outcomes remain predictable along ideological lines, suggesting the balancing function is increasingly performative rather than genuinely open-ended. Accessibility collapse is moderate (0.48) because the balancing framework leaves alternatives open in theory (a speaker can argue different weights, present new harm evidence, seek appellate review) but collapses them in practice as courts become entrenched in particular weighing patterns. Resistance is high (0.71) because speakers and absolutist interpreters actively contest the balancing framework's legitimacy, and targets dispute whether courts genuinely weigh their harms or merely perform it. The measurements show extractiveness and suppression increasing over the interval, theater rising, then plateauing—consistent with a tangled_rope framework that began as genuine coordination (balancing did solve the categorical-rule problem) but accumulated extractive overlay as institutional patterns entrenched and substituted performance for genuine weighing.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, the balancing reading is a coordination solution: they needed a framework that could account for context and harm rather than apply rigid rules. They do not experience themselves as extracting from speakers; they experience themselves as adjudicating competing rights. From speakers' perspective, especially those whose content is disfavored by the particular judges hearing their case, the balancing framework is a suppression mechanism: it looks like they must either conform to judicial preferences or litigate at massive cost, knowing that even winning at trial risks appellate reversal. From targets' perspective, the balancing framework sometimes protects them (courts weigh harm as a legitimate interest) but often fails them (courts weigh speaker interest more heavily even when harm is demonstrated). The engine should compute different d values for these seats: judiciary and targets closer to beneficiary end (d toward 0), speakers and absolutist interpreters closer to target end (d toward 1). Lower court judges sit near symmetric: they gain gatekeeper authority (beneficiary) but lose certainty and face appellate vulnerability (cost).
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary_as_gatekeeper derives d from its institutional power (powerful), analytical exit (no real exit; the institution *is* the constraint), and beneficiary role (sets the framework, conducts the weighing). The engine should derive d near 0.15-0.25 (low-to-moderate beneficiary). Speakers_of_disfavored_content derive d from powerless status, identity_locked exit (the speaker's self-conception depends on speaking this way), and payer role (bears the cost of uncertain protection). The engine should derive d near 0.75-0.85 (high target). Targets_of_systemic_harmful_speech derive d from organized power, constrained exit (they are already present and harmed), and beneficiary role (the framework authorizes courts to weigh their harms). The engine should derive d near 0.25-0.35 (low-to-moderate beneficiary, tempered by dependency on judicial acceptance). Lower court judges derive d from institutional power, constrained exit (must operate within appellate authority), and dual role (beneficiary and payer). The engine should derive d near 0.40-0.50 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing reading avoids both mandatrophy and false-benefit labeling by maintaining a genuinely dual structure: it coordinates around a principle (weighing competing interests) while simultaneously extracting from speakers by requiring them to litigate or self-censor to know their protection status. The founding problem (categorical rules proved inadequate for context-sensitive boundary-drawing) remains live—courts still face hard cases where the boundary is genuinely unclear. The framework's justification is not purely historical (it was built to solve a real problem) and not purely perpetual (each new speech case tests whether the framework still solves the problem or has become pretextual). The measurement series suggests the framework may be approaching mandatrophy: theater_ratio rises and plateaus, suggesting courts are increasingly performing the balancing ritual rather than genuinely re-weighing interests each case. If theater ratio continues to rise and extraction plateaus, the constraint will exhibit mandatrophy markers—the founding problem solved, but the framework persists for institutional/career reasons. The framework is not yet mandatrophic (t=40 extraction is 0.58, theater is 0.41—still shows functional extraction, not pure theater), but trajectory suggests future mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_framing,
    'Is the ''speech protection boundary'' better understood as a single contested kernel with three readings (absolutist/balancing/harm-limited), or as three independent constraint categories that happen to share subject matter?',
    'Examine whether all three readings operate on the same constitutional authority (the First Amendment and its competing values). If all three ground their legitimacy claims in the same foundational text and the same interpretive tradition, the kernel framing is appropriate; if they invoke separate authorities or incompatible foundational narratives, they are independent constraints.',
    'If the kernel framing holds, this constraint is one reading of a contested commitment; if independent, the constraint should be authored without kernel structure. The framing affects how the corpus models doctrinal contestation vs. standalone constraint diversity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_framing, conceptual, 'Whether the three readings share a kernel or are independent constraints').

omega_variable(
    balancing_vs_pretextual_suppression,
    'Does judicial balancing in practice genuinely re-weigh competing interests case-by-case, or has it become a ritualized cover for outcome-predetermined suppression?',
    'Examine outcome patterns: if balancing produces variable winners depending on factual variation (some speakers protected, others not, in ways that track the facts presented), the framework is genuine. If outcomes are predetermined by judicial ideology with balancing functioning as post-hoc justification regardless of facts, balancing has become pretextual.',
    'If pretextual, the constraint should be reclassified from tangled_rope (genuine coordination + extraction) to snare (extraction covered by coordination narrative). The measurement series showing rising theater_ratio suggests increasing pretextual character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_vs_pretextual_suppression, empirical, 'Whether case-by-case balancing is genuine or ritualized/predetermined').

omega_variable(
    speaker_identity_lock_mechanism,
    'What portion of the measured suppression on speakers stems from structural barriers (legal risk, litigation cost) versus internalized identity fusion (speaker views their identity as inseparable from speaking disfavored content)?',
    'Observe behavior post-threat: speakers who face legal risk but lack identity fusion will exit or self-censor when threatened; speakers with identity fusion will often continue despite risk. Post-suppression removal (if a speaker is silenced and suppression reversed), identity-fused speakers often continue patterns of thought even without behavioral outlet, while non-fused speakers move on.',
    'If primarily structural, exit after threat removal is plausible and suppression could be reversed by changing legal regime; if primarily internalized, the speaker carries suppression with them and regime change provides less relief. Identity lock amplifies effective suppression because the target cannot easily leave.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_identity_lock_mechanism, empirical, 'Structural vs. internalized suppression on identity-locked speakers').

omega_variable(
    target_harm_measurement_asymmetry,
    'Can courts accurately measure and weigh the harm that speech causes to targets, or does the process of turning lived harm (dignity violation, harassment, threat escalation) into a legal quantity systematize and therefore minimize it?',
    'Compare target-reported harm intensity and duration with judicial findings of harm in balancing decisions. If judicial findings track target reports, measurement is plausible; if judicial findings systematically rate harm lower than targets report, the legal quantification process is reductive.',
    'If measurement systematically minimizes harm, targets gain nominal standing (their harm is ''considered'') while remaining substantively unprotected because judges weigh it lower than speakers'' expression interest. The balancing reading would then benefit targets primarily symbolically rather than materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_harm_measurement_asymmetry, empirical, 'Whether judicial harm measurement captures target lived experience or systematically reduces it').

omega_variable(
    categorical_rule_adequacy_vs_balancing_necessity,
    'Could a refined categorical rule (e.g., Brandenburg + a separate category for systemic-harm speech + a separate category for false speech with demonstrated election impact) achieve the balancing reading''s coordination goals without the extractive overhead of case-by-case adjudication?',
    'Imagine doctrinal history counterfactually: if courts had refined Brandenburg and added targeted exceptions instead of inventing intermediate scrutiny, would later speech cases have been better or worse resolved? Historical fact: the balancing reading *emerged* as courts found categorical refinement insufficient; would it have remained necessary or would better categories have solved the problem?',
    'If refined categories could work, the balancing reading''s extractiveness is partly unnecessary—it extracts from speakers for coordination benefits that could be achieved through clearer rules. If categories provably cannot handle the contextual variation, the extraction is justified by genuine coordination need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_rule_adequacy_vs_balancing_necessity, conceptual, 'Whether balancing achieves novel coordination or refines existing rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__balancing_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(spee_tr_t8, observed).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__balancing_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__balancing_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(spee_tr_t24, observed).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__balancing_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(spee_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__balancing_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(spee_be_t8, observed).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__balancing_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__balancing_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(spee_be_t24, observed).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__balancing_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(spee_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.51).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__balancing_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(spee_su_t8, observed).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__balancing_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__balancing_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(spee_su_t24, observed).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__balancing_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(spee_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel is instantiated by three constraint stories, each a reading: absolutist_reading (categorical protection rule), balancing_reading (this story: case-by-case weighing), harm_limited_reading (target-protective rule). All three readings share the same foundational text (the First Amendment and its competing values) and the same interpretive tradition (First Amendment doctrine), making them a single kernel with multiple readings rather than three independent constraints. Each reading instantiates the kernel differently, producing different protection boundaries and different distributions of gatekeeper authority. The stories are linked by network.affects_constraints to represent the kernel family and the interpretive contest. Each story's cs_structure documents reading_relations and axioms that distinguish it from siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
