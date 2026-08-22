% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Conditional Speech Protection (Demonstrable-Harm Threshold Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the harm-threshold reading — of the
 *   contested speech_protection_kernel: the commitment, canonized in
 *   constitutional and treaty texts (UDHR/ICCPR art. 19, ECHR art. 10 and
 *   their qualification clauses), that expressive liberty is a protected
 *   default whose protection is withdrawn where expression demonstrably
 *   injures identifiable victims. The standing arrangement under contest —
 *   the epsilon referent — is the harm-conditioned protection regime itself
 *   as this reading assesses it: a broad protected zone bounded by an
 *   adjudicated harm threshold, administered by courts, commissions, and
 *   platform enforcement bodies. The reading's structural signature is a
 *   narrower protection boundary than its siblings carry: victim harm claims
 *   override speaker autonomy once the threshold is met, and the set of
 *   unprotected categories is correspondingly wider. Per the
 *   epsilon-invariance principle, the colloquial label 'freedom of speech'
 *   decomposes into five structurally distinct protection-boundary claims;
 *   this file authors only the harm-threshold member and links its siblings
 *   through network.affects_constraints. Claim and metrics are independent
 *   authored facts: the constraint is CLAIMED as tangled_rope (genuine
 *   protective coordination fused with asymmetric extraction through
 *   definitional discretion), while the metrics describe its observed
 *   operation without being tuned to that claim. KEY AGENTS (by structural
 *   relationship): - harm_adjudication_bodies: Agenda setter
 *   (institutional/identity_locked) — defines 'demonstrable,' operates the
 *   evidentiary standard, collects jurisdiction and caseload -
 *   recognized_harm_victims: Primary beneficiary (moderate/constrained) —
 *   their certified accounts ground restriction -
 *   threshold_crossing_speakers: Primary target (moderate/constrained) —
 *   bears fines, takedowns, injunctions - chilled_expression_communities:
 *   Preemptively suppressed target (powerless/trapped) — governed by
 *   anticipated misclassification - large_platform_operators: Enforcement
 *   intermediary, net beneficiary (institutional/arbitrage) — executes the
 *   threshold at scale, gains moat and blame cover -
 *   unrepresented_future_speakers: Excluded seat (powerless/trapped) —
 *   governed by standards set without them - free_expression_legal_scholars:
 *   Analytical observer (analytical/analytical) — tracks boundary drift
 *
 * KEY AGENTS:
 *   - harm_adjudication_bodies: Agenda setter (institutional/identity_locked) — defines demonstrability, operates the evidentiary standard, collects jurisdiction and caseload
 *   - recognized_harm_victims: Primary beneficiary (moderate/constrained) — certified accounts ground restriction
 *   - threshold_crossing_speakers: Primary target (moderate/constrained) — bears fines, takedowns, injunctions
 *   - chilled_expression_communities: Preemptively suppressed target (powerless/trapped) — governed by anticipated misclassification
 *   - large_platform_operators: Enforcement intermediary, net beneficiary (institutional/arbitrage) — executes the threshold at scale, gains moat and blame cover
 *   - unrepresented_future_speakers: Excluded seat (powerless/trapped) — governed by standards set without them
 *   - free_expression_legal_scholars: Analytical observer (analytical/analytical) — tracks boundary drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.55).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Conditional Speech Protection (Demonstrable-Harm Threshold Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '7bd28bde-db36-44ec-af2d-5be63e5843cb').
narrative_ontology:cs_kernel_codification('7bd28bde-db36-44ec-af2d-5be63e5843cb', fixed_text).
narrative_ontology:cs_authority_grounding('7bd28bde-db36-44ec-af2d-5be63e5843cb', lineage).
narrative_ontology:cs_interpretation_layer_present('7bd28bde-db36-44ec-af2d-5be63e5843cb').
narrative_ontology:cs_reading_relation('7bd28bde-db36-44ec-af2d-5be63e5843cb', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7bd28bde-db36-44ec-af2d-5be63e5843cb', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bd28bde-db36-44ec-af2d-5be63e5843cb', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('7bd28bde-db36-44ec-af2d-5be63e5843cb', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('7bd28bde-db36-44ec-af2d-5be63e5843cb', foundational, restriction_requires_demonstrated_victim_harm).
narrative_ontology:cs_axiom_status(restriction_requires_demonstrated_victim_harm, holdable).
narrative_ontology:cs_axiom_grounding('7bd28bde-db36-44ec-af2d-5be63e5843cb', restriction_requires_demonstrated_victim_harm, empirically_contingent).
narrative_ontology:cs_axiom('7bd28bde-db36-44ec-af2d-5be63e5843cb', foundational, victim_standing_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_standing_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('7bd28bde-db36-44ec-af2d-5be63e5843cb', victim_standing_overrides_speaker_autonomy, instrumental).
narrative_ontology:cs_reference_frame('7bd28bde-db36-44ec-af2d-5be63e5843cb', harm_conditioned_protection_regime).
narrative_ontology:cs_drift_state('7bd28bde-db36-44ec-af2d-5be63e5843cb', contemporary_platform_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7bd28bde-db36-44ec-af2d-5be63e5843cb', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, recognized_harm_victims).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_adjudication_bodies).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, large_platform_operators).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, threshold_crossing_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, chilled_expression_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, large_platform_operators).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, proportionality_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, human-rights commissions, press councils, and platform oversight boards that define what counts as 'demonstrable,' operate the evidentiary standards, and issue restriction orders. Jurisdiction, caseload, budget, and doctrinal authority flow to them with every recognized category. They cannot exit the adjudicative role without dissolving their mandate; their institutional identity is constituted by operating this boundary.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_adjudication_bodies, agenda_setter,
    institutional, generational, identity_locked, national).

% People who have suffered demonstrable injury from speech — defamation losses, coordinated harassment campaigns, incitement-fueled violence — and whose accounts, once certified, become the evidentiary basis for restriction. Standing, remedies, takedowns, and damages flow to them. They cannot leave the speech environment where the harm occurred; their remedy runs through the very adjudicative machinery whose standards determine whether their injury counts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, recognized_harm_victims, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression is classified as having crossed a harm threshold — defendants in defamation actions, commentators ruled to have incited, satirists misread as threats. They bear fines, takedowns, injunctions, and reputational sanction. Exit means leaving the jurisdiction's speech order or moving to fringe channels at the cost of audience.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, threshold_crossing_speakers, payer,
    moderate, biographical, constrained, national).

% Communities that self-censor under threshold uncertainty — minority groups discussing internal grievances, academics near sensitive findings, journalists approaching powerful subjects. No ruling names them; the anticipated cost of misclassification does the work. Their speech topics are constitutive of their civic identity, so silence is not a neutral alternative, and relocating speech abroad forfeits the audience that made it worth speaking.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, chilled_expression_communities, payer,
    powerless, biographical, trapped, national).

% Operate the infrastructures where most speech now occurs and execute harm-threshold enforcement at scale through terms-of-service moderation. Compliance costs and liability exposure flow out; legitimacy cover ('legally required'), regulatory moats smaller rivals cannot afford, and blame-shielding flow in. They arbitrage across jurisdictions, tuning enforcement to the strictest major market while applying it globally.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, large_platform_operators, payer).

% People whose future expression will be evaluated against thresholds being set now, but who have no seat in standard-setting consultations, which hear certified harm claims and institutional interests. Their expressive interests enter the process only retrospectively, as defendants.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, unrepresented_future_speakers, excluded,
    powerless, generational, trapped, national).

% Comparative constitutional scholars and civil-liberties litigators who track the boundary's movement, publish audits of category expansion, and litigate test cases. They collect no rents and bear no sanctions; their seat is the analytical record of drift.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, free_expression_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, harm_adjudication_bodies).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legible procedure for adjudicating the recurring conflict between expressive liberty and protection from demonstrable injury: diffuse grievances are converted into reviewable harm claims under stated evidentiary standards, so restriction decisions turn on criteria rather than on raw power, wealth, or crowd pressure.
% TRANSFER_FUNCTION: Moves expressive autonomy and sanction risk from speakers to adjudicating institutions' discretion; moves standing, remedies, and removal power to claimants whose harm is certified; moves definitional authority over 'harm' and 'victim' to courts, commissions, and platform policy bodies.
% ABSENT_VOICES: Future speakers whose expression will be judged by thresholds being set now have no seat in standard-setting; accused speakers in fast-moving tribunal and takedown processes frequently lack effective representation before the decision lands; audiences who value transgressive or dissenting speech appear only indirectly. The process hears certified harm claims and institutional interests but almost no prospective expressive interests, biasing the boundary toward restriction.
% DISAPPEARANCE_RATIONALE: If the harm-conditioned regime vanished overnight, speech law would reorganize around a sibling boundary (absolutist or dignity-based), every restriction resting on a demonstrated-harm finding would lose its operative basis, certified victims would lose their formal channel, and platform moderation rules referencing 'harm' would need re-grounding from scratch — the expressive order visibly depends on the arrangement.
% FOUNDING_PROBLEM: Unrestricted speech demonstrably injures — defamation destroys livelihoods, incitement produces violence, coordinated harassment drives people from public discourse — and mid-twentieth-century codifiers needed a principled line separating tolerable offense from actionable harm that did not hand authorities a general license to suppress.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional and treaty dockets across jurisdictions attest continued incidence of the underlying injuries (defamation, incitement, and harassment proceedings recur annually); the ICCPR and ECHR drafting records show the harm condition was negotiated as a response to documented interwar abuses; and free-expression scholars who reject the threshold's design nonetheless document the injuries that motivated it. No attesting source denies the problem's persistence; the contest is over the boundary's width, not the problem's existence.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.55) is substantial but bounded: the regime's core operation — restricting speech only upon demonstrated injury — is its designed function, and much of what it removes from speakers is removed pursuant to the threshold working as intended; the extractive share accumulates at the margins, where unprotected categories expand faster than demonstrated harm warrants (series: 0.28 to 0.55 across the interval). Suppression (0.62) is authored as a raw structural property, unscaled by power or scope: the machinery enforcing the threshold — statutory schemes, commissions, notice-and-takedown, platform moderation at scale — is coercive by construction, and its build-up is precisely the dynamic the suppression_requirement series traces (0.38 to 0.62), which is why that series is authored here rather than left to the scalar. Theater (0.34) rises with compliance ritualization: transparency reports, consultative procedures, and symbolic enforcement that manage legitimacy more than they adjudicate harm. Accessibility_collapse (0.40) is moderate: alternatives persist — offshore hosting, encrypted channels, pseudonymity — but each carries real cost and audience loss. Resistance (0.60) is high and persistent: absolutist litigators, civil-liberties organizations, and affected speaker communities contest every category expansion. All three series run on one shared six-point grid (1948-2025), each metric authored at every point; endpoints equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the adjudicator seat the arrangement is due process: claims heard, evidence weighed, criteria stated. The same structure reads as protection from the certified-victim seat and as revocable permission from the payer seats, where protection lasts only until an adversary certifies harm against you. The platform seat occupies a double position: compliance cost paid upward, competitive moat collected downward, since fixed enforcement burdens advantage incumbents over smaller rivals. Chilled communities occupy the extreme: they are governed without ever appearing in a case file, so no adjudicator ever observes their restraint. The engine derives this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: recognized_harm_victims (certified standing, remedies), harm_adjudication_bodies (jurisdiction, caseload, doctrinal authority), and large_platform_operators (regulatory moat, legitimacy cover, blame-shielding — net beneficiaries despite real compliance costs, hence the secondary payer role). Victim declarations drive high d: threshold_crossing_speakers bear the transfer directly with constrained exit; chilled_expression_communities sit nearest the full-target end — preemptively suppressed, effectively trapped, their expressive topics constitutive of civic identity so silence is not a neutral alternative. The derivation chain from these declarations plus exit options yields accurate directionalities for every seat, so no directionality_overrides entries are authored. Coalition note: chilled communities are individually powerless but structurally capable of coalition — shared grievance and dense networks give their effective power a ceiling above the individual atom, reachable through the organized-power pathway if they mobilize.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unrestricted speech demonstrably injures, and societies need a principled line short of a censor's general license — remains live, so no mandatrophy is declared and none is resolved: the arrangement has not outlived its function. The tangled-rope classification prevents both mislabelings: reading the regime as pure rope erases the extraction channel (definitional discretion over 'demonstrable,' weaponizable certification, category creep), while reading it as pure snare erases the protective function that certified victims concretely rely on and that corroborating sources outside the benefiting parties document. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag fires; the arrangement persists because the problem persists, not because anyone neglected to dismantle it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Which reading of the speech_protection_kernel does the operative legal order actually instantiate — this harm-threshold reading, or one of its siblings (absolutist, marketplace, dignity, democratic_participation)?',
    'Comparative mapping of operative judicial tests and platform policies onto reading-specific axioms: categoricity (absolutist), truth-discovery rationale (marketplace), subordination conditions (dignity), political-expression weighting (democratic_participation), demonstrated-harm requirements (this reading).',
    'If a sibling reading dominates operative doctrine, this constraint''s boundary width, victim set, and epsilon transfer to that sibling''s story; the family classification follows the dominant reading, not the colloquial label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'This story instantiates one reading of a contested kernel; operative doctrine may instantiate a sibling instead.').

omega_variable(
    demonstrability_standard_drift,
    'Does ''demonstrable harm'' retain its evidentiary meaning — causal proof of injury to an identifiable victim — or has operative practice drifted to precautionary risk assessment that restricts before demonstration?',
    'Trace adjudicated decisions and platform enforcement records across the interval for the evidentiary standard actually applied at the moment of restriction.',
    'If drifted, restriction occurs without the demonstration this reading''s foundational axiom requires — measured epsilon understates actual autonomy seizure and the computed type shifts toward snare; if intact, the threshold remains a genuine evidentiary brake on restriction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrability_standard_drift, empirical, 'Whether the ''demonstrable'' evidentiary brake still holds in operative practice.').

omega_variable(
    victim_certification_symmetry,
    'Is certification of harm claims symmetric across claimant social position, or do powerful and credentialed claimants more readily have their harms certified while marginalized speakers'' injuries go unrecognized?',
    'Audit recognized versus rejected harm claims by claimant position, resources, and target status across comparable injury types.',
    'Asymmetric certification converts the coordination function into an enforcement instrument of the credentialed — the same structure coordinates protection and extracts expressive liberty along status lines, deepening the tangled-rope verdict toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_certification_symmetry, empirical, 'Whether victim certification is status-symmetric.').

omega_variable(
    epsilon_referent_discipline,
    'Within this reading''s own lights, does restriction issued pursuant to demonstrated harm count as extraction from speakers at all, or as the regime''s proper protective function — and how much of the authored epsilon reflects overreach beyond demonstrated cases?',
    'Decompose the restriction caseload into demonstrated-harm rulings versus precautionary or expansive-category rulings; attribute epsilon accordingly.',
    'If most measured extraction sits in threshold-compliant rulings, the reading would contest the epsilon attribution itself; if it sits in expansive-category overreach, the reading''s own lights concede the extraction while defending the core — sharpening the tangled-rope verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_discipline, conceptual, 'How the reading''s own evaluative lights distribute the authored epsilon between proper function and overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_harm_threshold_tr_t1948, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t1948, observed).
narrative_ontology:measurement(spk_harm_threshold_tr_t1965, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1965, 0.14).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t1965, observed).
narrative_ontology:measurement(spk_harm_threshold_tr_t1980, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t1980, observed).
narrative_ontology:measurement(spk_harm_threshold_tr_t1995, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t1995, observed).
narrative_ontology:measurement(spk_harm_threshold_tr_t2010, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t2010, observed).
narrative_ontology:measurement(spk_harm_threshold_tr_t2025, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement_basis(spk_harm_threshold_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(spk_harm_threshold_be_t1948, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1948, 0.28).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t1948, observed).
narrative_ontology:measurement(spk_harm_threshold_be_t1965, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1965, 0.33).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t1965, observed).
narrative_ontology:measurement(spk_harm_threshold_be_t1980, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1980, 0.39).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t1980, observed).
narrative_ontology:measurement(spk_harm_threshold_be_t1995, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t1995, observed).
narrative_ontology:measurement(spk_harm_threshold_be_t2010, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t2010, observed).
narrative_ontology:measurement(spk_harm_threshold_be_t2025, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(spk_harm_threshold_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(spk_harm_threshold_su_t1948, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1948, 0.38).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t1948, observed).
narrative_ontology:measurement(spk_harm_threshold_su_t1965, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1965, 0.44).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t1965, observed).
narrative_ontology:measurement(spk_harm_threshold_su_t1980, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1980, 0.49).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t1980, observed).
narrative_ontology:measurement(spk_harm_threshold_su_t1995, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t1995, observed).
narrative_ontology:measurement(spk_harm_threshold_su_t2010, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t2010, observed).
narrative_ontology:measurement(spk_harm_threshold_su_t2025, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(spk_harm_threshold_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% 'Freedom of speech' is a colloquial label covering at least five structurally distinct protection-boundary claims; per the epsilon-invariance principle each is authored as its own story with its own epsilon, victim set, and boundary width, linked as a constraint family. Upstream members (absolutist, marketplace) assert broader protection and are routinely cited as the baseline against which narrowing is justified; downstream members (this harm-threshold reading, dignity) assert narrower boundaries whose extraction is measured against that baseline — the wider the upstream protection claim, the more expressive liberty each downstream narrowing transfers. This file authors only the harm-threshold member; the sibling files carry their own classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
