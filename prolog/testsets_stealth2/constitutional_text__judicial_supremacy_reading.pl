% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: political/constitutional-theory/comparative-law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'constitutional_text': the judicial supremacy reading, under which the
 *   constitutional text grants courts final interpretive authority and
 *   judicial invalidation of legislation is the conclusive determination of
 *   constitutional meaning. Under this arrangement courts enter as
 *   gatekeepers of constitutional meaning, legislative override is
 *   unavailable, and interpretation is highly rigid — change runs almost
 *   exclusively through amendment or appointment. The intended beneficiary is
 *   the rights-claimant protected against majoritarian overreach; the bearer
 *   of costs is democratic responsiveness, concretely the legislatures whose
 *   statutes are struck and the electorates whose mandates are voidable. The
 *   sibling readings (legislative_sovereignty_reading,
 *   popular_sovereignty_reading) are separate constraint stories in the same
 *   family, linked via network edges — they are not described or averaged
 *   inside this one. The ε referent is the standing judicial-supremacy
 *   arrangement itself, assessed by this reading's own lights. The claim
 *   (tangled_rope) and the metrics are authored independently: the metrics
 *   describe the arrangement's actual operation, and any divergence between
 *   claim and computed type is the datum, not an error.
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter and principal collector (institutional / identity_locked) — administers final interpretive authority and accrues the interpretive power it exercises
 *   - minority_rights_claimants: intended beneficiary (powerless / constrained) — receive a protected litigation forum against majoritarian legislation
 *   - legal_profession_bar_establishment: secondary beneficiary (organized / mobile) — collects complexity rents from doctrinal specialization
 *   - elected_legislatures: primary target (institutional / trapped) — statutes subject to conclusive invalidation with no override lever
 *   - democratic_electorates: diffuse target (powerless / constrained) — electoral mandates voidable by decision, no reversal channel
 *   - popular_constitutionalism_movements: excluded voice (moderate / constrained) — denies the arrangement's premise, holds no seat in adjudication
 *   - comparative_law_analysts: analytical observer (analytical / analytical) — sees the full cross-system structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.66).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "political/constitutional-theory/comparative-law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '1e705aed-a240-489a-a74b-ca64e7830594').
narrative_ontology:cs_kernel_codification('1e705aed-a240-489a-a74b-ca64e7830594', fixed_text).
narrative_ontology:cs_authority_grounding('1e705aed-a240-489a-a74b-ca64e7830594', lineage).
narrative_ontology:cs_interpretation_layer_present('1e705aed-a240-489a-a74b-ca64e7830594').
narrative_ontology:cs_reading_relation('1e705aed-a240-489a-a74b-ca64e7830594', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1e705aed-a240-489a-a74b-ca64e7830594', constitutional_text__popular_sovereignty_reading, influences).
narrative_ontology:cs_axiom('1e705aed-a240-489a-a74b-ca64e7830594', foundational, judicial_invalidation_conclusive).
narrative_ontology:cs_axiom_status(judicial_invalidation_conclusive, holdable).
narrative_ontology:cs_axiom_grounding('1e705aed-a240-489a-a74b-ca64e7830594', judicial_invalidation_conclusive, conventional).
narrative_ontology:cs_axiom('1e705aed-a240-489a-a74b-ca64e7830594', foundational, countermajoritarian_rights_protection_requires_final_arbiter).
narrative_ontology:cs_axiom_status(countermajoritarian_rights_protection_requires_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('1e705aed-a240-489a-a74b-ca64e7830594', countermajoritarian_rights_protection_requires_final_arbiter, instrumental).
narrative_ontology:cs_reference_frame('1e705aed-a240-489a-a74b-ca64e7830594', text_conferring_judicial_finality).
narrative_ontology:cs_drift_state('1e705aed-a240-489a-a74b-ca64e7830594', contemporary_comparative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e705aed-a240-489a-a74b-ca64e7830594', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, minority_rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, legal_profession_bar_establishment).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, elected_legislatures).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_electorates).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, countermajoritarian_judicial_review_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, final_interpretive_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which laws survive constitutional challenge and publishes the operative meaning of the constitution. Every ruling adds to a body of precedent later panels must follow. Judges reach the bench through political appointment but, once seated, answer to no electorate; the institution's standing rests on being the place where constitutional questions are finally settled. Relinquishing that role would mean the institution ceasing to be what it is.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, constitutional_courts, beneficiary).

% Brings challenges against laws that burden them and obtains remedies through constitutional litigation. Protection arrives only when a court accepts the claim; when courts decline or uphold the burdensome law, no other institution can revisit the question. Some organize into litigating coalitions; most encounter the arrangement one case at a time.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, minority_rights_claimants, beneficiary,
    powerless, biographical, constrained, national).

% Staffs and argues the litigation the arrangement generates. Constitutional complexity sustains demand for specialized counsel, clerkships, treatises, and commentary; leading firms and law schools build careers on mastery of doctrines only courts can definitively pronounce. The profession adapts readily to doctrinal change and loses little under any particular ruling.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legal_profession_bar_establishment, beneficiary,
    organized, biographical, mobile, national).

% Drafts and passes statutes knowing any of them can be struck down after enactment, with no procedural route to reinstate an overridden act short of constitutional amendment or new appointments. Majority coalitions form, legislate, and sometimes see their central promises nullified years later. Their counters — confirmation votes, jurisdiction bills, amendment pushes — are slow, blunt, and rarely decisive.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, elected_legislatures, payer,
    institutional, generational, trapped, national).

% Votes for platforms and candidates whose signature commitments can be voided by a decision no ballot can reverse. Between elections there is no channel through which their reading of the constitution counts; influence runs indirectly through the appointment politics of distant future vacancies.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_electorates, payer,
    powerless, biographical, constrained, national).

% Scholars, activists, and occasional politicians who argue that constitutional meaning belongs to the citizenry and that judicial pronouncements bind only until the people speak again through amendment, convention, or mobilization. They publish, litigate at the margins, and lobby for jurisdiction limits, but hold no formal seat in adjudication and no vote on doctrine.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, popular_constitutionalism_movements, excluded,
    moderate, generational, constrained, national).

% Tracks how different polities settle the same question — some constitutions let legislatures override courts outright, others locate authority in amendment conventions — and publishes the comparisons. Bears none of the arrangement's costs and collects none of its proceeds.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_law_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative resolver for disputes about constitutional meaning: interbranch conflicts are settled instead of escalating indefinitely, rights-claimants get a protected forum that electoral politics cannot close, and legal and political actors share stable expectations about which laws stand.
% TRANSFER_FUNCTION: Moves interpretive authority — and with it a working veto over legislation — from elected legislatures and the electorate to the judiciary. Each invalidation transfers one concrete policy decision from the majority's representatives to unelected judges; cumulatively it moves agenda-setting over constitutional meaning itself.
% ABSENT_VOICES: Popular-constitutionalist scholars and movements, legislators whose statutes were struck, and ordinary voters have no formal seat in constitutional adjudication. They speak only through appointment politics, amendment attempts, jurisdiction bills, and occasional open defiance — channels that are slow, indirect, and rarely decisive.
% DISAPPEARANCE_RATIONALE: If judicial finality vanished overnight, legislatures would reclaim the last word (or new settlement institutions would emerge to take it), the precedent corpus would convert from binding to advisory, pending and decided cases would reopen, and every rights-protection bargain currently routed through courts would be renegotiated through political channels. Appointment politics, legal education, and litigation markets would all reorganize around whatever replaced the final arbiter.
% FOUNDING_PROBLEM: Interbranch deadlock over constitutional meaning combined with the exposure of minorities to majoritarian legislation: the arrangement was built to supply a final, non-elective arbiter able to settle disputes the elected branches could not resolve about their own powers and limits.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians document the pre-finality deadlock record, and comparative scholarship shows jurisdictions without judicial finality experiencing both rights failures and alternative settlements — attesting from outside the beneficiary set that the underlying problem is real. Rights-advocacy organizations attest the protective function remains needed. Legislative leaders and popular-constitutionalist scholars, also outside the beneficiary set, dispute that the problem requires a judicial monopoly rather than layered settlement: the problem's liveness is corroborated while the necessity of this particular arrangement is contested.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: every invalidation is a real transfer of decision authority from elected bodies to courts, decoupled from any service the majority consented to — but a meaningful share of the burden is the price of the coordination function itself (authoritative settlement, protected rights forum), which is why this is not snare-level. Suppression 0.66: the override lever is foreclosed and ordinary popular-interpretation channels are closed; persistence depends on actively maintained compliance machinery — precedent-binding, appointment leverage, jurisdiction control, and deference norms. The suppression mechanism is predominantly structural (override impossibility, jurisdictional exclusivity, roughly 70%) with an internalized component (legitimacy deference that persists even where enforcement capacity thins, roughly 30%); the omega on compliance basis carries the residual ambiguity. Theater 0.30: adjudication is substantively functional, but a growing share of activity is the performance of neutral interpretation — doctrinal ritual and the neutrality myth — rather than dispute resolution. Accessibility_collapse 0.70: once finality is understood, intra-system alternatives collapse almost completely (no override, no rival interpretive channel), while extra-system alternatives (amendment, jurisdiction politics, comparative borrowing) remain partly open. Resistance 0.55: recurring court-curbing bills, packing fights, confirmation wars, and the scholarly assault from popular constitutionalism. The measurement series run on one shared grid (T0≈1965 to T60≈2025, abstract units) showing a ratchet: extraction, suppression requirement, and theater all rise monotonically. Episodic backlash produces temporary plateaus rather than reversals, so the series is modeled as a ratchet rather than a cycle; the oscillation-amplification question is noted but not the dominant dynamic. Identity-lock dynamics: the judiciary's fusion is institutional — the modern court has 'become' its function, and final-arbiter status is constitutive of judicial self-concept; if that frame broke, courts would recompute as one interpreter among several and the arrangement's enforcement profile would collapse toward ordinary interbranch negotiation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute the same text as opposite constraints. From the constitutional_courts seat the arrangement is legitimate coordination it staffs and embodies — finality is what a constitution is for. From the elected_legislatures and democratic_electorates seats the identical structure operates as enforced removal of decision authority: statutes struck with no recourse, mandates voidable by nine (or however many) unelected officials. Minority_rights_claimants sit closest to symmetric: the forum that protects them is the same one that can lock in hostile interpretations irreversibly. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. constitutional_courts (agenda_setter + beneficiary, identity_locked) sits near the beneficiary end — it collects the interpretive authority the arrangement concentrates, and its identity-lock removes any exit-modulating discount. minority_rights_claimants (beneficiary, constrained exit) derive low directionality: subsidized by the forum. legal_profession_bar_establishment (beneficiary, mobile exit approaching arbitrage) derives the lowest directionality — it collects fees from the complexity without bearing doctrinal risk. elected_legislatures (payer, trapped — the override lever simply does not exist for them) derive high directionality, amplified by their institutional power making them the visible counterparty. democratic_electorates (payer, constrained) derive high directionality diffusely. National spatial scope keeps verification feasible and scope amplification moderate. No directionality overrides are authored: the structural derivation from roles plus exit options already places every seat correctly, and the schema's override surface is keyed by power atom, which would collide across the two institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope ignores the transfer function — invalidation moves decisions from electorates to courts, and the profession collects complexity rents on top. Reading it as pure snare ignores the genuine coordination function that would leave a vacuum: authoritative settlement of interbranch disputes and a protected rights forum that electoral politics cannot close. The R5 interview shows the founding problem (deadlock plus majoritarian overreach) is still live and externally corroborated, so no mandatrophy-resolved flag is authored — but the measurement ratchet tracks mandate expansion beyond the founding scope: the enforcement requirement grows faster than the settlement function, which is the signature to watch for drift toward pure extraction. Coalition check: the two payer seats could in principle combine (legislative majorities plus electoral mobilization behind amendment or jurisdiction reform), and the analysis treats that coalition as the main realistic correction path — its repeated failure is itself part of why suppression trends upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel ''constitutional_text''. Does the constitutional text actually grant courts final interpretive authority, or is finality a practice the judiciary self-constituted and later attributed to the text?',
    'Comparative textual analysis of constitutional instruments alongside historical study of the assertion moments (Marbury-type cases): if canonical texts routinely omit any explicit grant of finality, the reading''s textual premise fails and finality is practice-grounded.',
    'If the grant is not textual, the arrangement''s authority shifts from lineage to self-asserted practice, its extractiveness rises (courts collecting authority they assigned themselves), and the CS framing under-determination resolves toward the less obvious framing — the legitimacy claim layered above the text rather than the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether judicial finality is text-granted or self-constituted by practice.').

omega_variable(
    sibling_reading_delta,
    'What exactly would the sibling readings change structurally? Under legislative_sovereignty_reading the override lever exists and courts advise rather than conclude; under popular_sovereignty_reading neither seat is final and the demos retains ultimate interpretive authority through amendment, convention, or revolution. Is this reading''s distinctive structural element the foreclosure of override, the location of ultimate authority, or both?',
    'Compile the sibling stories and diff their victim sets, enforcement requirements, disappearance verdicts, and computed types against this story''s.',
    'Confirms the family decomposition: if the diff shows the readings differ only in rhetoric while sharing structure, the kernel was mis-split into three stories and should be re-merged; if the diff localizes the difference in the override lever and victim sets, the decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Structural delta between this reading and its two siblings.').

omega_variable(
    rights_claimant_net_position,
    'Are rights-claimants net beneficiaries of judicial finality, or do they also bear costs when courts uphold oppressive legislation or lock in hostile interpretations that no override can dislodge?',
    'Longitudinal audit of invalidation outcomes by claimant type: win rates, plus entrenchment cases where a hostile interpretation became effectively irreversible absent amendment.',
    'If a substantial share of claimant classes nets negative, the beneficiary declaration splits, some currently-beneficiary seats acquire target-side directionality, and the coordination-function half of the classification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_claimant_net_position, empirical, 'Net beneficiary status of the rights-claimant seat.').

omega_variable(
    compliance_basis_ambiguity,
    'Is compliance with judicial rulings driven by voluntary legitimacy (deference norms internalized across the political system) or by enforceable leverage (appointment control, jurisdiction, precedent machinery)?',
    'Natural experiments from defiance episodes — desegregation timelines, court-curbing bills, packing threats: measure whether compliance recovered through legitimacy persuasion or through institutional retaliation and leverage.',
    'If compliance is chiefly coercive leverage, suppression is structural and higher than the scalar suggests; if chiefly legitimacy, the internalized share dominates and the arrangement would survive enforcement decay longer than a purely coerced structure would — changing the persistence forecast in either direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_basis_ambiguity, empirical, 'Voluntary-legitimacy versus coercive-leverage basis of compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__judicial_supremacy_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(cons_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_text__judicial_supremacy_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(cons_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_text__judicial_supremacy_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(cons_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family 'constitutional_text': the colloquial label 'the constitution settles who decides constitutional meaning' decomposes into three structurally distinct readings with different victim sets, enforcement profiles, and epsilon values — per the epsilon-invariance principle, one label covering multiple claims is multiple constraints. This member (judicial_supremacy_reading) links to both siblings. Direction of influence: this reading's settled-doctrine output is frequently cited as evidence inside popular-sovereignty debates, and legislative-override designs (notwithstanding clauses) are proposed as remedies to this reading's rigidity — so this story sits upstream of both siblings' operating environments without resolving the contest among them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
