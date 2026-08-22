% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection — Antisubordination (Remedial) Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   A dominant interpretive regime inside United States constitutional law
 *   reads the Fourteenth Amendment's Equal Protection Clause as an
 *   antisubordination command: state action may not perpetuate caste, and
 *   race-conscious measures aimed at dismantling inherited racial hierarchy
 *   are therefore constitutionally legitimate, sometimes required. Under this
 *   reading, universities weigh race in admissions, agencies operate minority
 *   contracting programs, legislatures craft race-targeted remedies, and
 *   courts police the line between dismantling and new preference. Authored
 *   as ONE reading of the contested equal-protection kernel (Rule 1): the
 *   epsilon referent is the standing remedial arrangement itself — the
 *   operative permission-and-policing structure — assessed by the reading's
 *   own lights, not the caste system it opposes and not the colorblind
 *   arrangement its rivals endorse. Per the epsilon-invariance principle, the
 *   label 'equal protection' decomposes into three structurally distinct
 *   constraints (this remedial reading, the colorblind reading, the diversity
 *   reading), linked through network.affects_constraints; each carries its
 *   own epsilon, beneficiaries, and victims. KEY AGENTS (by structural
 *   relationship): - remedial_program_administrators: Agenda-setting
 *   implementer (institutional/constrained) — designs and defends
 *   race-conscious programs; collects legitimacy and enrolled talent -
 *   federal_courts: Agenda-setting enforcer (institutional/constrained) —
 *   custodian of the doctrinal line determining which racial classifications
 *   survive - historically_subordinated_racial_groups: Primary beneficiary
 *   (organized/trapped) — named recipients of corrective access across
 *   generations - civil_rights_advocacy_organizations: Mission-fused
 *   beneficiary (organized/identity_locked) — litigates, drafts, and
 *   evidences the remedial project - historically_privileged_applicants:
 *   Primary target (moderate/constrained) — bears displaced opportunities
 *   wherever a program operates - class_disadvantaged_excluded_applicants:
 *   Excluded voice (powerless/trapped) — passed over by race-specific
 *   remedies with no seat in the doctrinal conversation -
 *   constitutional_law_scholars: Analytical observer (analytical/analytical)
 *   — maps the field; collects nothing, bears nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection — Antisubordination (Remedial) Reading").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6').
narrative_ontology:cs_kernel_codification('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', fixed_text).
narrative_ontology:cs_authority_grounding('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', lineage).
narrative_ontology:cs_interpretation_layer_present('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6').
narrative_ontology:cs_reading_relation('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', foundational, equal_protection_forbids_caste_perpetuation).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste_perpetuation, holdable).
narrative_ontology:cs_axiom_grounding('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', equal_protection_forbids_caste_perpetuation, deontological).
narrative_ontology:cs_axiom('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', secondary, race_conscious_remediation_is_legitimate).
narrative_ontology:cs_axiom_status(race_conscious_remediation_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', race_conscious_remediation_is_legitimate, instrumental).
narrative_ontology:cs_reference_frame('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', caste_abolition_baseline).
narrative_ontology:cs_drift_state('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', contemporary_post_sffa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0e5f9ef5-43ea-4ff3-8cd7-71923b292cd6', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, remedial_program_administrators).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, caste_abolition_commitment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run race-conscious admissions, hiring, and contracting programs under a constitutional permission that treats such measures as tools for dismantling inherited racial hierarchy. They set program scope, eligibility, and the weight given to race, defend the programs in litigation, and recalibrate them as judicial doctrine tightens. Their institutions gain mission legitimacy and enrolled talent; they also absorb compliance costs and adverse rulings.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, remedial_program_administrators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, remedial_program_administrators, beneficiary).

% Adjudicate which race-conscious measures survive, applying tests that ask whether a program remedies identified discrimination or serves another compelling end and whether it is narrowly drawn. Each ruling redraws the boundary of what state actors may do; the judiciary's authority over this field rests on continuing custody of that line.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Are the populations the remedial framework names as owed correction for state-built and state-sanctioned hierarchy. Members gain preferential access to universities, employment, and public contracts where programs operate; the framework's promise runs to the group across generations, and members cannot decline to be racial subjects of the classification system that delivers it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, trapped, national).

% Litigate to establish and defend the remedial permission, draft model programs, compile the evidentiary records of past discrimination, and staff the coalitions that sustain the framework. Their institutional purpose is constituted by the antisubordination project; retreating from it would dissolve the organizations' reason for being.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Lose admissions seats, promotions, contracts, or program eligibility to racial preferences they did not consent to, on the theory that their group's historical advantage justifies individual displacement. Some litigate — a coordinated coalition recently prevailed at the Supreme Court in the admissions domain — some relocate to institutions or jurisdictions without programs; none can opt out of being classified where a program operates.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_applicants, payer,
    moderate, biographical, constrained, national).

% Are poor and working-class applicants of every race whom race-specific remedies pass over. They would press for class-based preferences cutting across the racial scheme, but they hold no seat in the doctrinal conversation, which is framed entirely around race.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, class_disadvantaged_excluded_applicants, excluded,
    powerless, biographical, trapped, national).

% Map the doctrinal field, test the competing readings against text, history, and precedent, and forecast where the line moves next. They collect no program benefits and bear no program costs.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective commitment problem: how a polity that has formally renounced racial caste prevents inherited hierarchy from reproducing itself through facially neutral institutions, by authorizing and sometimes requiring state action targeted at the hierarchy's racial structure.
% TRANSFER_FUNCTION: Moves access to state-conferred goods — university admissions, public employment, government contracts, electoral structures — from applicants disfavored by the racial classification toward members of groups the arrangement names as owed correction; and moves interpretive authority over the permissibility of racial classifications to the judiciary.
% ABSENT_VOICES: Class-disadvantaged applicants of all races, whom race-specific remedies pass over and who would argue for cross-racial class-based preferences; residents of jurisdictions whose programs were struck down before they could benefit; and future cohorts who inherit both the residual hierarchy and the resentment the classification system generates, none of whom consented to the trade.
% DISAPPEARANCE_RATIONALE: If the remedial permission vanished overnight, operating programs would lose their legal foundation, admissions and contracting compositions would shift immediately, pending desegregation decrees and race-conscious voting-rights enforcement would lose their doctrinal anchor, and the litigation coalition defending the framework would dissolve — the allocation of opportunities across racial lines would reorganize around whichever reading replaced it.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to destroy the Black Codes and prevent states from re-creating caste through law; the remedial reading holds that problem live wherever state-built or state-sanctioned racial hierarchy persists beneath formally neutral rules.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: congressional and municipal findings of past discrimination compiled under the strong-basis-in-evidence standard and upheld in the Croson line of cases, intergenerational wealth-gap research in economics and sociology, and Reconstruction historiography all attest the founding problem and its persistence. Critics from the colorblind seat attest the historical problem was real while disputing its present reach — corroboration of the problem's reality, contest over its currency.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.55 (inside the expected 0.45-0.60 band): the arrangement denies concrete opportunities to identifiable individuals through racial classification even as it pursues a corrective end the reading endorses — the reading's own lights count that cost as real, priced against the hierarchy being dismantled. Suppression (0.48) is a raw structural property, deliberately unscaled: persistence requires active judicial policing (strict scrutiny, narrow-tailoring review, program termination orders), and participants cannot opt out of classification where programs operate; only extractiveness is scaled downstream by directionality and scope. Theater_ratio 0.35: the dismantling function is real, but a growing share of activity is symbolic commitment and diversity-branded maintenance. Accessibility_collapse 0.35: alternatives remain live — race-neutral class-based preferences, jurisdictional arbitrage, and the two sibling readings all persist as operable positions. Resistance 0.62: sustained litigation culminating in a successful coalition challenge in the admissions domain, plus state-level bans and ballot initiatives. Claim/metric independence: claimed_type tangled_rope is asserted from structure — a genuine coordination function (an anti-caste commitment coordinating state action), asymmetric extraction (a named payer set), and active enforcement — while the metrics are authored independently from observed operation; the engine computes per-seat types from the structural data, and any divergence from the claim is the measurement the corpus exists to take. Temporal grid: one shared grid for all tracked metrics, t=0 corresponds to 1964, one unit is roughly two years, t=30 corresponds to 2024; all three metrics are authored at every point. Receipt surface: gain_flow names historically_subordinated_racial_groups because the transferred opportunities (seats, contracts, positions) demonstrably land with program-eligible members of those groups — administrators accrue legitimacy incidentally, but the material transfer lands elsewhere. fixing_cost 'prohibitive': wholesale removal would require unwinding entrenched precedent across education, contracting, employment, and voting enforcement, with political and constitutional stakes exceeding any single fixer's benefit — partial removal in admissions has occurred, but full removal remains prohibitively costly relative to perceived benefit.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical doctrine. The disfavored-applicant seat experiences the arrangement as uncompensated racial exclusion — a classification imposed without consent, contested by a coalition that ultimately prevailed in the admissions domain, demonstrating that same-level lateral coalition power can move even a constitutionally anchored structure. The beneficiary seat experiences overdue correction for state-built hierarchy. The administrator seat experiences legitimate governance under legal permission. The judicial seat experiences line-drawing among compelling interests. Same instrument, different computed types per seat — the engine derives this divergence from power, exit, and directional data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for historically_subordinated_racial_groups and remedial_program_administrators; the victim declaration drives high d for historically_privileged_applicants. The single directionality override (institutional -> 0.25) exists because the automatic derivation cannot distinguish the two institutional seats — program administrators (declared beneficiaries, deriving d near the beneficiary end) and courts (agenda-setters with no beneficiary declaration, falling back mid-range) — when both are structurally invested in the regime's persistence: administrators gain mission legitimacy from running programs, and courts hold authority over this field precisely as custodians of the line. One moderate near-beneficiary value captures both honestly. The excluded seat takes no flow; the observer seat is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-reproduced caste — is corroborated as historically real and disputed as presently resolved (founding_problem_status: contested). The mandatrophy question is whether the remedial apparatus transitions out as disparities close, completing its work, or entrenches as permanent allocative infrastructure that renews regardless of measured progress. The classification guards against both mislabels: calling the arrangement pure extraction erases the genuine coordination function that gives it legitimacy among its beneficiaries; calling it pure coordination erases the named payers whose opportunities are displaced. The omega remedial_mandate_trajectory routes the transition-versus-entrenchment question to evidence rather than settling it by label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the equal_protection_commitment kernel governs — antisubordination (this file), anticlassification (colorblind_reading), or diversity-as-compelling-interest (diversity_reading)?',
    'Doctrinal evolution: successive Supreme Court majorities selecting among readings; each reading lives as a separate constraint story linked through network.affects_constraints, and the contest is never averaged inside any one file.',
    'Colorblind adoption deletes this reading''s beneficiary set entirely (no permitted race-conscious action, hence no disfavored payers); diversity adoption narrows beneficiaries to educational institutions and shrinks the victim set to applicants outside holistic review; this story''s epsilon, beneficiaries, and victims are valid only under the remedial frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the equal-protection kernel; sibling readings instantiate different constraints with different beneficiary/victim sets, and the disagreement is located in the Clause''s core premise (anticlassification vs. antisubordination).').

omega_variable(
    beneficiary_victim_inversion,
    'Is the race-disfavored applicant a net loser under this arrangement, or does the dismantling function return net class-level gains to them that invert the apparent victim position depending on observer seat?',
    'Longitudinal cohort outcomes: trace disfavored applicants'' lifetime economic and civic outcomes against no-program counterfactual baselines, and program beneficiaries'' outcomes against matched comparison groups.',
    'If disfavored applicants are net losers even accounting for dismantled hierarchy, extraction stands as authored; if class-level gains offset individual displacement, the seat migrates toward symmetry and effective extraction falls materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_inversion, empirical, 'Whether the payer seat''s loss is net or offset by gains from the hierarchy the arrangement dismantles.').

omega_variable(
    remedial_mandate_trajectory,
    'Is the remedial mandate transitioning (subordination measurably receding and programs sunsetting as their work completes) or entrenching (programs persisting as permanent allocative infrastructure regardless of disparity trends)?',
    'Compare racial disparity trajectories against program sunset behavior: do programs expire when their trigger conditions resolve, or renew irrespective of measured progress?',
    'Transition confirmed: the arrangement carries transitional character and its justification decays with success. Entrenchment confirmed: the arrangement persists past its mandate and the founding-problem mismatch flag fires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_mandate_trajectory, empirical, 'Whether the remedial apparatus is transitional or self-perpetuating.').

omega_variable(
    corrective_vs_extractive_transfer,
    'Is the opportunity transfer this arrangement performs corrective restitution for state-built hierarchy, or forward-looking allocation that creates newly disfavored persons without remedying identified past injury?',
    'Causal tracing of program benefits to injuries the state itself inflicted (versus societal discrimination generally), auditing whether program beneficiaries overlap the injured populations under the strong-basis-in-evidence standard.',
    'Corrective framing lowers the moral valence of measured extraction (restitution rather than rent); allocative framing raises it (innocent parties bearing costs for others'' benefit) and pushes the arrangement toward purer extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corrective_vs_extractive_transfer, conceptual, 'Whether the transfer is restitutionary or newly distributive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t5, equal_protection_commitment__remedial_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(equa_tr_t5, observed).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t15, equal_protection_commitment__remedial_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(equa_tr_t15, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__remedial_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(equa_tr_t25, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(equa_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t5, equal_protection_commitment__remedial_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(equa_be_t5, observed).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t15, equal_protection_commitment__remedial_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(equa_be_t15, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__remedial_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(equa_be_t25, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(equa_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t5, equal_protection_commitment__remedial_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(equa_su_t5, observed).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t15, equal_protection_commitment__remedial_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(equa_su_t15, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__remedial_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement_basis(equa_su_t25, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(equa_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'equal protection' covers three structurally distinct claims, authored as three stories. This remedial reading permits race-conscious state action to dismantle caste (beneficiaries: subordinated groups and implementing state actors; victims: disfavored applicants; epsilon 0.55). The colorblind sibling forbids all racial classification (no permitted beneficiaries, symmetric prohibition, near-zero extraction). The diversity sibling permits race as one factor for educational diversity (institutional beneficiaries, narrower victim set). The colorblind reading is upstream in lineage (Harlan's Plessy dissent) and is cited as the rival foundation; the diversity reading borrows this reading's permission structure while substituting the compelling interest. Each file links the others through network.affects_constraints; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
