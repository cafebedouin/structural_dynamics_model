% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual-Necessity Reading of the Humane Treatment Standard (Common Article 3)
 *   domain: legal/international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions requires humane treatment
 *   of persons taking no active part in hostilities — a floor every party
 *   owes every detainee. This story authors the contextual-necessity reading
 *   of that standard: the floor is real but conditional — an executive may
 *   determine that national security imperatives override it for designated
 *   detainees, and 'humane treatment' is then defined operationally by the
 *   agencies running the interrogation. The arrangement has a genuine
 *   coordination half: for the covered detainee set the floor delivers
 *   notification, visitation, and humane conditions, and gives all parties a
 *   common minimum they can reciprocate. It also has an asymmetric half: the
 *   necessity designation removes the floor's protection from a set chosen by
 *   the very agencies that collect the interrogation's product. This file is
 *   one member of a three-reading family over the humane_treatment_standard
 *   kernel (with absolute_prohibition and proportionality_balancing authored
 *   as separate stories); its ε describes only this conditional arrangement
 *   as it operates.
 *
 * KEY AGENTS:
 *   - security_intelligence_agencies: agenda-setter (institutional/arbitrage) — administers the override, defines 'humane' operationally, designates targets, collects the intelligence product
 *   - state_executive_branches: primary beneficiary with agenda-setting authority (institutional/arbitrage) — issues necessity determinations and legal cover
 *   - designated_high_value_detainees: primary target (powerless/trapped) — bear the override directly, cannot contest designation
 *   - baseline_protected_detainees: coordinated beneficiaries (powerless/trapped) — the floor serves them
 *   - allied_governments: secondary beneficiaries and bearers (institutional/mobile) — intelligence in, sovereignty exposure out through their own courts
 *   - field_military_personnel: diffuse bearers with secondary benefit (moderate/constrained) — liability asymmetry: discretion up, exposure down
 *   - icrc_and_access_monitors: excluded monitors (organized/constrained) — denied access exactly where the override operates
 *   - human_rights_treaty_bodies: analytical observers (institutional/analytical) — hold the non-derogable reading from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual-Necessity Reading of the Humane Treatment Standard (Common Article 3)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "legal/international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'ea620626-c21f-40a6-a0a5-06f57118c05a').
narrative_ontology:cs_kernel_codification('ea620626-c21f-40a6-a0a5-06f57118c05a', fixed_text).
narrative_ontology:cs_authority_grounding('ea620626-c21f-40a6-a0a5-06f57118c05a', extraction).
narrative_ontology:cs_interpretation_layer_present('ea620626-c21f-40a6-a0a5-06f57118c05a').
narrative_ontology:cs_reading_relation('ea620626-c21f-40a6-a0a5-06f57118c05a', humane_treatment_standard__humane_treatment_absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('ea620626-c21f-40a6-a0a5-06f57118c05a', humane_treatment_standard__humane_treatment_proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('ea620626-c21f-40a6-a0a5-06f57118c05a', foundational, necessity_overrides_detention_floor).
narrative_ontology:cs_axiom_status(necessity_overrides_detention_floor, holdable).
narrative_ontology:cs_axiom_grounding('ea620626-c21f-40a6-a0a5-06f57118c05a', necessity_overrides_detention_floor, empirically_contingent).
narrative_ontology:cs_axiom('ea620626-c21f-40a6-a0a5-06f57118c05a', secondary, humane_treatment_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ea620626-c21f-40a6-a0a5-06f57118c05a', humane_treatment_context_dependent, conventional).
narrative_ontology:cs_reference_frame('ea620626-c21f-40a6-a0a5-06f57118c05a', security_overridable_humane_floor).
narrative_ontology:cs_drift_state('ea620626-c21f-40a6-a0a5-06f57118c05a', post_exposure_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea620626-c21f-40a6-a0a5-06f57118c05a', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_intelligence_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_executive_branches).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, allied_governments).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, baseline_protected_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, designated_high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, field_military_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, field_military_personnel).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, allied_governments).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, executive_necessity_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__contextual_necessity, security_exception_interpretivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the interrogation programs the override authorizes: they draft the operational definitions of 'humane treatment,' decide which detainees fall under necessity designations, select and operate the interrogation sites, and collect the resulting intelligence. When oversight or litigation threatens a program, they reclassify, relocate, or re-badge it, including through proxy arrangements in other jurisdictions. Their exit is flexible in form though not in mission: they can move a program anywhere, but not the requirement to run one.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_intelligence_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Issue the necessity determinations and the legal memoranda that make the override lawful domestically, and receive the actionable intelligence and the political credit for acting decisively against threats, while day-to-day administration sits with the agencies. They can terminate the arrangement by directive, and have under exposure pressure, but termination draws institutional resistance, allied complications, and accusations of weakness, so it is politically expensive even where legally simple.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_executive_branches, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, state_executive_branches, agenda_setter).

% Receive intelligence shared from the programs and, in some cases, host facilities or permit transit. They bear sovereignty costs: their own courts have found them liable for hosting (European Court of Human Rights judgments against Poland, Lithuania, and Romania), and they can refuse transit or hosting — several have — at the price of friction with the primary state. Their payer position is felt in their own courtrooms.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, allied_governments, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, allied_governments, payer).

% The large majority of detainees in a conflict, held under the ordinary floor: notification of capture, monitor visitation, humane conditions, and eventual release or transfer. The floor's rules are the rules they are held by. They have no exit from custody, and what protection they have depends entirely on the floor continuing to apply to them.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, baseline_protected_detainees, beneficiary,
    powerless, immediate, trapped, global).

% Detainees designated under necessity determinations — typically those believed to hold knowledge of leadership, networks, or imminent operations. For them the floor is defined by their captors: they are held in sites closed to monitors, subjected to the authorized program, and their designation can be made, revised, or revoked entirely at the designating authority's discretion. They have no exit of any kind and no forum in which to contest the designation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, designated_high_value_detainees, payer,
    powerless, immediate, trapped, global).

% Soldiers and interrogators who implement detention practice. For ordinary detainees they work under the floor's clear rules, which protect them as much as the detainees. Where programs are improvised or the override's legal cover fails, criminal liability and career ruin land on them rather than on the officials who authorized the discretion — the consistent pattern of prosecutions after detention scandals. They cannot leave the service mid-deployment, and refusing an order carries its own costs.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, field_military_personnel, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, field_military_personnel, beneficiary).

% The ICRC and allied monitor organizations operate a confidential visiting system that reaches most detention sites. Their access is denied or delayed exactly at the designated sites where the override operates — the confidentiality bargain that gives them access everywhere else excludes them here. They would insist on unconditional floor application and access to all sites; they are not in the room where designations are made.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, icrc_and_access_monitors, excluded,
    organized, generational, constrained, global).

% UN treaty bodies, special procedures, and regional courts receive state reports, hear complaints, and issue findings on detention treatment. They read the floor as non-derogable and treat the override as a violation of it; their findings carry normative and reputational force but limited direct enforcement, and they analyze the arrangement from outside any party's command structure.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_intelligence_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For parties to a non-international armed conflict, the Common Article 3 floor coordinates a minimum standard of detainee treatment that each side can observe and reciprocate, giving commanders a bright-line rule for detention discipline. Under the contextual-necessity reading, the override adds a second coordination function for the adopting state: a lawful, internally authorized channel for interrogating designated detainees, replacing ad hoc illegality with a managed program.
% TRANSFER_FUNCTION: Moves bodily security, dignity, and legal protection away from detainees designated under necessity determinations and toward security agencies as intelligence product; moves interpretive discretion over 'humane treatment' from courts and treaty bodies to executive legal offices; moves political cover from legal advisors to executives. For the covered detainee set, the baseline continues to deliver protection.
% ABSENT_VOICES: The designated detainees themselves have no seat anywhere in the process that designates them — the necessity determination is made entirely by the parties who benefit from running the interrogation. ICRC delegates are denied access to the designated sites, so the visiting system that reaches every other place of detention stops at exactly the places the override governs. Proponents of the non-derogable reading are excluded from the memo-writing process that defines 'humane treatment' operationally.
% DISAPPEARANCE_RATIONALE: If the conditional structure — the necessity override and the discretion it grants — vanished overnight, interrogation governance would reorganize: agencies would lose the lawful channel and face a binary of absolute compliance or overt lawbreaking; designation-based site segregation would collapse; monitor access would expand to all detention sites; and the covered set's protections would become unconditional. The 1949 floor itself would persist — what rearranges is the override apparatus and the discretion economy built on it.
% FOUNDING_PROBLEM: The kernel (Common Article 3, 1949) was built to solve the problem of detainees in non-international conflicts held wholly outside any legal protection. This reading was built to solve a further claimed problem: that an absolute floor leaves the state unable to interrogate high-value detainees who hold time-critical knowledge of imminent threats, and that the floor therefore requires a necessity override for designated cases.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and executive legal offices attest liveness, citing time-critical threat scenarios. Outside the benefiting parties, the corroboration runs the other way: the ICRC's 2007 detained-intelligence study, UN Committee Against Torture concluding observations, and the 2014 SSCI program review document that the override in practice was applied broadly rather than confined to imminent-threat scenarios, and interrogation-efficacy research disputes that coercion yields intelligence unavailable by other means. Partial external corroboration exists, and it attests against liveness; no external source attests for it.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the override transfers bodily security and legal protection from a designated class to the agencies that hold the designation pen, and the designation criterion ('national security imperatives') is defined by the beneficiary. Suppression (0.68) is structural: detainees in designated sites have zero exit and no contest forum, monitors are excluded by the same confidentiality machinery that gives them access elsewhere, and programs survive exposure through reclassification and relocation. Theater (0.5) reflects the definitional work the arrangement requires — 'enhanced interrogation is not torture,' 'humane treatment' redefined operationally, closures announced while practices migrate to proxies. Accessibility_collapse (0.45) is low for an actively enforced arrangement because the alternative reading remains fully live in treaty law — the torture prohibition is non-derogable under the CAT and ICCPR — so the arrangement suppresses the alternative operationally within adopting states but cannot collapse it. Resistance (0.6) is sustained: ICRC reporting, treaty-body findings, litigation (Hamdan, Boumediene, ECHR judgments against hosting states), a legislative investigation (SSCI), and professional-body repudiation. Suppression_requirement is authored as a series because the story specifically tracks enforcement-capacity change: build-up 2001–2004, partial decay under exposure and litigation 2006–2016, re-legitimation in the 2020s. All three series run on one shared ten-point grid (1949–2026) so every tracked metric is authored at every examined time point. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agency seat the arrangement is a lawful channel it built and administers — the override is what makes interrogation governable at all. From the designated-detainee seat the floor exists but is defined, for them, by their captor: protection conditional on the discretion of the party that benefits from suspending it. From the field seat the standard is a liability asymmetry — interpretive discretion concentrates upward, criminal exposure lands downward. From the treaty-body seat the arrangement is a non-derogable norm wearing a domestic legal costume. Same structure, four different experiences; the engine computes this divergence from the structural data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: security_intelligence_agencies (collects the intelligence, holds the designation pen, arbitrage exit — d near the beneficiary end), state_executive_branches (collects intelligence and political cover, can terminate at will — low d), allied_governments (receive intelligence with mobile exit but bear sovereignty costs — low-moderate d), baseline_protected_detainees (the floor subsidizes them — low d despite captivity; trapped beneficiaries are still beneficiaries). Targets: designated_high_value_detainees (trapped victims of a designation they cannot contest — d near the full-target end; trapped targets sit at the extreme). field_military_personnel are listed as victims, but the derivation from victim status alone would overstate their d: their costs are diffuse (liability exposure, moral injury) and they also receive the floor's clarity for ordinary detention, so a directionality override sets moderate-power agents to d=0.58 — collateral bearers, not the object of the transfer. ICRC monitors are excluded rather than coordinated; their exclusion at the designated sites is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope guards both failure directions. Against rope-laundering: the floor's genuine coordination function must not absorb the override's asymmetric costs — the same agencies that collect the product draw the designation, so the coordination story cannot be read as the whole structure. Against snare-overshoot: the floor genuinely protects the covered set (visitation, notification, and conditions hold for the majority of detainees), so the coordination half is real, not cover. The mandatrophy question turns on the founding problem's liveness: the necessity premise is contested and the external corroboration runs against it. If the empirical premise fails (omega necessity_scenario_frequency) while the discretion economy persists, the arrangement's mandate has outlived its function and the constraint drifts toward snare; if the override's existence corrodes the floor everywhere (omega baseline_genuineness_under_override), the coordination half collapses and the drift completes. The R5 mismatch consumer reads status=contested × verdict=world_rearranges: no dead-mandate flag fires yet, but the contested status is the live tripwire. Receipt surface: gains demonstrably accrue to security_intelligence_agencies (intelligence product, budget, institutional discretion), so gain_flow names that seat rather than 'diffuse'; fixing_cost is authored prohibitive because the executive could terminate the override by directive, but the perceived cost — institutional resistance, intelligence-community warnings, allied exposure, and surrender of the override option itself — exceeds the benefit as the fixer assesses it, and persistence across administrations despite exposure is the behavioral evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading (contextual_necessity) of the kernel humane_treatment_standard; what would change structurally if a party adopted the sibling reading absolute_prohibition or proportionality_balancing instead?',
    'Adoption of a sibling reading by a party''s legal framework (treaty-body jurisprudence accepted domestically, or domestic codification of non-derogability or of case-by-case balancing); compare victim sets and ε across the sibling stories.',
    'Under absolute_prohibition the designated set merges into the protected set — the victim set expands to all detainees in the adopting party''s custody, the override apparatus loses its legal object, and ε falls toward the baseline''s coordination cost. Under proportionality_balancing designation discretion is replaced by case-by-case adjudication, shrinking but not eliminating the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: which reading of the humane-treatment kernel this story instantiates, and what sibling adoption would change.').

omega_variable(
    derogability_disagreement_location,
    'Where exactly do the readings disagree — is the contest located in the baseline''s existence or in its derogability?',
    'Structural comparison of the three sibling stories: all three accept the 1949 floor''s existence; they diverge solely on whether executive necessity determinations can suspend it for designated persons.',
    'Locating the disagreement in derogability rather than the floor means this reading''s asymmetric costs live entirely in the override machinery — a remedy that removes only the override (not the baseline) eliminates the asymmetry while preserving the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derogability_disagreement_location, conceptual, 'The readings'' disagreement is localized in the derogability clause, not in the baseline itself.').

omega_variable(
    necessity_scenario_frequency,
    'How often do genuine time-critical necessity scenarios occur, and does coercive interrogation of designated detainees yield actionable intelligence unavailable through non-coercive means?',
    'Interrogation-efficacy research, declassified program assessments (e.g., the 2014 SSCI review), and case-level audits comparing coercive and rapport-based interrogation outcomes.',
    'If necessity scenarios are rare and coercion ineffective, the override is discretion-capture and the constraint drifts toward snare — extraction with the coordination story failing. If both hold, part of the measured cost is the operating cost of a real security function and the tangled_rope reading is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_scenario_frequency, empirical, 'The empirical premise of the necessity override: scenario frequency and coercive efficacy.').

omega_variable(
    designation_discretion_capture,
    'Is necessity designation driven by threat assessment, or by institutional incentive — do agencies designate the detainees whose interrogation they want to run?',
    'Audit of designation records against published threat criteria; comparison of designation rates and criteria across oversight-intensity regimes and across adopting states.',
    'If designation tracks institutional preference rather than threat, the designated set is drawn by the beneficiary — the transfer is self-dealing, supporting the high end of the extractiveness range and snare-drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designation_discretion_capture, empirical, 'Whether the necessity designation is threat-driven or self-dealing.').

omega_variable(
    baseline_genuineness_under_override,
    'Does the Common Article 3 floor genuinely protect the covered detainee set, or does the override''s existence corrode the floor everywhere through definitional creep?',
    'Compare treatment outcomes, monitor access, and complaint rates for covered versus designated detainees across adopting and non-adopting states over the interval.',
    'If the floor corrodes globally, the coordination half is theatrical, the authored theater_ratio is understated, and the constraint completes the drift toward snare or piton; if the floor holds for the covered set, the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_genuineness_under_override, empirical, 'Whether the baseline coordination function survives the override''s existence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(huma_tr_t13, humane_treatment_standard__contextual_necessity, theater_ratio, 13, 0.12).
narrative_ontology:measurement(huma_tr_t26, humane_treatment_standard__contextual_necessity, theater_ratio, 26, 0.18).
narrative_ontology:measurement(huma_tr_t38, humane_treatment_standard__contextual_necessity, theater_ratio, 38, 0.28).
narrative_ontology:measurement(huma_tr_t52, humane_treatment_standard__contextual_necessity, theater_ratio, 52, 0.4).
narrative_ontology:measurement(huma_tr_t55, humane_treatment_standard__contextual_necessity, theater_ratio, 55, 0.5).
narrative_ontology:measurement(huma_tr_t57, humane_treatment_standard__contextual_necessity, theater_ratio, 57, 0.48).
narrative_ontology:measurement(huma_tr_t62, humane_treatment_standard__contextual_necessity, theater_ratio, 62, 0.52).
narrative_ontology:measurement(huma_tr_t67, humane_treatment_standard__contextual_necessity, theater_ratio, 67, 0.55).
narrative_ontology:measurement(huma_tr_t77, humane_treatment_standard__contextual_necessity, theater_ratio, 77, 0.5).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(huma_be_t13, humane_treatment_standard__contextual_necessity, base_extractiveness, 13, 0.15).
narrative_ontology:measurement(huma_be_t26, humane_treatment_standard__contextual_necessity, base_extractiveness, 26, 0.2).
narrative_ontology:measurement(huma_be_t38, humane_treatment_standard__contextual_necessity, base_extractiveness, 38, 0.35).
narrative_ontology:measurement(huma_be_t52, humane_treatment_standard__contextual_necessity, base_extractiveness, 52, 0.5).
narrative_ontology:measurement(huma_be_t55, humane_treatment_standard__contextual_necessity, base_extractiveness, 55, 0.74).
narrative_ontology:measurement(huma_be_t57, humane_treatment_standard__contextual_necessity, base_extractiveness, 57, 0.66).
narrative_ontology:measurement(huma_be_t62, humane_treatment_standard__contextual_necessity, base_extractiveness, 62, 0.6).
narrative_ontology:measurement(huma_be_t67, humane_treatment_standard__contextual_necessity, base_extractiveness, 67, 0.58).
narrative_ontology:measurement(huma_be_t77, humane_treatment_standard__contextual_necessity, base_extractiveness, 77, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(huma_su_t13, humane_treatment_standard__contextual_necessity, suppression_requirement, 13, 0.28).
narrative_ontology:measurement(huma_su_t26, humane_treatment_standard__contextual_necessity, suppression_requirement, 26, 0.3).
narrative_ontology:measurement(huma_su_t38, humane_treatment_standard__contextual_necessity, suppression_requirement, 38, 0.42).
narrative_ontology:measurement(huma_su_t52, humane_treatment_standard__contextual_necessity, suppression_requirement, 52, 0.55).
narrative_ontology:measurement(huma_su_t55, humane_treatment_standard__contextual_necessity, suppression_requirement, 55, 0.76).
narrative_ontology:measurement(huma_su_t57, humane_treatment_standard__contextual_necessity, suppression_requirement, 57, 0.7).
narrative_ontology:measurement(huma_su_t62, humane_treatment_standard__contextual_necessity, suppression_requirement, 62, 0.62).
narrative_ontology:measurement(huma_su_t67, humane_treatment_standard__contextual_necessity, suppression_requirement, 67, 0.58).
narrative_ontology:measurement(huma_su_t77, humane_treatment_standard__contextual_necessity, suppression_requirement, 77, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, cat_absolute_torture_ban).

% DUAL FORMULATION NOTE:
% The colloquial label 'humane treatment under Common Article 3' decomposes into three structurally distinct constraints — one per reading of the humane_treatment_standard kernel — because each reading yields a different victim set and a different ε: absolute_prohibition (victim set empty in principle; ε near the baseline's coordination cost), proportionality_balancing (victim set determined case by case; intermediate ε), and this story's contextual_necessity (victim set drawn by executive designation; substantially extractive). This reading is downstream of the other two in argument structure: necessity claims are cited as the reason the absolute reading is 'unworkable,' and the extreme cases the necessity reading supplies are what the balancing reading must accommodate. Each file links its siblings via affects_constraints; ε is authored per reading and never averaged across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
