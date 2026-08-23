% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: Article 3 Negative-Liberty Reading: Procedural Justice Gate on State Deprivation
 *   domain: constitutional/human_rights/political_philosophy
 *
 * SUMMARY:
 *   The Universal Declaration's Article 3 — 'everyone has the right to life,
 *   liberty and security of person' — is a single kernel text that three live
 *   readings instantiate as three different constraints; this file authors
 *   the negative-liberty reading only. On this reading the article prohibits
 *   the state from depriving anyone of life or liberty except through narrow
 *   procedural justice, and 'security' means freedom from state violence
 *   rather than state protection. The standing arrangement under contest is
 *   the actual global practice of state deprivation: capital punishment
 *   retained by dozens of states, administrative and counterterrorism
 *   detention, normalized emergency powers, and expansive official-force
 *   doctrines, all wearing procedural credentials of varying thickness.
 *   Assessed by this reading's own lights, that arrangement takes life and
 *   liberty from individuals far beyond what narrow procedural justice
 *   licenses, so epsilon is authored high against the standing arrangement —
 *   never against the rights-respecting order this reading would build. The
 *   reading's operative program follows from that assessment: abolition of
 *   capital punishment, restrictive doctrine on state self-defense and
 *   emergency powers, expansive due process. The sibling readings
 *   (positive_entitlement_reading, procedural_hybrid_reading) are separate
 *   constraint files sharing this referent; they are not described inside
 *   this one. KEY AGENTS (by structural relationship): -
 *   incumbent_executives: agenda-setting beneficiary
 *   (institutional/constrained) — directs the machinery of deprivation -
 *   state_security_establishments: primary beneficiary and enforcement arm
 *   (institutional/identity_locked) — executes deprivations, holds custody -
 *   capital_defendants: primary target (powerless/trapped) — face categorical
 *   deprivation of life - security_detainees: primary target
 *   (powerless/trapped) — held under exception-clause authority -
 *   ordinary_residents: symmetric seat (moderate/constrained) — protected by
 *   order, exposed to arrest power - constitutional_human_rights_courts:
 *   analytical observer (institutional/analytical) — adjudicates the
 *   procedural gate - abolition_advocacy_movements: organized observer
 *   (organized/identity_locked) — litigates and documents -
 *   populations_under_authoritarian_rule: excluded voice (powerless/trapped)
 *   — heaviest exposure, no seat in the argument
 *
 * KEY AGENTS:
 *   - incumbent_executives: agenda-setting beneficiary (institutional/constrained) — sets deprivation policy, controls emergency powers
 *   - state_security_establishments: primary beneficiary and enforcement arm (institutional/identity_locked) — executes deprivations, collects custody control
 *   - capital_defendants: primary target (powerless/trapped) — face categorical deprivation of life
 *   - security_detainees: primary target (powerless/trapped) — held under exception-clause deprivation
 *   - ordinary_residents: symmetric seat (moderate/constrained) — receives public order, carries universal exposure
 *   - constitutional_human_rights_courts: analytical observer (institutional/analytical) — defines where the procedural gate sits
 *   - abolition_advocacy_movements: organized observer (organized/identity_locked) — external documentation and litigation pressure
 *   - populations_under_authoritarian_rule: excluded voice (powerless/trapped) — bears the severest deprivations absent from the interpretive forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.64).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "Article 3 Negative-Liberty Reading: Procedural Justice Gate on State Deprivation").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'b611fb2c-15cc-49ea-93b9-2340188b4da2').
narrative_ontology:cs_kernel_codification('b611fb2c-15cc-49ea-93b9-2340188b4da2', fixed_text).
narrative_ontology:cs_authority_grounding('b611fb2c-15cc-49ea-93b9-2340188b4da2', lineage).
narrative_ontology:cs_interpretation_layer_present('b611fb2c-15cc-49ea-93b9-2340188b4da2').
narrative_ontology:cs_reading_relation('b611fb2c-15cc-49ea-93b9-2340188b4da2', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('b611fb2c-15cc-49ea-93b9-2340188b4da2', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('b611fb2c-15cc-49ea-93b9-2340188b4da2', foundational, state_deprivation_requires_narrow_procedural_justice).
narrative_ontology:cs_axiom_status(state_deprivation_requires_narrow_procedural_justice, holdable).
narrative_ontology:cs_axiom_grounding('b611fb2c-15cc-49ea-93b9-2340188b4da2', state_deprivation_requires_narrow_procedural_justice, deontological).
narrative_ontology:cs_axiom('b611fb2c-15cc-49ea-93b9-2340188b4da2', foundational, security_is_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_is_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('b611fb2c-15cc-49ea-93b9-2340188b4da2', security_is_freedom_from_state_violence, deontological).
narrative_ontology:cs_axiom('b611fb2c-15cc-49ea-93b9-2340188b4da2', secondary, capital_punishment_is_categorical_violation).
narrative_ontology:cs_axiom_status(capital_punishment_is_categorical_violation, holdable).
narrative_ontology:cs_axiom_grounding('b611fb2c-15cc-49ea-93b9-2340188b4da2', capital_punishment_is_categorical_violation, deontological).
narrative_ontology:cs_reference_frame('b611fb2c-15cc-49ea-93b9-2340188b4da2', prepolitical_individual_immunities).
narrative_ontology:cs_drift_state('b611fb2c-15cc-49ea-93b9-2340188b4da2', contemporary_security_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b611fb2c-15cc-49ea-93b9-2340188b4da2', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, incumbent_executives).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, state_security_establishments).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, capital_defendants).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, security_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, ordinary_residents).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, ordinary_residents).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, state_monopoly_on_legitimate_force).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, national_security_exception_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, punitive_deterrence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the policies under which the state arrests, imprisons, and executes: declare emergencies, direct prosecutors, appoint judges, and decide which deprivations of life and liberty proceed. They answer electorally for security failures but rarely personally for deprivations carried out in their name. Leaving office ends their command of the machinery but not their exposure to its political consequences.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, incumbent_executives, agenda_setter,
    institutional, biographical, constrained, national).

% Police, intelligence services, prison administrations, and military units operating domestically. They take and hold custody of persons, carry out sentences up to death, and defend the legal authorities that let them do so. Their budgets, careers, and institutional identities are bound up in the deprivation powers they exercise; accountability reaches them mainly through the courts they appear before.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_establishments, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, state_security_establishments, agenda_setter).

% Constitutional chambers and regional human rights tribunals hear challenges to deprivations of life and liberty, decide what procedure suffices, and can strike down statutes or award remedies. They do not run the machinery; they define, case by case, where its limits sit. Their dockets are dominated by the questions this arrangement turns on.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, constitutional_human_rights_courts, observer,
    institutional, generational, analytical, continental).

% Persons prosecuted for capital crimes, held through years of appeal under sentence of death. Everything they have — life, contact with family, any future — depends on how narrowly the procedures around their case are drawn. They cannot leave custody, change jurisdiction, or wait out the process; the process is the whole of their world.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, capital_defendants, payer,
    powerless, immediate, trapped, national).

% Persons held under preventive, administrative, or counterterrorism authorities that bypass ordinary charge-and-trial procedure, sometimes for years, sometimes without published charges. Their release turns on executive review and closed evidence they cannot confront. Exit routes — counsel, habeas petitions, diplomatic pressure — exist but run through the same institutions holding them.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, security_detainees, payer,
    powerless, immediate, trapped, national).

% Everyone living under the state's jurisdiction. They receive public order — streets policed, crimes punished — and live mostly untouched by the machinery. But the same powers that hold them safe can be turned on them: any resident is one accusation away from arrest, and few can relocate to another legal order. They vote on the policies and serve on the juries.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, ordinary_residents, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, ordinary_residents, payer).

% Transnational NGOs, bar associations, and religious bodies that document executions and detentions, litigate test cases, and campaign for abolition and procedural reform. Their funding and reputation ride on the issues staying live; they cannot stand down from the cause without dissolving into something else.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, abolition_advocacy_movements, observer,
    organized, generational, identity_locked, global).

% People living where deprivations of life and liberty proceed with little pretense of procedure — mass internment, extrajudicial killing, disappeared persons. They bear the arrangement's heaviest costs yet have no seat in the treaty reviews, courts, or scholarship where its rules are argued; their testimony arrives secondhand, if at all.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, populations_under_authoritarian_rule, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, state_security_establishments).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the exercise of lethal and custodial power through authorized procedure: a single adjudicated monopoly replaces private vengeance, lynch law, and arbitrary executive seizure, so that deprivation of life or liberty happens, when it happens, through gates someone other than the taker controls.
% TRANSFER_FUNCTION: Moves life, liberty, and bodily security from individuals under state jurisdiction into state custody and control — prison, execution, internment — in amounts the recognized procedures authorize; the reading's program would narrow the flow's breadth by withdrawing the unauthorized portion back to individuals.
% ABSENT_VOICES: Populations under authoritarian rule suffer the arrangement's severest deprivations but hold no seat in the treaty reviews, courts, and scholarship where its content is argued; their testimony enters secondhand through NGOs and diaspora networks. Future cohorts subject to precedents set today are likewise unrepresented.
% DISAPPEARANCE_RATIONALE: If the procedural gate vanished overnight, deprivation of life and liberty would proceed by raw executive discretion: detention without charge, punishment without trial, official killing without review. Policing, punishment, and emergency governance would reorganize around unreviewable authority within months, and every safeguard currently litigated would have to be rebuilt from nothing.
% FOUNDING_PROBLEM: Unchecked sovereign power over life and liberty — the interwar and wartime record of extrajudicial killing, secret detention, and lawless punishment that the post-war human rights settlement was written against.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN treaty-body concluding observations, regional-court judgments finding violations, and civil-society execution and detention censuses all attest that state deprivation beyond narrow procedure remains widespread. The state parties' own attestations are partial and self-interested and are not relied on.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the referent is the standing arrangement of state deprivation assessed by this reading's lights — continuing executions across retentionist jurisdictions, exception-clause detention, emergency-rule normalization, and self-defense doctrines that excuse official killing place the arrangement well above any coordination-cost floor. Suppression 0.64 is authored as a raw structural property, unscaled by power or scope: jurisdiction is compulsory, exit is emigration at personal ruin, and the arrangement's maintainers prosecute resistance and control the courts that review them; only extractiveness is scaled downstream, by directionality and spatial scope. Theater 0.42: the procedural machinery performs real gating (habeas grants, acquittals, commutations occur) while a growing share of activity is compliance performance — reports filed, reviews attended, exceptions renewed. Accessibility_collapse 0.60: once the state's monopoly on lawful force is understood, private-vengeance and secession alternatives collapse, though exit-by-emigration survives for the mobile minority. Resistance 0.55: abolition campaigns, strategic litigation, and refugee flight meet the arrangement continuously and occasionally win. The measurement series share one eight-point grid (t = years since 1948); the shape is mid-century improvement (enforcement machinery built, abolition spreading regionally) followed by a post-2000 ratchet (counterterror detention, emergency powers, mass internment) — a drift pattern, not a cycle, so no intermittent-reinforcement analysis applies. Coalition note: the payer seats are individually powerless but have litigated in coalition with the advocacy seat and won real concessions (regional abolition protocols, habeas expansions), which is why resistance sits above the helpless floor. The claimed type is authored independently of these metrics: tangled_rope because the arrangement genuinely coordinates the exercise of lethal and custodial power — a real collective-action problem no private ordering solves — while named parties pay asymmetrically under active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda/beneficiary seats should compute different types from identical text. From capital_defendants and security_detainees, the arrangement is whatever the procedural gate fails to stop: process without protection, experienced as pure taking. From incumbent_executives and state_security_establishments, the same arrangement is legitimate governance — the minimum order that makes everything else possible — and the reading's demands (abolition, narrowed self-defense) register as incapacitation of necessary state function. The courts' seat sees adjudicable doctrine rather than either. The engine derives this divergence from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   State-side seats (incumbent_executives, state_security_establishments) are declared beneficiaries of the standing arrangement and derive directionality near the beneficiary end: the arrangement subsidizes their command over life and liberty. Individual payer seats (capital_defendants, security_detainees) are declared victims with trapped exit, deriving directionality near the full-target end, amplified by their powerlessness and compulsory jurisdiction. ordinary_residents carry a dual declaration (beneficiary of order, payer of exposure) and should land near symmetric. No directionality_overrides are authored: the three institutional seats share a power atom but differ by declared role (agenda_setter, beneficiary, observer), which the structural derivation already reads, so a power-atom-keyed override would misfire across all three simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unchecked sovereign power over life and liberty, crystallized by the mid-century atrocities — is still live: executions continue, exception-clause detention grows, and the corroboration record comes from outside the benefiting parties. No mandatrophy resolution is declared. The classification earns its keep by refusing both available mislabels: a pure-coordination label would erase the named payers, and a pure-extraction label would erase the real gating the machinery performs. Watch-item: if the founding problem ever died (universal abolition plus closed exception clauses) while the texts and tribunals persisted, the arrangement would drift toward ceremonial maintenance — the founding_problem_status x disappearance_verdict mismatch check is the tripwire for that future.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Article 3 kernel governs the standing arrangement — the negative-liberty prohibition authored here, the positive-entitlement obligation, or the procedural-only hybrid?',
    'Doctrinal convergence tracking: watch whether treaty bodies and constitutional courts converge on one reading (e.g., general comments moving toward combined negative and positive dimensions) or whether the contest persists across jurisdictions.',
    'If the positive reading prevails, the victim set shifts from individuals facing deprivation to populations denied provision and epsilon must be re-authored against a welfare-referent arrangement; if the hybrid prevails, substantive deprivation drops out of scope and epsilon falls toward procedural-failure levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This story is one reading of the udhr_article_3 kernel; sibling readings instantiate different constraints with different victim sets over the same referent.').

omega_variable(
    rights_natural_law_or_construction,
    'Is the prohibition on state deprivation a discovered natural limit on sovereignty that would persist without enforcement, or a constructed political settlement maintained only by active enforcement?',
    'Comparative-historical test: examine whether deprivation restraint survives enforcement collapse (failed states, coups, occupation) or reverts immediately to executive discretion.',
    'If natural-law-real, the constraint trends mountain and the declared state-side beneficiaries mark a false summit; if constructed, the tangled_rope classification stands and enforcement decay converts it toward snare at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_natural_law_or_construction, conceptual, 'Natural-law versus constructed-settlement ambiguity of the rights constraint.').

omega_variable(
    jurisdictional_epsilon_variance,
    'Does the standing arrangement carry a single global epsilon, or do abolitionist and retentionist jurisdictions instantiate materially different arrangements that this aggregate story blurs?',
    'Jurisdiction-level re-measurement: if per-jurisdiction epsilon variance exceeds the spread between this story''s endpoint values, decompose into per-regime stories linked by network edges.',
    'Decomposition would yield near-rope classifications in abolitionist jurisdictions and snare-flavored classifications in mass-deprivation jurisdictions, changing the family topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_epsilon_variance, empirical, 'Whether the global aggregate masks divergent jurisdictional arrangements.').

omega_variable(
    emergency_powers_ratchet,
    'Is the post-2000 growth of security exceptions (counterterrorism detention, emergency decrees, pandemic powers) a reversible perturbation or a ratchet permanently converting coordination into extraction?',
    'Continue the measurement grid past the interval end: if base_extractiveness keeps rising after each emergency formally lapses, the ratchet reading is confirmed.',
    'A confirmed ratchet would push payer-seat classifications toward snare and date a tangled_rope-to-snare transition within the next measurement window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_powers_ratchet, empirical, 'Direction of the security-exception trajectory beyond the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_a3_neglib_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t0, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t10, udhr_article_3__negative_liberty_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t10, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t20, udhr_article_3__negative_liberty_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t20, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t30, udhr_article_3__negative_liberty_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t30, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t40, udhr_article_3__negative_liberty_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t40, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t50, udhr_article_3__negative_liberty_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t50, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t60, udhr_article_3__negative_liberty_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t60, observed).
narrative_ontology:measurement(udhr_a3_neglib_tr_t70, udhr_article_3__negative_liberty_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement_basis(udhr_a3_neglib_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(udhr_a3_neglib_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t0, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t10, udhr_article_3__negative_liberty_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t10, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t20, udhr_article_3__negative_liberty_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t20, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t30, udhr_article_3__negative_liberty_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t30, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t40, udhr_article_3__negative_liberty_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t40, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t50, udhr_article_3__negative_liberty_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t50, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t60, udhr_article_3__negative_liberty_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t60, observed).
narrative_ontology:measurement(udhr_a3_neglib_be_t70, udhr_article_3__negative_liberty_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(udhr_a3_neglib_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_a3_neglib_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t0, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t10, udhr_article_3__negative_liberty_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t10, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t20, udhr_article_3__negative_liberty_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t20, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t30, udhr_article_3__negative_liberty_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t30, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t40, udhr_article_3__negative_liberty_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t40, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t50, udhr_article_3__negative_liberty_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t50, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t60, udhr_article_3__negative_liberty_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t60, observed).
narrative_ontology:measurement(udhr_a3_neglib_su_t70, udhr_article_3__negative_liberty_reading, suppression_requirement, 70, 0.64).
narrative_ontology:measurement_basis(udhr_a3_neglib_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: udhr_article_3 decomposes per the epsilon-invariance principle into three readings — negative_liberty (this file), positive_entitlement, procedural_hybrid. All three share the standing referent (actual state practice over life and liberty) but author different epsilon by their own lights: this reading measures deprivation beyond narrow procedural justice; the positive reading measures withheld provision; the hybrid measures procedural failure only. The negative reading is upstream in the lineage (the classical-liberal stratum of the text) and structurally pressures the hybrid, whose habeas and torture provisions inherit its premises; neither it nor the positive reading forecloses the other, and both remain live across jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
