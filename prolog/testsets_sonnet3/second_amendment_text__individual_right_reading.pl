% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Personal Self-Defense Core Protected Activity)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Second Amendment
 *   kernel: the individual-right reading, under which the operative clause
 *   ('the right of the people to keep and bear Arms, shall not be infringed')
 *   guarantees a personal right independent of militia service, with
 *   self-defense as core protected activity. This reading was doctrinally
 *   entrenched by District of Columbia v. Heller (2008) and extended by later
 *   decisions establishing a text-history-tradition test for evaluating
 *   firearms regulations. It is authored here as its own constraint, with its
 *   own extraction profile — the sibling readings
 *   (collective_security_reading, originalist_civic_virtue_reading) are
 *   separate constraints with different beneficiary/victim structures and
 *   different ε, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiary (moderate/mobile) — gains constitutional floor under personal possession
 *   - firearms_industry: institutional beneficiary (organized/arbitrage) — gains demand floor and litigation shield against regulation
 *   - gun_rights_advocacy_organizations: agenda-setter (institutional/arbitrage) — shapes doctrine through strategic litigation
 *   - domestic_violence_survivors: primary payer (powerless/trapped) — bears elevated risk from harder-to-sustain disarmament orders
 *   - communities_with_high_gun_violence_exposure: diffuse payer (powerless/trapped) — bears reduced local regulatory capacity
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates doctrinal boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Personal Self-Defense Core Protected Activity)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '8c5d238a-615c-446d-a050-ba5b056ad42b').
narrative_ontology:cs_kernel_codification('8c5d238a-615c-446d-a050-ba5b056ad42b', fixed_text).
narrative_ontology:cs_authority_grounding('8c5d238a-615c-446d-a050-ba5b056ad42b', lineage).
narrative_ontology:cs_interpretation_layer_present('8c5d238a-615c-446d-a050-ba5b056ad42b').
narrative_ontology:cs_reading_relation('8c5d238a-615c-446d-a050-ba5b056ad42b', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('8c5d238a-615c-446d-a050-ba5b056ad42b', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('8c5d238a-615c-446d-a050-ba5b056ad42b', foundational, operative_clause_independent_of_militia_condition).
narrative_ontology:cs_axiom_status(operative_clause_independent_of_militia_condition, holdable).
narrative_ontology:cs_axiom_grounding('8c5d238a-615c-446d-a050-ba5b056ad42b', operative_clause_independent_of_militia_condition, conventional).
narrative_ontology:cs_axiom('8c5d238a-615c-446d-a050-ba5b056ad42b', foundational, self_defense_as_core_preexisting_natural_right).
narrative_ontology:cs_axiom_status(self_defense_as_core_preexisting_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('8c5d238a-615c-446d-a050-ba5b056ad42b', self_defense_as_core_preexisting_natural_right, deontological).
narrative_ontology:cs_reference_frame('8c5d238a-615c-446d-a050-ba5b056ad42b', founding_era_operative_clause_primacy).
narrative_ontology:cs_drift_state('8c5d238a-615c-446d-a050-ba5b056ad42b', post_bruen_historical_tradition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c5d238a-615c-446d-a050-ba5b056ad42b', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, communities_with_high_gun_violence_exposure).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, law_enforcement_regulators_seeking_disarmament_of_high_risk_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_legislatures_seeking_permit_regimes).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_natural_right_to_self_defense).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, constitutional_protection_independent_of_militia_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own or wish to own firearms for personal self-defense, hunting, or lawful use. Under this reading, their right to keep and bear arms is constitutionally guaranteed regardless of militia affiliation, insulating them from many state and local restrictions. They can relocate to more permissive jurisdictions or challenge restrictive laws in court.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms and ammunition to the individual consumer market this reading protects and expands. Benefits directly from the constitutional floor placed under demand and from the reading's role in defeating many proposed regulations; can shift production and marketing across state lines to exploit favorable jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate, lobby, and shape doctrine to entrench the individual-right reading as controlling law. Sets the interpretive agenda through strategic litigation (test cases designed to reach favorable courts), model legislation, and public messaging. Directly shapes which permit and restriction regimes survive judicial review.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    institutional, generational, arbitrage, national).

% Face elevated lethality risk when abusers retain firearm access; under the individual-right reading, disarmament orders and prohibitions tied to domestic violence findings face heightened constitutional scrutiny and are harder to sustain or enforce quickly. Cannot exit the risk structure — they depend on courts and legislatures to authorize and uphold protective disarmament, which this reading makes more contestable.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_violence_survivors, payer,
    powerless, immediate, trapped, local).

% Live in neighborhoods where firearm availability correlates with elevated injury and death. Local ordinances aimed at reducing firearm density or restricting carry face preemption or invalidation under the individual-right reading, narrowing the community's regulatory tools. Residents cannot easily relocate away from concentrated exposure.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, communities_with_high_gun_violence_exposure, payer,
    powerless, generational, trapped, local).

% Administer background check systems, red-flag laws, and felon/domestic-abuser dispossession statutes. Under the individual-right reading, each such statute must survive individualized constitutional challenge, raising the administrative and litigation cost of removing firearms from persons deemed high-risk, and creating gaps where enforcement stalls pending appellate resolution.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, law_enforcement_regulators_seeking_disarmament_of_high_risk_individuals, payer,
    institutional, biographical, constrained, national).

% Enact may-issue permitting, waiting periods, and carry restrictions to manage public safety. The individual-right reading, especially post-Bruen text-history-tradition analysis, forces these regimes to satisfy a historical-analogue test, invalidating many discretionary permitting schemes and shifting legislative bargaining power toward shall-issue defaults.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_legislatures_seeking_permit_regimes, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, state_legislatures_seeking_permit_regimes, agenda_setter).

% Adjudicate which regulations survive under the individual-right framework, applying historical-tradition analysis to test modern statutes against founding-era analogues. Their doctrinal choices determine how far the reading's protective scope extends into modern regulatory territory.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline that lets individuals plan around a predictable right to acquire and possess firearms for self-defense without needing to demonstrate militia affiliation or organizational membership — reducing case-by-case discretion and arbitrary denial by local officials.
% TRANSFER_FUNCTION: Shifts the burden of justification from the individual (who no longer must show militia-relevant purpose) onto the state (which must now justify each restriction against a historical-tradition test), and correspondingly shifts risk from gun owners facing case-by-case denial onto potential victims of armed individuals whose access becomes harder to restrict.
% ABSENT_VOICES: Domestic violence survivors, survivors of mass shootings, and residents of high-violence-exposure communities are rarely direct parties to the litigation that sets this doctrine; the constitutional cases are typically brought by individual gun owners or advocacy organizations challenging restrictions, not by those bearing the downstream safety costs of the resulting arms-availability floor.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were abandoned overnight in favor of a militia-conditioned reading, state and local governments could reinstate discretionary permitting, categorical possession bans, and broader disarmament authority without triggering strict individual-rights review — the entire body of post-2008 firearms litigation and legislation would need to be rebuilt around a different constitutional floor.
% FOUNDING_PROBLEM: The reading was advanced to resolve genuine ambiguity in the Amendment's text — whether the prefatory militia clause limits or merely explains the operative right — and to protect individuals from arbitrary official denial of a right to self-defense that many understood as pre-existing the Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the gun-rights advocacy network (including some who reject the reading's outcome) corroborate that the operative-clause/prefatory-clause textual ambiguity is genuine and pre-dates modern advocacy; public health researchers and some judges outside the beneficiary set contend the reading's practical effect has shifted from resolving textual ambiguity to foreclosing regulatory experimentation, making the 'problem it solves' status contested rather than settled.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).
:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme because the reading does have a genuine coordination function — it removes arbitrary official discretion over who may exercise a constitutionally enumerated right — but that function is paired with a real, asymmetric cost borne by disarmament-seeking regulators and by populations exposed to firearm risk from persons the reading makes harder to disarm. Suppression (0.38) reflects the doctrine's growing capacity, since Heller and especially since Bruen's historical-analogue test, to foreclose regulatory alternatives that do not have a sufficiently precise founding-era analogue — a suppression mechanism that has hardened over time, which the suppression_requirement series tracks. Theater ratio is low-moderate (0.22): the doctrine does substantive interpretive work in courts, but an increasing share of litigation activity is oriented toward defeating regulation as a categorical matter (performing fidelity to a historical test) rather than adjudicating genuine textual ambiguity, hence the modest upward drift.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry sit near the beneficiary end: the reading subsidizes their position by shifting the burden of constitutional justification onto regulators. Gun rights advocacy organizations are treated as agenda-setters with arbitrage-grade exit because they can select forums and test cases strategically. Domestic violence survivors and high-exposure communities sit near the full-target end: they are trapped (cannot exit the risk exposure), powerless in the litigation process that sets the doctrine, and bear costs through a mechanism (harder disarmament, weaker local regulation) that operates through the same constitutional structure that benefits gun owners. Law enforcement/regulators are institutional but constrained — they retain formal power but face rising compliance costs from the historical-tradition test.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status split into 'contested' status is deliberate: the underlying textual ambiguity this reading resolves (prefatory vs. operative clause) is real and long predates modern advocacy, so the reading cannot be dismissed as pure mandatrophy invention. But the corroboration record shows the reading's practical function has migrated — from resolving genuine 1791-era ambiguity to foreclosing 21st-century regulatory experimentation regardless of the empirical safety record — which is exactly the divergence the tangled_rope classification is built to hold without collapsing into either 'this is just neutral text' or 'this is pure extraction with no coordination value.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_limiting_function,
    'Does the prefatory militia clause function as a substantive limitation on the operative clause''s scope, or purely as an explanatory preamble that does not narrow the right it introduces?',
    'Resolution would require either a definitive historical-linguistic consensus on 18th-century legal drafting conventions for prefatory clauses, or a durable multi-generational judicial consensus that does not later fracture along predictable ideological lines.',
    'If the prefatory clause is found to be substantively limiting, this reading''s core premise (individual right independent of militia service) is undermined and the constraint''s classification would shift toward the collective_security_reading''s structure, with a different beneficiary/victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_limiting_function, conceptual, 'Whether the militia clause is a limitation or mere preamble — the central textual dispute the kernel turns on.').

omega_variable(
    coordination_extraction_separability,
    'Is the reading''s coordination function (predictable protection against arbitrary official denial of a recognized right) separable from its extraction function (foreclosure of regulations targeting demonstrated risk factors like domestic violence findings)?',
    'Comparative analysis of jurisdictions or historical periods where individual possession was protected via statute (not constitutional floor) alongside robust risk-based disprivileging (felon/DV dispossession) without facing heightened constitutional scrutiny — if safety outcomes and possession stability both hold, the functions are separable.',
    'If separable, current extraction is unnecessary to the coordination benefit and could be reduced without sacrificing the reading''s core protective function; if inseparable, the tangled_rope''s extraction component is closer to inherent to the coordination it provides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether extraction from risk-exposed populations is a necessary cost of the individual-right coordination benefit or a severable excess.').

omega_variable(
    historical_tradition_test_stability,
    'Will the post-Bruen text-history-tradition methodology stabilize as a durable interpretive framework, or will its indeterminacy (which historical analogues count, at what level of generality) produce enough inconsistent lower-court outcomes that it is itself revised or abandoned?',
    'Track circuit split resolution rates and Supreme Court intervention frequency over the next decade; a persistently high reversal/split rate would indicate the test is unworkable as currently specified.',
    'If the test proves unstable, the reading''s suppression mechanism (foreclosing regulations lacking precise historical analogues) would weaken, reducing the current trajectory of rising suppression_requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_tradition_test_stability, empirical, 'Whether the historical-analogue methodology that operationalizes this reading will prove durable or will itself require revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_text__individual_right_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1970, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_text__individual_right_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.21).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_text__individual_right_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__individual_right_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1970, second_amendment_text__individual_right_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement_basis(seco_be_t1970, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.32).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_text__individual_right_reading, base_extractiveness, 2016, 0.37).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.4).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_text__individual_right_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__individual_right_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1970, second_amendment_text__individual_right_reading, suppression_requirement, 1970, 0.16).
narrative_ontology:measurement_basis(seco_su_t1970, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_text__individual_right_reading, suppression_requirement, 2016, 0.33).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.36).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2025, second_amendment_text__individual_right_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(seco_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language 'Second Amendment meaning' claim into structurally distinct kernel readings, per the ε-invariance principle. individual_right_reading (this story) authors a moderate, tangled_rope-classified ε (0.42) reflecting a genuine anti-arbitrariness coordination function paired with asymmetric extraction from disarmament-dependent safety interests. collective_security_reading and originalist_civic_virtue_reading are separate files with different beneficiary/victim sets and different ε values — they are not alternate measurements of this same constraint. All three are linked bidirectionally via affects_constraints since a shift in doctrinal dominance among the readings directly reallocates which populations bear extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
