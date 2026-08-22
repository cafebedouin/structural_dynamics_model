% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate Authority (Bodily Autonomy Primary Reading)
 *   domain: public health law / constitutional rights / bioethics
 *
 * SUMMARY:
 *   A public health mandate regime — compulsory vaccination requirements
 *   enforced through employment conditions, school and venue entry rules,
 *   military regulations, and fines — is read here through the
 *   bodily_autonomy_primary reading of the public_health_mandate_authority
 *   kernel: the mandate is a categorical violation of bodily sovereignty, and
 *   no collective benefit justifies non-consensual medical intervention. The
 *   story's epsilon referent is the standing mandate arrangement itself,
 *   assessed by this reading's own lights — never the voluntary,
 *   consent-based arrangement this reading would put in its place. This story
 *   is one member of a three-story kernel family: the public_health_primary
 *   sibling reads the same arrangement as a duty to protect the vulnerable
 *   commons (different victim set, far lower epsilon over the identical
 *   referent), and the proportionality_reading reads legitimacy as
 *   severity-indexed. Each sibling is a separate constraint with its own
 *   epsilon, its own victim set, and its own classification; they are linked
 *   via network.affects_constraints, and the divergence among them is the
 *   measured content of the kernel contest. Claim and metrics are authored
 *   independently: claimed_type states this reading's structural assessment;
 *   the metrics describe the arrangement's operation as this reading assesses
 *   it; the engine computes per-seat classifications from the structural
 *   data, and divergence between claim and computed type is signal, not
 *   error.
 *
 * KEY AGENTS:
 *   - unvaccinated_objectors: Primary target (moderate/constrained) — bear the arrangement's penalties; their consent is the decision input the arrangement does not accept
 *   - public_health_agencies: Primary beneficiary and agenda setter (institutional/arbitrage) — collect compliance and authority; set exemption criteria; the seat the arrangement's gains demonstrably accrue to
 *   - immunocompromised_high_risk: Protected beneficiary (powerless/trapped) — receive reduced exposure while bearing no penalty; excluded from the victim set under this reading (no duty to protect via bodily invasion)
 *   - public_health_advocates: Advocacy beneficiary (organized/mobile) — receive the advocated policy outcome; zero extractiveness on this seat, nothing in the arrangement is imposed on them
 *   - military_service_members: Secondary target (powerless/trapped) — total employment relationship forecloses market exit
 *   - vaccine_injury_claimants: Secondary target (powerless/trapped) — bear the intervention's physical harm through capped administrative recourse
 *   - mandate_compliant_employers: Beneficiary (powerful/mobile) — gain standardization and liability shielding; can exit enforcement when costs rise
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicate the sovereignty boundary; explicitly reject the categorical premise while upholding most of the arrangement
 *   - medical_ethics_dissenters: Excluded voice (moderate/constrained) — informed-consent objections with no seat in mandate design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate Authority (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public health law / constitutional rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'fe2596d6-d057-4f77-8551-e0fee54d7147').
narrative_ontology:cs_kernel_codification('fe2596d6-d057-4f77-8551-e0fee54d7147', formalized).
narrative_ontology:cs_authority_grounding('fe2596d6-d057-4f77-8551-e0fee54d7147', lineage).
narrative_ontology:cs_interpretation_layer_present('fe2596d6-d057-4f77-8551-e0fee54d7147').
narrative_ontology:cs_reading_relation('fe2596d6-d057-4f77-8551-e0fee54d7147', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('fe2596d6-d057-4f77-8551-e0fee54d7147', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('fe2596d6-d057-4f77-8551-e0fee54d7147', foundational, categorical_bodily_sovereignty).
narrative_ontology:cs_axiom_status(categorical_bodily_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('fe2596d6-d057-4f77-8551-e0fee54d7147', categorical_bodily_sovereignty, deontological).
narrative_ontology:cs_axiom('fe2596d6-d057-4f77-8551-e0fee54d7147', secondary, informed_consent_non_waivable_by_collective_decision).
narrative_ontology:cs_axiom_status(informed_consent_non_waivable_by_collective_decision, holdable).
narrative_ontology:cs_axiom_grounding('fe2596d6-d057-4f77-8551-e0fee54d7147', informed_consent_non_waivable_by_collective_decision, deontological).
narrative_ontology:cs_reference_frame('fe2596d6-d057-4f77-8551-e0fee54d7147', inviolable_bodily_sovereignty_baseline).
narrative_ontology:cs_drift_state('fe2596d6-d057-4f77-8551-e0fee54d7147', contemporary_post_covid_mandate_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('fe2596d6-d057-4f77-8551-e0fee54d7147', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_high_risk).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, mandate_compliant_employers).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, vaccine_injury_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, military_service_members).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, jacobson_deference_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, collective_immunity_public_good_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who decline the mandated medical intervention. They face employment termination, denial of school and venue entry, fines, and in some settings professional decertification. Medical and religious exemptions exist but are narrow, discretionary, and unevenly administered. Some can relocate to a non-mandating jurisdiction or employer; for most that cost is prohibitive. Their consent to the intervention is the one decision input the arrangement is built not to accept.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_objectors, payer,
    moderate, biographical, constrained, national).

% Promulgate and enforce mandate orders, define exemption criteria, and collect compliance. They gain enforcement authority, measurable compliance rates, and budget justification from the arrangement's operation, and they bear political backlash when enforcement intensifies. They can convert mandates to recommendations, sunset emergency orders, or shift enforcement intensity — adjusting the arrangement sits within their administrative power.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, beneficiary).

% Medically vulnerable individuals who receive reduced exposure risk as population immunity rises. They bear none of the arrangement's penalties; the costs land entirely on those who decline. Their vulnerability is not escapable, and their protection depends on others' compliance rather than on anything they can purchase or do themselves. Under the consent standard this story applies, the benefit they receive cannot be collected by invading another person's body, and they are not recorded among those bearing the arrangement's harms.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_high_risk, beneficiary,
    powerless, biographical, trapped, national).

% Employers that adopt or comply with mandate requirements. They gain a standardized workforce health status, reduced liability exposure, and a defensible rule — the requirement is the government's, not their choice. Several dropped their requirements once political and litigation costs rose, showing that exit from enforcement is available to them in a way it is not for those subject to the requirement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, mandate_compliant_employers, beneficiary,
    powerful, biographical, mobile, national).

% Epidemiologists, public-health-school ethicists, and civic organizations that campaigned for mandates. They receive the policy outcome they advocated and bear none of its penalties. Their advocacy shapes exemption design, enforcement scope, and the public justification offered for the arrangement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates, beneficiary,
    organized, generational, mobile, national).

% Service members subject to compulsory vaccination under military regulations. Refusal brings administrative separation, pay forfeiture, and potential prosecution under the uniform code. The employment relationship is total — no market exit exists — so their position differs categorically from civilian objectors who can at least change employers or jurisdictions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, military_service_members, payer,
    powerless, biographical, trapped, national).

% Individuals who suffered adverse events following a mandated vaccination. Their claims route through dedicated administrative programs with high denial rates, damage caps, and no jury trial; ordinary tort recourse against manufacturers is largely foreclosed for these products. They bear the intervention's physical harm while the arrangement's design routes their compensation through channels they do not control.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_injury_claimants, payer,
    powerless, biographical, trapped, national).

% Courts adjudicating the boundary between state police powers and bodily integrity. They have upheld most mandates under a century-old deference standard while explicitly rejecting arguments that consent is categorically required. Their doctrine is where the arrangement's limits are drawn and redrawn.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Bioethicists and medical-ethics bodies invoking Nuremberg-code and Belmont-report informed-consent norms against the mandates. They published objections and testified where invited, but held no seat in the emergency-order design that produced the requirements. Their exclusion from the design conversation is part of how the requirements proceeded as administrative acts rather than consensual ones.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, medical_ethics_dissenters, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the free-riding problem in epidemic disease control: population-level protection requires simultaneous individual medical decisions, and voluntary uptake underproduces because each person's protection depends on others' compliance. The mandate aligns individual decisions to a collective threshold through compulsion.
% TRANSFER_FUNCTION: Moves decisional authority over medical treatment of one's own body from unvaccinated individuals to public health authorities and mandating institutions; moves compliance costs (job loss, enrollment denial, venue exclusion, fines, military separation) onto refusers while distributing reduced-disease-risk benefits across the protected population.
% ABSENT_VOICES: Unvaccinated objectors were heard in comment periods but their consent was excluded as a decision input by design — the mandate's premise treats their refusal as the defect to be overridden. Medical-ethics dissenters invoking informed-consent norms held no seat in emergency-order design. Vaccine-injured claimants had no seat in the risk allocation the mandate imposed. The emergency-authorization statute's refusal-option requirement was argued in litigation but never operationalized as policy.
% DISAPPEARANCE_RATIONALE: Emergency-preparedness doctrine, school-entry requirements, healthcare-employment conditions, military medical readiness, and agency enforcement capacity are organized around the mandate authority. If it vanished overnight, public health would rebuild around voluntary and incentive-based uptake, employers would rewrite workforce rules, and the agencies' emergency toolkit would contract; the protected seats would lose risk reduction they currently receive without bearing any cost. The rearrangement would be substantial — which is the structural fact, whatever the sibling readings make of its justification.
% FOUNDING_PROBLEM: Epidemic disease control under free-riding: voluntary vaccination underproduces relative to the population threshold needed to protect those who cannot be vaccinated, as shown by smallpox mortality in the Jacobson era and by measles and COVID outbreaks since.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological literature and the historical mortality record attest the founding problem is live (smallpox mortality, measles resurgence, COVID mortality data). Courts in the Jacobson line acknowledged the problem while adjudicating the means. This reading's own holders attest the problem is live while denying it justifies non-consensual intervention — corroboration of the problem, not of the arrangement. No corroborator outside the benefiting parties attests that the founding problem justifies the categorical override of consent; that attestation exists only inside the benefiting set.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the arrangement's transfer is decisional authority over one's own body, taken without consent — under this reading's lights the transfer is total in kind even where its breadth varies. Suppression is equally high (0.85): persistence depends on termination, exclusion, fines, and military process rather than on participant preference, and the enforcement machinery visibly ratcheted during the COVID interval before partial rollback. Theater is moderate (0.45): the vaccination itself is functionally real, but a growing share of the justification apparatus — collective-benefit framing offered as sufficient warrant, compliance dashboards, rhetoric that outlasted the evidence it cited — performs warrant rather than provides it. Accessibility collapse is moderate (0.6): exemptions, relocation, and employer variation leave real alternatives, but each is costly, discretionary, or unavailable to the trapped seats. Resistance is high (0.7): mass litigation, protests, state-level statutory bans, and documented noncompliance. The measurement series runs on one shared grid (t=0..120, Jacobson 1905 to present) so every tracked metric is authored at every point: the t=80 dip records the interval in which the categorical limit was visibly respected — no HIV mandate was imposed despite severity — and the t=120 peak records the COVID enforcement ratchet and its partial persistence. The arrangement's gains demonstrably accrue to the agencies seat (compliance rates, enforcement precedent, budget justification), so gain_flow names that seat rather than diffuse; fixing is prohibitive for the operative national fixers (courts and federal agencies), since removal requires overturning a century of deference doctrine and dismantling emergency-preparedness machinery — state-level statutory bans are cheap only for fixers who do not bear the preparedness function.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different types from the same structure. From the unvaccinated, military, and injury-claimant seats the arrangement is uncompensated taking: the transfer is imposed, not exchanged, and exit is trapped or prohibitively costly. From the agencies, advocates, and employers seats the same arrangement is coordination they administer or benefit from at no bodily cost to themselves. Constitutional courts occupy the analytical seat and have explicitly rejected this reading's categorical premise while upholding most of the arrangement's operation. The engine computes this divergence per seat from the authored structural data; this story's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: public_health_agencies (collect compliance and authority, control the rules), public_health_advocates (receive the advocated outcome and bear no penalty — this realizes the expected structural delta of zero extractiveness on public-health-primary advocates, since nothing in the arrangement is imposed on them), mandate_compliant_employers (gain standardization and liability shielding), and immunocompromised_high_risk (receive protection while bearing no penalty — the delta places them in the beneficiary set rather than the victim set: this reading denies a duty enforceable by bodily invasion, not the existence of the benefit they receive). Payers derive high directionality, amplified by trapped or constrained exit: unvaccinated_objectors (constrained), military_service_members (trapped — total employment relationship), vaccine_injury_claimants (trapped — capped administrative recourse). No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the intended d values for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — epidemic disease control under free-riding — is live, so this is not a mandatrophy case: the arrangement's function has not atrophied and no piton reading is on offer from any seat. The classification discipline here runs in the opposite direction: from the public_health_primary seat the same arrangement computes as coordination with costs, and from this seat it computes as coercion with a rejected warrant. Authoring the victim and beneficiary structure explicitly, rather than averaging across readings, is what lets the engine register both computations as data about the kernel contest instead of collapsing them into one verdict. Mandatrophy resolved: false — the dispute is normative and live, not lifecycle decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the bodily_autonomy_primary reading of the public_health_mandate_authority kernel; which structural facts change if a sibling reading governs the same arrangement?',
    'Compare computed classifications across the sibling stories (public_health_primary, proportionality_reading) over the shared referent; the victim-set and epsilon deltas are the measured contest.',
    'Under public_health_primary the unvaccinated leave the victim set and the immunocompromised enter it as victims of non-protection; epsilon drops toward coordination-cost levels and the computed type moves toward rope or tangled_rope. Under proportionality_reading the victim set becomes severity-indexed and epsilon varies by mandate subtype.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Location of the kernel contest: categorical sovereignty vs commons-protection duty vs sliding-scale legitimacy.').

omega_variable(
    epsilon_reading_indexing,
    'Epsilon is authored here as reading-indexed over the fixed referent (the standing mandate arrangement, assessed by this reading''s own lights); would a different reading of the identical arrangement author a different epsilon, and is that divergence or inconsistency?',
    'Cross-reading comparison of the sibling stories'' authored epsilon over the same referent, holding the referent fixed per the kernel-reading epsilon rule.',
    'Divergence across readings over a shared referent is the framework''s measurement of value disagreement, not an epsilon-invariance violation; only intra-story epsilon instability would violate invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_indexing, conceptual, 'Reading-indexed epsilon over a fixed referent; sibling stories are the comparison set.').

omega_variable(
    categorical_premise_grounding,
    'Do the reading''s own holders ground the categorical premise deontologically (binding regardless of empirical facts) or empirically contingent (binding because intervention risks and alternatives are what they are)?',
    'Observe whether holders update the categorical claim in response to risk-profile and natural-immunity evidence; survey the reading''s own literature for Nuremberg/Belmont invocations versus risk-based objections.',
    'If empirically contingent, the foundational axiom routes toward engine-computed foreclosure under axiom_overriding drift; if deontological, it does not route regardless of drift, and the declared repudiation_pressure drift state stays structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_premise_grounding, empirical, 'Grounding type of the categorical axiom as actually held by the reading''s adherents.').

omega_variable(
    victim_set_scope_ambiguity,
    'Does the coercion-harm victim set extend uniformly across all mandate forms (school entry with exemptions, employment conditions, military compulsion, quarantine), or does the categorical reading''s own application vary by imposition severity?',
    'Parse the reading''s application across mandate subtypes; if severity-sensitive, the reading is internally graduated and the story decomposes per the epsilon-invariance principle into per-subtype constraints linked by network edges.',
    'If uniform, one constraint with one epsilon (this story as authored); if graduated, epsilon varies by subtype and separate stories with linked network edges are required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_scope_ambiguity, conceptual, 'Scope of the categorical prohibition across mandate subtypes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 20, 0.15).
narrative_ontology:measurement(publ_tr_t40, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 40, 0.2).
narrative_ontology:measurement(publ_tr_t60, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 60, 0.2).
narrative_ontology:measurement(publ_tr_t80, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 80, 0.25).
narrative_ontology:measurement(publ_tr_t100, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 100, 0.3).
narrative_ontology:measurement(publ_tr_t110, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 110, 0.35).
narrative_ontology:measurement(publ_tr_t120, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 120, 0.45).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(publ_be_t40, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(publ_be_t60, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(publ_be_t80, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(publ_be_t100, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(publ_be_t110, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 110, 0.6).
narrative_ontology:measurement(publ_be_t120, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 120, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(publ_su_t40, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(publ_su_t60, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(publ_su_t80, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(publ_su_t100, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 100, 0.35).
narrative_ontology:measurement(publ_su_t110, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 110, 0.55).
narrative_ontology:measurement(publ_su_t120, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 120, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'public health mandate authority' covers three structurally distinct constraints, one per reading of the kernel. This story (bodily_autonomy_primary) authors high epsilon over the standing arrangement; the public_health_primary sibling authors low epsilon over the identical referent (the unvaccinated leave its victim set, the immunocompromised enter it); the proportionality_reading authors epsilon as a function of imposition severity. Same referent, reading-indexed epsilon — the stories form one family via affects_constraints, and divergence in computed classification across the family is the corpus's measurement of the kernel contest, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
