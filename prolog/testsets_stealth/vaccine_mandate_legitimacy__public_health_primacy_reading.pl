% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy — Public Health Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   Compulsory immunization regimes condition employment, schooling, venue
 *   access, and movement on vaccination status, backed by termination,
 *   exclusion, fines, and narrowly administered exemptions. This file
 *   instantiates ONE reading of the vaccine_mandate_legitimacy kernel — the
 *   public_health_primacy reading, on which the state's duty to prevent
 *   collective harm grounds mandate authority and unvaccinated status counts
 *   as an externality others are entitled to be protected from. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest (the operating mandate regime) assessed by this
 *   reading's own lights; the sibling readings are separate constraint files,
 *   not hedges inside this one. KEY AGENTS (by structural relationship):
 *   public_health_bureaucracy: agenda-setter (institutional/arbitrage) —
 *   writes and enforces the requirements, accrues authority;
 *   mandate_refusers: primary payer (moderate/constrained) — bears
 *   termination, exclusion, fines; conscientious_objectors: payer
 *   (moderate/identity_locked) — refusal fused with religious identity;
 *   immunocompromised_patients: beneficiary (powerless/trapped) — dependent
 *   on community coverage; elderly_high_risk_adults: beneficiary
 *   (organized/constrained); vaccine_manufacturers: beneficiary
 *   (powerful/arbitrage) — guaranteed indemnified demand; healthcare_workers:
 *   payer with incidental beneficiary position (organized/constrained);
 *   employers_subject_to_mandates: payer and delegated administrator
 *   (organized/mobile); vaccinated_general_public: beneficiary
 *   (moderate/mobile) — the consenting majority; constitutional_courts:
 *   observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.49).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy — Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '2adbf515-dbff-4923-b06e-0017fb4d549c').
narrative_ontology:cs_kernel_codification('2adbf515-dbff-4923-b06e-0017fb4d549c', fixed_text).
narrative_ontology:cs_authority_grounding('2adbf515-dbff-4923-b06e-0017fb4d549c', lineage).
narrative_ontology:cs_interpretation_layer_present('2adbf515-dbff-4923-b06e-0017fb4d549c').
narrative_ontology:cs_reading_relation('2adbf515-dbff-4923-b06e-0017fb4d549c', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2adbf515-dbff-4923-b06e-0017fb4d549c', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('2adbf515-dbff-4923-b06e-0017fb4d549c', foundational, state_duty_prevent_collective_harm).
narrative_ontology:cs_axiom_status(state_duty_prevent_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('2adbf515-dbff-4923-b06e-0017fb4d549c', state_duty_prevent_collective_harm, deontological).
narrative_ontology:cs_axiom('2adbf515-dbff-4923-b06e-0017fb4d549c', foundational, unvaccinated_status_is_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_externality, holdable).
narrative_ontology:cs_axiom_grounding('2adbf515-dbff-4923-b06e-0017fb4d549c', unvaccinated_status_is_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('2adbf515-dbff-4923-b06e-0017fb4d549c', police_power_collective_defense_framework).
narrative_ontology:cs_drift_state('2adbf515-dbff-4923-b06e-0017fb4d549c', post_emergency_endemic_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2adbf515-dbff-4923-b06e-0017fb4d549c', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, elderly_high_risk_adults).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, conscientious_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_workers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_general_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_subject_to_mandates).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, collective_harm_prevention_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, externality_internalization_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, police_power_public_health_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers compulsory immunization requirements: sets which vaccines, which populations, which penalty schedules, and which exemption categories count. Each emergency deployment expands its staff, data systems, and enforcement reach, and the expanded apparatus remains on the books after the acute phase passes. Its officials testify that the requirements track outbreak science; its budget lines and rulemaking dockets grow with each deployment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Decline recommended immunizations for personal, philosophical, or medical-skeptic reasons. Where requirements bind, they face termination from employment, exclusion from schools and venues, fines, or loss of benefits. Narrow exemption categories leave few lawful paths; relocating to a permissive jurisdiction means leaving work, family, and community behind. Most bear the costs quietly; a visible minority organizes protests and litigation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, mandate_refusers, payer,
    moderate, biographical, constrained, national).

% Refuse on religious or conscience grounds they regard as non-negotiable commitments. Seeking an exemption requires submitting their inner life to administrative review; denial forces a choice between the commitment and their livelihood. Their refusal is fused with religious identity and congregational membership, so complying would cost them standing in the community that holds the commitment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, conscientious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Cannot mount protective responses to vaccination themselves, or are medically advised against certain products. Their safety depends on the people around them being covered. They have no exit from this dependence: changing jurisdictions does not change their physiology. They advocate for the strictest possible requirements and the narrowest exemptions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, global).

% Face the highest severe-outcome risk from the diseases in question and gain the largest absolute protection from high community coverage. Well organized through advocacy associations, they lobby for retention and tightening of requirements. They also bear the costs when requirements misfire, such as poorly sequenced booster campaigns or supply diverted from their cohort.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, elderly_high_risk_adults, beneficiary,
    organized, biographical, constrained, national).

% Sell the products the requirements compel. Government purchase guarantees and indemnification arrangements shift demand risk and liability away from them, and purchase contracts are negotiated with the same agencies that set the requirements. They fund a substantial share of the safety surveillance the agencies cite in defense of the program.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Work where transmission risk and patient vulnerability are both highest, so requirements bind them first and most strictly. A minority refused and left the profession or was terminated, and the resulting staffing shortages are absorbed by colleagues who complied. Those who remain in compliance work in a measurably lower-exposure environment than before the requirements.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_workers, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_workers, beneficiary).

% Must verify status, run exemption processes, and separate non-compliant staff under government rules they did not write. Compliance costs land on their HR and legal functions; non-compliance exposes them to fines and liability. Large employers initially lobbied for uniform federal rules to preempt a patchwork of state rules; as labor markets tightened, some began lobbying for repeal.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_subject_to_mandates, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_subject_to_mandates, agenda_setter).

% Took the vaccines and carry the records and passes the system runs on. They receive the protection and the restored access to venues, workplaces, and travel. Their consent is polled continuously and sustains the requirements politically; their attention moves on once acute risk fades, which is precisely when enforcement maintenance becomes least examined.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_general_public, beneficiary,
    moderate, biographical, mobile, national).

% Adjudicate whether the requirements fit the police-power tradition: rational-basis review under the century-old precedent line, heightened review for religious-exemption denials, and separation-of-powers challenges to agency rulemaking. Their docket volume tracks the intensity of enforcement, and their opinions redraw the boundary within which the administering agencies operate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in communicable-disease control: an individual's vaccination decision ignores the transmission risk their uncovered status imposes on others, so voluntary uptake undersupplies the herd protection that high-risk members cannot procure for themselves. Compulsory coverage internalizes that externality and reaches thresholds during surges that incentives alone did not.
% TRANSFER_FUNCTION: Moves employment access, educational access, and freedom of movement from refusers into a collectively managed protection pool; moves administrative authority, staffing, and budget to public health agencies; moves guaranteed, indemnified revenue to manufacturers.
% ABSENT_VOICES: Refusers appear in the process mainly as objects of administration rather than co-authors of it: exemption hearings adjudicate their claims but do not seat them in rulemaking. Quarantine-skeptical constitutionalists and disability-rights advocates who object to categorical exclusion have thin formal representation in emergency health orders; their voice arrives mostly after the fact, through litigation and post-hoc legislative revision.
% DISAPPEARANCE_RATIONALE: If the requirements and their enforcement vanished overnight, coverage would sag below thresholds in identifiable pockets, employers and schools would improvise private admission rules to fill the vacuum, insurers and event operators would reprice uncovered risk, and the agencies would shed an enforcement apparatus built over the emergency period. The surrounding arrangements are load-bearing enough that removal forces reorganization rather than return to a prior equilibrium.
% FOUNDING_PROBLEM: Recurrent epidemic catastrophe in the era before threshold coverage: communities absorbed periodic waves of smallpox and comparable diseases because voluntary uptake never reached the level at which transmission chains break. The modern mandate authority was founded on the state's police power to prevent that collective harm, crystallized in the early twentieth-century precedent line.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: historical mortality records attest the pre-coverage epidemic toll; the judicial precedent line rests on documented outbreak harm rather than agency assertion; and independent epidemiological literature attests the existence of coverage thresholds. The administering agencies also attest the problem is live, but the corroborating sources above stand outside the beneficiary set. Whether the founding problem remains live in the current endemic phase is disputed between those sources and the arrangement's critics.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.49: by this reading's own lights, most of the burden on refusers is the justified price of externality internalization rather than rent, so epsilon sits well below what a bodily-autonomy reading would author over the identical arrangement; the residual extraction is the part even this reading must count — enforcement authority persisting past epidemiological necessity, guaranteed-margin procurement, and compliance maintenance continuing after acute risk faded. Suppression is a raw structural property, unscaled: 0.72 reflects termination, exclusion, fines, and narrow exemption channels as the arrangement's actual holding mechanism. Theater_ratio 0.48 is the interval-end value of a rising series — the functional core (vaccination reduces severe disease) is real, but a growing share of activity maintains compliance machinery after the threat that justified it receded. Accessibility_collapse 0.55: alternatives (voluntarism with incentives, test-or-work regimes, targeted high-risk-setting mandates, natural-immunity recognition) stayed live and were actually adopted in some jurisdictions, so alternatives collapsed only partially. Resistance 0.70: mass protest, litigation waves, and legislative reversals met the requirements throughout. The claimed type, tangled_rope, is stated from structure — a genuine coordination function AND named payers bearing costs through the same enforced mechanism — independently of the metric values; the engine computes per-seat types from the structural data. All three tracked metric series run on one shared time grid (t=0,5,10,15,20,25,30) so no row substitutes an end-state scalar into an earlier period.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the bureaucracy's position the arrangement is a coordination instrument it built, justified by a duty it holds, with exit available through redeployment to the next health domain; from the refuser seats the same structure operates as concentrated, personally ruinous pressure with narrowing lawful exits. The two beneficiary clusters diverge too: immunocompromised patients experience the requirements as lifeline, while manufacturers experience them as demand guarantee. Healthcare workers and employers occupy genuinely dual positions — bound and burdened, yet also protected and delegating administration — which is why both carry secondary roles. The engine computes these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directional values: the bureaucracy (also agenda-setter), the medically vulnerable, the organized elderly cohort, and manufacturers all sit toward the subsidized end, with the trapped exit of immunocompromised patients pinning them nearest full beneficiary. Victim declarations drive high directional values: refusers and conscientious objectors bear the transfer through the same mechanism that coordinates everyone else, and their constrained or identity-locked exits push them toward the full-target end. Healthcare workers and employers derive intermediate positions from their dual roles. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure coordination (the rope the endorsing reading naturally reaches for) would erase the named payers whose costs are the mechanism's operating input; reading it as pure extraction (the critics' framing) would erase the genuine collective-action function that survives even hostile audit. Tangled rope holds both. On the genealogy interview: founding_problem_status is contested and disappearance_verdict is world_rearranges, so the mismatch consumer finds no dead-mandate zombie flag today — the pathogen still circulates and the coverage thresholds still bind. But the theater_ratio trajectory (0.14 to 0.48) is the leading indicator: if the founding problem is later adjudged dead while the world still rearranges around the apparatus, the capture/zombie flag fires on exactly this story's temporal record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_unresolved,
    'This story instantiates only the public_health_primacy reading of the vaccine_mandate_legitimacy kernel; the bodily_autonomy and risk_stratification readings instantiate different constraints over the same statutes, with different victim sets and different epsilon values. Which reading governs a given jurisdiction''s arrangement is itself politically unsettled.',
    'Generate the sibling reading stories and compare computed classifications; jurisdiction-level legal analysis maps which reading each legal order actually operationalizes in its exemption and enforcement design.',
    'Under the bodily_autonomy reading every coercee enters the victim set and epsilon approaches its maximum; under risk_stratification only below-threshold coercion counts as unjust, shrinking the victim set and lowering epsilon for targeted regimes. The same statute classifies differently across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_unresolved, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    externality_transmission_waning,
    'Does unvaccinated status still constitute a material externality under endemic-phase conditions, given that vaccine-induced protection against transmission wanes faster than protection against severe outcomes?',
    'Household-transmission and variant-period studies quantifying the infection-transmission reduction attributable to current vaccination status, as distinct from severe-outcome reduction.',
    'If transmission reduction is marginal, the externality premise that converts refusal into a regulable harm weakens, and this reading''s own lights assign higher epsilon to the same enforcement burden — pushing the arrangement toward pure extraction even from the endorsing seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_transmission_waning, empirical, 'Empirical foundation of the externality premise under endemic conditions.').

omega_variable(
    authority_necessity_residual,
    'How much of the public health bureaucracy''s accrued enforcement authority is necessary for live harm prevention, and how much persists as self-maintaining apparatus after the necessity passed?',
    'Sunset audits comparing enforcement footprint, staffing, and spending against measured outbreak-response activity across the post-acute period, jurisdiction by jurisdiction.',
    'A large unnecessary residual raises the extraction component attributable to the agenda-setter seat and pushes that seat''s computed classification toward pure extraction; a small residual supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_necessity_residual, empirical, 'Necessity versus persistence of the accrued enforcement apparatus.').

omega_variable(
    exemption_denial_sincerity,
    'Do high religious-exemption denial rates reflect genuine scarcity of sincere claims, or administrative filtering designed to suppress exemption uptake?',
    'Compare approval rates, review procedure design, and appellate reversal rates across jurisdictions with substantively identical standards but procedurally different exemption administrations.',
    'If filtering dominates, the measured suppression is more structural than the written standards suggest and the refuser seats'' effective suppression exceeds the authored scalar; if sincerity scarcity dominates, the suppression reflects the inherent difficulty of adjudicating the category.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_denial_sincerity, conceptual, 'Whether exemption administration suppresses claims or merely adjudicates them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(vacc_tr_t5, observed).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(vacc_tr_t10, observed).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(vacc_tr_t15, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(vacc_tr_t25, observed).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(vacc_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(vacc_be_t5, observed).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(vacc_be_t10, observed).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(vacc_be_t15, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(vacc_be_t25, observed).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(vacc_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(vacc_su_t5, observed).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(vacc_su_t10, observed).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(vacc_su_t15, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(vacc_su_t25, observed).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(vacc_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, digital_health_pass_infrastructure).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate legitimacy' decomposes into three kernel readings, each an epsilon-invariant constraint. This file is the public_health_primacy reading (victims: refusers and conscientious objectors; epsilon authored by the endorsing reading's lights over the standing arrangement). The bodily_autonomy_primacy reading makes every coercee a victim and authors maximal epsilon; the risk_stratification_reading restricts the victim set to those coerced below the actuarial threshold. The upstream reading (this one) supplies the doctrinal foundation the stratified reading qualifies, and its overreach episodes degrade the legitimacy conditions under which targeted mandates operate — hence the influences edge. All three files link one another via network.affects_constraints in addition to the typed edges in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
