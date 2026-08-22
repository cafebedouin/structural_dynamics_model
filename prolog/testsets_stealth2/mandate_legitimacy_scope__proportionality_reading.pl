% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Conditions on Vaccination Mandate Legitimacy (severity / safety-efficacy / less-restrictive-alternatives)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   State vaccination-mandate regimes - school-entry requirements, employment
 *   rules, outbreak-response compulsion - operate inside a proportionality
 *   framework: a mandate is legitimate only when disease severity, vaccine
 *   safety and efficacy, and the unavailability of less restrictive
 *   alternatives jointly support it. This story instantiates the
 *   proportionality reading of the mandate_legitimacy_scope kernel (committer
 *   structure is recorded in commentary.kernel_context and the omega
 *   variables, per the reading rules); its referent is the standing
 *   arrangement - mandate regimes governed by proportionality review -
 *   assessed by this reading's own lights, which endorse the framework as the
 *   correct settlement of the police-power-versus-bodily-integrity conflict.
 *   The framework's operation nonetheless imposes real, rising costs:
 *   justification and litigation burdens on health authorities in every case,
 *   compelled intervention on mandate subjects where the prongs are
 *   satisfied, residual outbreak risk on the medically vulnerable and on
 *   infants wherever a prong fails or litigation delays enforcement, and fee
 *   flows to the litigation bar from both sides. claimed_type is authored as
 *   tangled_rope from the structure alone - a genuine evidentiary
 *   coordination function fused with asymmetric, conditionally distributed
 *   costs, held up by active judicial enforcement - while the metrics are
 *   authored independently as descriptive facts; the engine computes per-seat
 *   classifications from the structural data, and any divergence between the
 *   claim and the computed types is the measurement the corpus exists to
 *   take.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda-setting seat with payer overlay (institutional/constrained) - sets and defends mandates, bears the three-prong justification burden, collects legitimacy where the prongs are satisfied
 *   - judiciary: co-agenda-setter (institutional/analytical) - administers the framework and sets the rigor of prong application
 *   - mandate_subjects: conditional target (moderate/constrained) - bears compelled vaccination where the prongs hold, holds the procedural shield where they fail
 *   - immunocompromised_and_medically_vulnerable: conditional target (powerless/trapped) - bears residual outbreak risk wherever a prong fails or litigation delays enforcement; cannot vaccinate or exit exposure
 *   - infants_too_young_for_vaccination: voiceless risk-bearer (powerless/trapped) - bears every prong failure's consequences with no seat in the proceedings
 *   - conscientious_objectors: protected seat (moderate/identity_locked) - the framework's alternatives prong and exemption routes are built around their refusal
 *   - vaccine_manufacturers: incidental collector (institutional/arbitrage) - validated mandates guarantee demand; their safety and efficacy data are load-bearing in the second prong
 *   - epidemiological_evidence_community: empowered seat (organized/mobile) - the framework makes expert severity and safety evidence load-bearing
 *   - mandate_litigation_bar: fee-collecting seat (organized/mobile) - every prong dispute is billable from both sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.52).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.36).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Conditions on Vaccination Mandate Legitimacy (severity / safety-efficacy / less-restrictive-alternatives)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '0d120e95-d327-4136-9031-74892f7ab0b1').
narrative_ontology:cs_kernel_codification('0d120e95-d327-4136-9031-74892f7ab0b1', formalized).
narrative_ontology:cs_authority_grounding('0d120e95-d327-4136-9031-74892f7ab0b1', lineage).
narrative_ontology:cs_interpretation_layer_present('0d120e95-d327-4136-9031-74892f7ab0b1').
narrative_ontology:cs_reading_relation('0d120e95-d327-4136-9031-74892f7ab0b1', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('0d120e95-d327-4136-9031-74892f7ab0b1', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('0d120e95-d327-4136-9031-74892f7ab0b1', foundational, mandate_legitimacy_is_parameter_conditional).
narrative_ontology:cs_axiom_status(mandate_legitimacy_is_parameter_conditional, holdable).
narrative_ontology:cs_axiom_grounding('0d120e95-d327-4136-9031-74892f7ab0b1', mandate_legitimacy_is_parameter_conditional, empirically_contingent).
narrative_ontology:cs_axiom('0d120e95-d327-4136-9031-74892f7ab0b1', foundational, least_restrictive_alternative_requirement).
narrative_ontology:cs_axiom_status(least_restrictive_alternative_requirement, holdable).
narrative_ontology:cs_axiom_grounding('0d120e95-d327-4136-9031-74892f7ab0b1', least_restrictive_alternative_requirement, instrumental).
narrative_ontology:cs_reference_frame('0d120e95-d327-4136-9031-74892f7ab0b1', evidence_calibrated_mandate_authority).
narrative_ontology:cs_drift_state('0d120e95-d327-4136-9031-74892f7ab0b1', post_pandemic_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d120e95-d327-4136-9031-74892f7ab0b1', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, conscientious_objectors).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, epidemiological_evidence_community).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, mandate_litigation_bar).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, mandate_subjects).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, immunocompromised_and_medically_vulnerable).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, infants_too_young_for_vaccination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, mandate_subjects).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_and_medically_vulnerable).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, evidence_based_public_health_law).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run immunization programs and issue school-entry, employment, and outbreak-response vaccination requirements. Under the framework they operate in, each requirement must be supported by severity, safety-efficacy, and less-restrictive-alternatives evidence, and defended in court when challenged. Where the evidence supports a requirement they collect judicial validation and public legitimacy; where it does not they must fall back on education and voluntary campaigns. They cannot exit the legal terrain every mandate stands on, but they can shape it through the record they build.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, payer).

% Hear challenges to vaccination requirements and decide whether the record satisfies the three prongs. They set how rigorously the prongs are applied, from deep evidentiary review to deferential acceptance of agency assertions, and their doctrine determines which requirements stand. No mandate survives without passing their review, and no challenge to one gets a forum they do not control.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Parents of schoolchildren, employees, and students who face vaccination requirements. Where the prongs are satisfied they bear the intervention itself, vaccination on pain of exclusion from school or work; where the prongs fail they keep a procedural shield that blocks the requirement. Their exits are partial: private schooling, homeschooling, remote work, or medical and religious exemption routes where offered, each with real costs. They breathe the same air as everyone else, so requirements that stand also protect them.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_subjects, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, mandate_subjects, beneficiary).

% People who cannot be vaccinated or respond poorly to vaccines and who depend on those around them being vaccinated. Where the prongs are satisfied they receive protection they cannot generate for themselves; where a prong fails, a contested safety finding or a judge accepting education campaigns as sufficient, they absorb the residual outbreak risk. The risk arrives through ordinary community contact and cannot be exited. Advocacy organizations speak for them; they rarely hold party status in the cases that decide their exposure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_and_medically_vulnerable, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, immunocompromised_and_medically_vulnerable, beneficiary).

% Babies too young for the vaccination schedule, protected only by the vaccination of everyone around them. Every requirement blocked or delayed on a prong failure raises their exposure during the window before they can be vaccinated. They appear in the proceedings only through the arguments of others; they cannot object, testify, or litigate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, infants_too_young_for_vaccination, excluded,
    powerless, immediate, trapped, national).

% People who decline vaccination on religious or philosophical grounds and litigate or organize against requirements. The framework's alternatives prong and exemption routes are built around their refusal; where they prevail they keep their unvaccinated status, where they fail they face the same exclusion choice as other mandate subjects. Compliance would dissolve the commitment their position rests on, so they litigate rather than vaccinate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, conscientious_objectors, beneficiary,
    moderate, biographical, identity_locked, national).

% Produce the vaccines whose safety and efficacy data form the second prong. Every validated requirement guarantees demand for their products, and adverse-event findings or efficacy shortfalls can unmake a requirement's legal foundation. They operate across jurisdictions and can shift portfolio and market focus if any single jurisdiction's rules turn hostile.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Academic and agency scientists who produce the severity burden, vaccine effectiveness, and adverse-event data the prongs consume. The framework makes their evidence load-bearing in court, which channels funding, citation, and standing to them. They can shift to other research questions if the demand for their work changes.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, epidemiological_evidence_community, beneficiary,
    organized, biographical, mobile, global).

% Lawyers who represent health authorities defending requirements and objectors challenging them. Every prong dispute is billable work on both sides, and the framework's evidentiary demands generate expert-witness and record-building engagements. They can move to other dockets if mandate litigation dries up.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_litigation_bar, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurrent collective problem of calibrating state coercive medical power to epidemiological facts: it gives legislatures, agencies, and courts a shared three-prong procedure (disease severity; vaccine safety and efficacy; availability of less restrictive alternatives) that authorizes intervention where the parameters justify it and blocks it where they do not, replacing case-by-case political warfare over each mandate with a common evidentiary framework.
% TRANSFER_FUNCTION: Moves justification burdens and decision costs onto health authorities, who must assemble severity, safety, and alternatives evidence for every mandate and defend it in court; moves residual infection risk onto medically vulnerable people and infants wherever a prong fails or litigation delays enforcement; moves compelled intervention onto mandate subjects wherever all three prongs are satisfied; and distributes litigation fees to the bar from both sides of every dispute.
% ABSENT_VOICES: Infants too young to be vaccinated and the medically vulnerable bear the downside of every prong failure but appear in the proceedings only through proxy advocates; communities in active outbreaks bear the delay costs of proportionality litigation without party status; and disability-rights critics of the less-restrictive-alternatives prong, who read remote schooling and workplace workarounds as segregation, have no seat in the doctrinal conversation that deploys it.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, every mandate's validity would be re-decided under whichever categorical reading courts adopted next: under a public-health-primacy approach, flu-scale and convenience mandates become enforceable and vulnerable populations gain protection at mandate subjects' expense; under a categorical bodily-integrity approach, measles-scale mandates fall and outbreak response loses its central tool. School-entry law, employment rules, and outbreak authority would all be re-litigated from zero; the arrangement's dependents are everyone the mandates touch.
% FOUNDING_PROBLEM: The smallpox-era collision that the Jacobson settlement addressed: state police power to compel vaccination against epidemic disease versus the individual's bodily integrity, resolved by holding compulsion within state power while requiring its exercise to be proportionate to the demonstrated threat. This reading is the disciplined, evidence-gated form of that settlement.
% FOUNDING_PROBLEM_CORROBORATION: Both rival readings corroborate that the founding problem is live; they dispute the answer, not the question: primacy advocates press for broader compulsion in outbreaks while autonomy advocates litigate every mandate. A century of mandate case law, the post-pandemic wave of mandate litigation, the bioethics literature on least-restrictive-means, and legislative hearings on mandate bills all attest the severity-versus-liberty calibration problem from outside any single beneficiary seat.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end) because the framework's costs are real but case-conditional: justification burdens and litigation fall on authorities in every case, compulsion falls on subjects only where the prongs hold, residual risk falls on the vulnerable only where they fail - and the reading itself endorses much of this as the price of calibrated protection. Suppression (0.36) is the framework's binding force on official action plus its intensifying enforcement machinery (injunction practice, strict-scrutiny statutes), not coercion of subjects: it forecloses no rival reading and blocks no subject exit, since exemptions, private schooling, and remote work persist. Theater (0.35) splits between genuine prong analysis in paradigm cases (measles outbreaks force real severity findings) and deferential rubber-stamping elsewhere, rising with politicization. Accessibility collapse is low (0.30) because the categorical rival readings remain fully available as frameworks - nothing about this doctrine forecloses them as positions. Resistance (0.60) comes from both flanks: primacy advocates read the prongs as handcuffs, autonomy advocates as capitulation, and litigants push each prong in both directions. The three measurement series share one grid (t=0,4,8,12,16,20,24). The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change - the framework's active force against official action intensified across the post-pandemic litigation wave - while extractiveness and theater track cost accumulation and deference drift. gain_flow is authored 'diffuse' after checking every seat: authorities collect legitimacy only where the prongs hold, objectors collect shields only where they fail, manufacturers collect demand only from validated mandates, and the bar collects fees from both sides - no seat captures the framework's proceeds as a whole; the largest procedural spend dissipates into the litigation sector, which captures fees rather than governing surplus. fixing_cost is omitted: the benefit of removal is indeterminate (removal toward which sibling reading?), so a cost class relative to benefit is not established.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is the point: from the health-authority seat the framework is a legitimacy engine it feeds evidence into and occasionally loses in; from the mandate-subject seat it is a shield or a compulsion depending on the pathogen in front of the court; from the immunocompromised seat it is a delay machine standing between them and community protection; from the objector seat it is the framework built around their refusal; from the manufacturer seat a demand guarantee; from the bar seat a revenue stream. Same doctrine, different experiences - the engine computes this divergence from role, power, and exit data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: manufacturers (arbitrage exit) sit nearest the beneficiary end; the evidence community and the bar (both mobile) close behind; conscientious objectors are beneficiaries but identity-locked, which tempers rather than inverts their position; authorities are beneficiaries with a payer overlay (secondary_role payer) - they collect validation but pay justification costs in every case, so their d sits above the pure-beneficiary end. Victims derive high directionality: the immunocompromised (trapped, unable to vaccinate or exit exposure) sit near full target; infants (trapped, powerless, voiceless) beside them; mandate subjects (constrained, secondary_role beneficiary) sit high but tempered by the shield the framework hands them in failed-prong cases. The signature structural fact is conditionality: each seat's position flips with disease parameters, which is why epsilon is authored for the framework itself rather than for any single mandate. No directionality overrides are used - the beneficiary/victim declarations plus exit options capture the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - calibrating police power to demonstrated epidemic threat without collapsing bodily integrity - is live: every new pathogen reopens the calibration, and the R5 mismatch consumer finds status=live against verdict=world_rearranges, so no zombie flag fires. The tangled_rope claim guards against both mislabels: reading the framework as pure coordination would erase the concentrated, voiceless residual risk the alternatives prong dumps on infants and the immunocompromised; reading it as pure extraction would erase its genuine authorizing function - the same structure that blocks flu-scale mandates upholds measles-scale ones, and outbreak response depends on that. The drift watch runs piton-ward: if theater keeps climbing and prong application collapses into deference, the framework persists as theatrical proportionality maintained by inertia and professional habit - the theater_ratio series is the early-warning track, and the rigor_versus_deference omega is its resolution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the proportionality_reading of the mandate_legitimacy_scope kernel. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Compare the three sibling stories'' victim sets and epsilon values directly: the disagreement is located in whether mandate legitimacy is conditional (this reading), categorically available (public_health_primary), or categorically unavailable (bodily_autonomy_primary) - in the conditionality structure, not in the value of vaccination or the reality of disease.',
    'If public_health_primary governed, the victim set expands to all mandate subjects for every pathogen and epsilon for mandate regimes rises. If bodily_autonomy_primary governed, the victim set becomes every person subjected to non-consensual intervention regardless of parameters and measles-scale mandates become illegitimate. This reading''s parameter-conditional victim set is the middle structure the other two bracket.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of the mandate_legitimacy_scope kernel; sibling readings would move the victim set and epsilon to categorical extremes.').

omega_variable(
    prong_threshold_underdetermination,
    'Where do the thresholds sit - how severe must disease be, how safe and efficacious the vaccine, how unavailable the alternatives - and who sets them?',
    'Doctrinal development plus empirical epidemiology: courts or legislatures fixing numeric or settled case-by-case thresholds would resolve it; until then each application reopens the question.',
    'Threshold placement moves people between the victim sets: mandate subjects bear compulsion above the threshold, vulnerable populations bear residual risk below it - the same framework yields opposite classifications one threshold-width apart.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prong_threshold_underdetermination, conceptual, 'The prongs are empirically measurable but their decision thresholds are a normative choice the framework itself does not fix.').

omega_variable(
    epsilon_pathogen_variance,
    'Does the single authored epsilon adequately represent a framework whose operation differs sharply by pathogen - measles-scale mandates validate while flu-scale ones fail?',
    'Decomposition test: if drift analysis shows divergent trajectories for high-severity and low-severity applications, split into per-pathogen constraint stories (measles mandate regime, influenza mandate regime) linked by network edges; the framework-level story retains the test''s own operational epsilon.',
    'Per-pathogen stories would compute low extraction from subjects in validated cases and blocked-overreach profiles in failed cases; the framework-level epsilon of 0.52 averages across both and would be replaced by pathogen-specific values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_pathogen_variance, empirical, 'Whether to decompose the framework story per pathogen or retain one epsilon for the test itself.').

omega_variable(
    rigor_versus_deference,
    'Do courts actually apply the three prongs with the rigor the framework requires, or does deference to agency assertions reduce prong analysis to post-hoc rationalization?',
    'Systematic coding of mandate case outcomes against the depth of prong analysis: opinion text, record citations, and whether expert testimony is weighed or merely accepted.',
    'High deference raises theater_ratio and effective extraction - the framework becomes cover for outcomes reached on other grounds - and pushes the structure toward theatrical maintenance; rigorous application keeps the coordination function real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigor_versus_deference, empirical, 'Whether prong application is real analysis or rubber-stamping; drives the theater_ratio interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mls_proportionality_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mls_proportionality_tr_t4, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(mls_proportionality_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(mls_proportionality_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(mls_proportionality_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(mls_proportionality_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(mls_proportionality_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(mls_proportionality_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mls_proportionality_be_t4, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(mls_proportionality_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(mls_proportionality_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(mls_proportionality_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(mls_proportionality_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(mls_proportionality_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mls_proportionality_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(mls_proportionality_su_t4, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(mls_proportionality_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(mls_proportionality_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.28).
narrative_ontology:measurement(mls_proportionality_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(mls_proportionality_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(mls_proportionality_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% The colloquial question 'are vaccine mandates legitimate?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the mandate_legitimacy_scope kernel: this proportionality reading (legitimacy conditional on disease parameters, moderate epsilon, conditional victim set), public_health_primary (legitimacy whenever protection requires it, victim set = all mandate subjects for all pathogens), and bodily_autonomy_primary (legitimacy never, victim set = everyone subjected to non-consensual intervention). Each sibling is a separate story with its own epsilon, beneficiaries, and victims; they are linked here because the upstream empirical question (how severe, how safe, how available alternatives) feeds all three, and each reading's operation changes the litigation environment the others operate in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
