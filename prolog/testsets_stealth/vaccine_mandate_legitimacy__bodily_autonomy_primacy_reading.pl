% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Absolute Medical Self-Sovereignty Settlement (Bodily Autonomy Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   The standing arrangement this story is about is the medical-consent
 *   settlement: the doctrinal architecture — informed consent, constitutional
 *   bodily-integrity protections, the Nuremberg-lineage research codes — that
 *   insulates every person's medical decisions from state compulsion, with
 *   mandate authority confined to narrow, contested conditionality. The
 *   kernel contest asks whether epidemic-scale collective harm overrides that
 *   insulation. This file instantiates the bodily_autonomy_primacy_reading:
 *   the insulation holds categorically, regardless of outcome. CONSTRAINT
 *   FAMILY DECOMPOSITION: the colloquial label 'vaccine mandate legitimacy'
 *   covers three structurally distinct claims with different epsilon values
 *   and different victim sets, written as separate stories and linked via
 *   network.affects_constraints. The public_health_primacy_reading treats
 *   mandate authority as the justified baseline and places the unvaccinated
 *   in its target set; the risk_stratification_reading conditions legitimacy
 *   on actuarial thresholds. THIS reading inverts the structure: its victim
 *   set is the immunocompromised and medically vulnerable, who bear the
 *   exposure risk that a coercion-capable regime would suppress, and its
 *   beneficiary set centers uncoerced individuals and the liberty movements
 *   organized around them. The epsilon here is authored for the consent
 *   settlement as this reading assesses it — including the risk burden it
 *   knowingly places on the vulnerable ('regardless of outcome') — not for
 *   the mandate regime, which is the siblings' referent. CLAIM/METRIC
 *   INDEPENDENCE: the claimed type is mountain because the reading's form is
 *   genuinely categorical, outcome-independent, and natural-law-shaped; the
 *   metrics are authored from the arrangement's actual operation, which is
 *   contested, partly extractive, and enforcement-dependent. Any divergence
 *   between claim and computed type is the datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - individual_medical_decisionmakers: Primary beneficiary (moderate/mobile) — holds an unconditional veto over their own medical treatment
 *   - liberty_advocacy_movements: Organized beneficiary (organized/identity_locked) — converts the settlement's persistence into caseload, membership, funding, and standing
 *   - immunocompromised_patients: Primary target (powerless/trapped) — absorbs exposure risk they cannot offset by any personal choice
 *   - chronic_condition_elderly: Secondary target (powerless/trapped) — bears consequence-amplified exposure set by others' choices
 *   - frontline_healthcare_workers: Dual-positioned bearer (organized/constrained) — staffs surge exposure while holding the same personal veto they defend
 *   - public_health_authorities: Excluded institutional actor (institutional/constrained) — strongest instruments removed from its toolkit
 *   - constitutional_courts: Agenda-setting administrator (institutional/constrained) — polices the compulsion line, collects nothing
 *   - bioethics_community: Analytical observer (analytical/analytical) — sees the full structure, bears none of the risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.62).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.58).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Absolute Medical Self-Sovereignty Settlement (Bodily Autonomy Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '1e6a39f5-bb0a-4be9-acde-3dd326618f2d').
narrative_ontology:cs_kernel_codification('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', formalized).
narrative_ontology:cs_authority_grounding('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', lineage).
narrative_ontology:cs_interpretation_layer_present('1e6a39f5-bb0a-4be9-acde-3dd326618f2d').
narrative_ontology:cs_reading_relation('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', secondary, consent_requirement_invariant_to_consequence_severity).
narrative_ontology:cs_axiom_status(consent_requirement_invariant_to_consequence_severity, holdable).
narrative_ontology:cs_axiom_grounding('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', consent_requirement_invariant_to_consequence_severity, deontological).
narrative_ontology:cs_reference_frame('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', absolute_bodily_sovereignty_baseline).
narrative_ontology:cs_drift_state('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', post_covid_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e6a39f5-bb0a-4be9-acde-3dd326618f2d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individual_medical_decisionmakers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, chronic_condition_elderly).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, frontline_healthcare_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, frontline_healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every competent adult holds an unconditional veto over what is done to their own body medically. No employer, school, or agency can compel submission to an injection or procedure; the protection requires nothing from them in return and travels with them within the jurisdiction. During epidemics they may decline and continue ordinary life, and the decision is theirs alone whatever the consequences for others.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individual_medical_decisionmakers, beneficiary,
    moderate, biographical, mobile, national).

% Networks of organizations, litigators, and activists whose stated purpose is defending medical decision rights. The settlement's persistence supplies their caseload, membership rolls, fundraising appeals, and public standing; each mandate controversy recruits and finances them anew. Their organizational identity is fused with the principle itself — abandoning it would dissolve the movement, so exit from the position is not a live option for the organizations or their core members.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, identity_locked, national).

% Transplant recipients, chemotherapy patients, and others whose conditions blunt vaccine protection. They depend on the surrounding population's immunity, which they cannot procure by any choice of their own. When coverage is voluntary and uneven, they absorb exposure through work, transit, and care settings they cannot avoid; shielding at home is their main lever and it costs income, schooling, and access to treatment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).

% Older adults and people with diabetes, lung disease, and cardiac disease. Vaccination helps them, but their risk scales with community transmission volume, which under a strictly voluntary regime is set by everyone else's choices. They ride the same exposure curve as the general population while carrying far higher consequence per exposure; isolation is their only personal control and it trades health protection for everything else they need.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, chronic_condition_elderly, payer,
    powerless, biographical, trapped, national).

% Nurses, physicians, and aides who staff the wards when transmission runs high, absorbing occupational exposure as a condition of employment. The same settlement forbids anyone compelling their own medical decisions — a protection they invoke for themselves and their families. Leaving the profession forfeits training investment, licensure, and professional identity, so their exit is costly and rarely taken.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, frontline_healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, frontline_healthcare_workers, beneficiary).

% Agencies charged with controlling epidemics. The settlement removes their strongest instruments: they may inform, offer, persuade, and track, but not compel. During crises they petition legislatures and courts for authority and are repeatedly narrowed or reversed; their professional assessments of preventable mortality carry no operational weight inside the settlement's administration.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, excluded,
    institutional, generational, constrained, national).

% Judges who articulate and police the line between permissible encouragement and forbidden compulsion. They absorb the docket every time an epidemic tempts legislatures and agencies toward mandates; their precedents define what the settlement permits, and emergency-era rulings are revisited and partly reversed afterward. They collect no revenue from the arrangement and cannot decline the disputes that reach them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Scholars spanning the autonomy and utilitarian wings of the field. They document the settlement's historical origins, stress-test its edge cases against pandemic scenarios, and supply the arguments both sides carry into court. They hold no enforcement power, collect nothing from the arrangement, and bear none of the disease risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bioethics_community, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a mutual-restraint settlement around bodily inviolability: every person's medical participation is secured against collective override, which stabilizes trust in clinical medicine, protects dissenting minorities from majoritarian health politics, and gives clinicians a consent framework that does not renegotiate under pressure.
% TRANSFER_FUNCTION: Moves disease-exposure risk from the general population onto those who cannot secure protection through their own choices — the immunocompromised, the medically fragile elderly, and the workers who staff surges — and moves assurance, standing, and organizational resources toward uncoerced individuals and the movements organized around them; during crises it also moves litigation volume and advocacy revenue toward liberty organizations.
% ABSENT_VOICES: The immunocompromised and their advocates appear in bioethics journals but are structurally absent from the forums that administer the settlement: courts hear liberty claims brought by refusalants, while no comparable standing exists for aggregate mortality arguments on behalf of the unprotected. Public health authorities hold no seat in the settlement's administration at all.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, epidemic governance would reorganize around compulsion: employment and school conditionality would normalize within a few seasons, agencies would deploy mandate authority as standard tooling, the vulnerable's exposure risk would compress while refusalants' protections evaporated, and the litigation market built on the consent line would collapse — the liberty-advocacy sector would lose its organizing object.
% FOUNDING_PROBLEM: State and institutional temptation to override bodily consent under collective pressure: the twentieth-century record of coerced experimentation, sterilization, and research abuse, and the recurring wartime and epidemic impulse to conscript bodies for communal ends. The settlement was built to make medical participation unconditionally consensual so that medicine stays trustworthy enough for people to enter it voluntarily.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Nuremberg tribunal record and subsequent research-ethics codes attest the abusive-practice problem; public-health-ethics scholarship — much of it written by mandate sympathizers — concedes the historical basis while disputing the categorical scope; court opinions on both sides of mandate litigation cite the abuse lineage. No corroborating source attests that the problem is dead; the live dispute is over scope, not existence.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the settlement transfers real, concentrated, involuntary risk onto those least able to bear it, but the transfer is incomplete — voluntary uptake, therapeutics, and the vulnerable's own mitigation blunt it, and the reading accepts the residue knowingly as the price of the categorical rule. Suppression 0.58: the settlement forecloses an entire class of protective instruments for the vulnerable and is held in place by active judicial review against recurrent incursions; this is structural foreclosure, not violence. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio 0.15: the settlement functions substantively (courts really block compulsion, consent doctrine really governs practice), with a performative surplus during crises when liberty politics becomes signaling. Accessibility_collapse 0.60: granting the categorical premise collapses all compromise positions, but the premise itself is contested and intermediate regimes remain live in practice, so alternatives are only partly closed. Resistance 0.68: sustained, organized, institutional — agencies, portions of bioethics, employers, and emergency-mode courts press against the line every cycle. CYCLICAL PATTERN: the series shows a full crisis cycle — long quasi-stable plateau (t=0..70), crisis incursion and peak strain (t=73..74), litigation and partial restoration (t=76..78). The oscillation is driven by epidemic recurrence colliding with a categorical rule; the intermittent character is itself part of the mechanism, since each crisis briefly legitimizes instruments that the restored settlement then disavows. Base_properties values are measured at t=78, the post-restoration end state. RECEIPT SURFACE: gain_flow is authored as 'diffuse' as an affirmative checked claim — the settlement's core gain (personal sovereignty assurance) accrues to every individual holder in unaggregable slices; liberty movements capture real organizational rents but not the primary gain, and no named seat collects the transferred risk as benefit. fixing_cost is 'prohibitive': removing the settlement would require reversing entrenched substantive-due-process lines and surviving supermajority-level political resistance, vastly exceeding any fixer's benefit. The prohibitive-plus-diffuse combination is the piton cell by cell semantics; the structure here is a live contested settlement rather than atrophied performance, and downstream consumers should read the cell as a flag for review, not a verdict. COORDINATION TYPE: identity_coordination — the settlement's dominant function is maintaining the shared boundary norm of bodily inviolability, whose failure would dissolve the consent framework itself; the known gaming risk of identity-framed cover stories is handled by the naturality and framing omegas below.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the immunocompromised and elderly seats, the settlement is a hazard imposed by strangers' choices — a rule that prices their safety below others' prerogative. From the individual-decisionmaker and liberty-movement seats, the same structure is freedom's precondition and the guard against a coercive ratchet. From the courts' seat it is an administrable line that generates docket; from the agencies' seat it is an amputated toolkit. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. individual_medical_decisionmakers sit near the beneficiary end: full subsidy of protection, mobile exit, no enforcement burden returned. liberty_advocacy_movements sit nearer still: organized, identity-locked collectors of the settlement's persistence rents, with exit unthinkable. immunocompromised_patients and chronic_condition_elderly sit near the full-target end: powerless, trapped, bearing the arrangement's concentrated cost with no arbitrage. frontline_healthcare_workers derive mid-to-high: declared victims through surge exposure, damped by their secondary benefit from the same consent norms. constitutional_courts derive near-symmetric: they administer and bear docket and legitimacy strain without collecting. public_health_authorities derive high despite institutional power: the settlement's foreclosure of their instrument class is the cost they bear, and exclusion gives them no compensating flow. Larger spatial scope (national) modestly amplifies effective extraction for the trapped targets because verification of voluntary-coverage adequacy is harder at scale; the engine owns that modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state temptation to override consent under collective pressure, with an atrocity lineage — is live: every epidemic renews it, so there is no dead mandate being theatrically maintained and no sunset to declare. The classification discipline cuts both ways here. It prevents rope-washing (reading the vulnerable's risk burden as mere coordination cost of a noble mutual-disarmament pact) by keeping the victim declarations and the extractiveness series on the books; and it prevents snare-washing (reading the whole settlement as pure extraction) by keeping the genuine coordination function — trust stabilization, minority protection, clinician certainty — structurally declared. The mountain claim is submitted to the metrics rather than assumed: if the categorical form is a constructed settlement benefiting identifiable agents rather than a natural law, the false-summit signature and the naturality omega are the machinery that surfaces it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_settlement,
    'Is the categorical no-coercion principle a genuine moral law discoverable by reason (self-ownership as a natural feature of persons), or a constructed liberal-democratic settlement that benefits identifiable agents — uncoerced individuals and organized liberty movements — at the identified cost of the medically vulnerable?',
    'Cross-cultural and historical comparison of consent norms under survival pressure, combined with philosophical analysis of whether the deontological premise survives the extreme edge case (contagious, highly lethal pathogen with safe effective vaccine).',
    'If constructed, the mountain claim fails and the arrangement recomputes toward the tangled-rope/snare side with the liberty movements as concentrated beneficiaries; if natural, the mountain certification stands and the vulnerable''s burden reads as an acknowledged price of a moral absolute rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_settlement, conceptual, 'Whether the categorical settlement is natural law or constructed benefit-bearing arrangement (FSM-required omega).').

omega_variable(
    voluntary_uptake_threshold_contingency,
    'How much of the vulnerable''s extracted risk is intrinsic to the categorical rule versus contingent on voluntary uptake reaching effective coverage thresholds for each pathogen?',
    'Epidemiological modeling of voluntary-uptake equilibria per pathogen class (measles-grade transmissibility versus influenza-grade versus novel pandemic), comparing achievable coverage under persuasion-only regimes against herd-protection thresholds.',
    'If voluntary equilibrium sits below the protection threshold for high-consequence pathogens, the extraction is structural and severe, pushing the computed type toward the snare side; if thresholds are reachable voluntarily for most pathogens, the extraction is contingent and modest, supporting the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_uptake_threshold_contingency, empirical, 'Whether the risk transfer onto the vulnerable is structural or uptake-contingent.').

omega_variable(
    crisis_cycle_ratchet_direction,
    'Is the crisis cycle (incursion, litigation, restoration) converging — each cycle restoring less and ratcheting conditionality inward — or bounded, with full restoration each time?',
    'Compare restoration completeness across successive epidemic episodes (1918 influenza, HIV-era screening disputes, COVID-19 mandate wave, the next major event): measure how much post-crisis doctrine returns to the pre-crisis baseline versus retaining crisis-era conditionality.',
    'Converging cycles imply the categorical reading is steadily losing ground to the risk-stratification world and the settlement is drifting toward a conditioned regime; bounded cycles imply a durable settlement whose mountain claim is structurally reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_cycle_ratchet_direction, empirical, 'Direction of the crisis-cycle oscillation: ratchet toward conditionality or durable restoration.').

omega_variable(
    movement_identity_symmetry_test,
    'Would the liberty movements'' defense of the absolute survive a scenario in which compulsion demonstrably saved large numbers of their own constituents, or is the defense constitutive of organizational identity such that no evidence could move it?',
    'Vignette surveys of movement rank-and-file under symmetry conditions, plus historical analysis of internal splits when members'' interests diverged from the categorical line.',
    'If identity-locked, the settlement''s enforcement depends on the movements'' continued vitality — a fading movement would leave the rule under-defended and accelerate drift; if principled, enforcement is robust to membership turnover and the identity-lock reading of the beneficiary seat is wrong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(movement_identity_symmetry_test, empirical, 'Whether the beneficiary movements'' commitment is evidential or identity-constitutive.').

omega_variable(
    cs_framing_lineage_vs_distributed,
    'Is the correct commitment-system framing ''constitutional lineage adjudicated by courts'' (declared: a designated interpreter exists and issues binding rulings), or ''distributed epistemic contest among three readings with no designated interpreter'' (bioethics and public discourse as dispersed authority)?',
    'Examine whether judicial rulings actually settle practice between crises or merely pause it: if agencies, legislatures, and international bodies continuously relitigate the line regardless of precedent, the distributed framing better describes the operative authority; if precedent binds between crises, lineage holds.',
    'Under the distributed framing, interpretation_layer_present becomes invalid, authority_grounding shifts to distributed, and the kernel''s classification pattern changes from court-centered lineage maintenance to open multi-party contest; the declared framing was chosen because binding rulings (e.g., the strike-down of the broadest federal mandate instrument) demonstrably reset practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_lineage_vs_distributed, conceptual, 'Framing under-determination in the commitment-system classification of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vml_bodily_autonomy_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t58, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 58, 0.12).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t70, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 70, 0.13).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t73, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 73, 0.18).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t74, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 74, 0.22).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t76, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 76, 0.18).
narrative_ontology:measurement(vml_bodily_autonomy_tr_t78, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(vml_bodily_autonomy_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(vml_bodily_autonomy_be_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(vml_bodily_autonomy_be_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(vml_bodily_autonomy_be_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 45, 0.41).
narrative_ontology:measurement(vml_bodily_autonomy_be_t58, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 58, 0.44).
narrative_ontology:measurement(vml_bodily_autonomy_be_t70, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 70, 0.47).
narrative_ontology:measurement(vml_bodily_autonomy_be_t73, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 73, 0.58).
narrative_ontology:measurement(vml_bodily_autonomy_be_t74, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 74, 0.66).
narrative_ontology:measurement(vml_bodily_autonomy_be_t76, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 76, 0.63).
narrative_ontology:measurement(vml_bodily_autonomy_be_t78, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 78, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vml_bodily_autonomy_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(vml_bodily_autonomy_su_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(vml_bodily_autonomy_su_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(vml_bodily_autonomy_su_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(vml_bodily_autonomy_su_t58, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 58, 0.53).
narrative_ontology:measurement(vml_bodily_autonomy_su_t70, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 70, 0.55).
narrative_ontology:measurement(vml_bodily_autonomy_su_t73, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 73, 0.62).
narrative_ontology:measurement(vml_bodily_autonomy_su_t74, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 74, 0.72).
narrative_ontology:measurement(vml_bodily_autonomy_su_t76, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 76, 0.68).
narrative_ontology:measurement(vml_bodily_autonomy_su_t78, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 78, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'vaccine mandate legitimacy' decomposes into three constraint stories per the epsilon-invariance principle: the public_health_primacy_reading (mandate authority as justified baseline; unvaccinated as externality; lowest epsilon from its seat), the risk_stratification_reading (actuarial-threshold conditionality; intermediate epsilon and a threshold-dependent victim set), and this bodily_autonomy_primacy_reading (categorical prohibition; victim set centered on the immunocompromised and medically vulnerable who bear the risk a coercion-capable regime would suppress). The upstream story is the public-health reading, which holds operative institutional power and is cited as the practical default; this reading functions as the categorical boundary constraint that the upstream story's exercises of authority press against. Each story carries its own epsilon, beneficiaries, victims, and claimed type; the family is linked through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
