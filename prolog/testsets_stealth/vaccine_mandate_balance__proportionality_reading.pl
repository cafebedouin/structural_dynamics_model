% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Gate (Strict-Threshold Reading)
 *   domain: public health ethics / constitutional law / political philosophy
 *
 * SUMMARY:
 *   This story instantiates the proportionality_reading of the
 *   vaccine_mandate_balance kernel: the standing arrangement under contest is
 *   the proportionality-governed mandate regime in constitutional democracies
 *   from Jacobson (1905) through the post-emergency recalibration (2025) —
 *   the requirement that any compulsion clear strict severity,
 *   transmissibility, and safety thresholds and honor robust exemptions. Per
 *   the epsilon-referent rule, extractiveness is authored for THAT
 *   arrangement, assessed by this reading's own lights, never for the
 *   arrangements the sibling readings would install. The kernel decomposes
 *   into three constraint stories per the epsilon-invariance principle:
 *   public_health_primary authors high epsilon for the harms of refusal and
 *   under-vaccination; bodily_autonomy_primary authors high epsilon for any
 *   state-compelled injection; this reading authors moderate epsilon
 *   concentrated on conditional risk transfers (dependent-population exposure
 *   under widened exemptions, worker penalties under activated requirements).
 *   The three files are linked through network.affects_constraints; merging
 *   them into one story would produce an epsilon that swings with the
 *   pathogen, which is the exact instability decomposition exists to prevent.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: Agenda-setter (institutional/constrained) — administers the threshold inquiry, defines exemption adequacy, accumulates supervisory jurisdiction with each case
 *   - - public_health_agencies: Dual-positioned cost-bearer and legitimacy beneficiary (institutional/constrained) — bears the evidentiary burden and mid-campaign dissolutions; gains durable mandates when review is passed
 *   - - elected_officials: Instrument-bearer with political mobility (powerful/mobile) — loses policy tools to adverse rulings, gains or loses votes with each position
 *   - - religious_and_medical_exemption_holders: Primary beneficiary (powerless/mobile) — liberty protected by the exemption floor; can exit the protected status unilaterally by vaccinating
 *   - - herd_immunity_dependents: Primary cost-bearer (powerless/trapped) — bears residual exposure that widens with every exemption and every deferred requirement; cannot vaccinate into safety
 *   - - general_complying_public: Near-symmetric participant (organized/mobile) — carries the needle when requirements activate, receives the assurance that compulsion stays behind a demanding bar
 *   - - noncompliant_workers_under_active_mandates: Conditional cost-bearer (moderate/constrained) — employment consequences when thresholds are met and their objections fall outside accepted exemption categories
 *   - - bioethics_and_law_observers: Analytical observer (analytical/analytical) — tracks calibration across jurisdictions, holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.32).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.3).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Gate (Strict-Threshold Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public health ethics / constitutional law / political philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'ecf351ff-8773-4187-9817-bb35e4a37c3e').
narrative_ontology:cs_kernel_codification('ecf351ff-8773-4187-9817-bb35e4a37c3e', formalized).
narrative_ontology:cs_authority_grounding('ecf351ff-8773-4187-9817-bb35e4a37c3e', lineage).
narrative_ontology:cs_interpretation_layer_present('ecf351ff-8773-4187-9817-bb35e4a37c3e').
narrative_ontology:cs_reading_relation('ecf351ff-8773-4187-9817-bb35e4a37c3e', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('ecf351ff-8773-4187-9817-bb35e4a37c3e', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('ecf351ff-8773-4187-9817-bb35e4a37c3e', foundational, mandate_legitimacy_tracks_epidemiological_parameters).
narrative_ontology:cs_axiom_status(mandate_legitimacy_tracks_epidemiological_parameters, holdable).
narrative_ontology:cs_axiom_grounding('ecf351ff-8773-4187-9817-bb35e4a37c3e', mandate_legitimacy_tracks_epidemiological_parameters, empirically_contingent).
narrative_ontology:cs_axiom('ecf351ff-8773-4187-9817-bb35e4a37c3e', foundational, robust_exemptions_condition_all_compulsion).
narrative_ontology:cs_axiom_status(robust_exemptions_condition_all_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('ecf351ff-8773-4187-9817-bb35e4a37c3e', robust_exemptions_condition_all_compulsion, deontological).
narrative_ontology:cs_reference_frame('ecf351ff-8773-4187-9817-bb35e4a37c3e', bounded_police_power_strict_balancing).
narrative_ontology:cs_drift_state('ecf351ff-8773-4187-9817-bb35e4a37c3e', contemporary_post_emergency_recalibration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ecf351ff-8773-4187-9817-bb35e4a37c3e', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, religious_and_medical_exemption_holders).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, general_complying_public).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, herd_immunity_dependents).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, noncompliant_workers_under_active_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, elected_officials).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, general_complying_public).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review mandate statutes and emergency orders against the severity, transmissibility, and vaccine-safety record, deciding case by case whether the government has carried its justification and whether the exemption scheme offered is adequate. Each threshold case enlarges the court's supervisory role; judges cannot decline jurisdiction when challenged orders arrive, and precedent binds future panels.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Run immunization programs and draft the orders that compulsion rests on. Before requiring anything they must assemble the epidemiological case — attack rates, severity data, safety surveillance — and defend it in court; orders that fail review are dissolved mid-campaign. When their case succeeds, the resulting mandate carries a durability that ad hoc orders never had.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_agencies, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, public_health_agencies, beneficiary).

% Enact, amend, or repeal mandate statutes and answer politically for both outbreaks and compulsion. Litigation losses cost them policy instruments; litigation wins cost them votes in districts hostile to required injections. They can shift position between legislative sessions in ways career agency staff cannot.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, elected_officials, payer,
    powerful, biographical, mobile, national).

% Hold documented contraindications or sincere religious objections and rely on the exemption channel to remain employed and enrolled without accepting the injection. Their protection is only as wide as the exemption categories the reviewing court accepts, and they can end their protected status at any time by choosing vaccination.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, religious_and_medical_exemption_holders, beneficiary,
    powerless, biographical, mobile, national).

% Live with transplanted organs, ongoing chemotherapy, or advanced age — conditions that make vaccination ineffective or impossible. Their safety depends on the immunity of people around them, so every widening of exemptions and every season of deferred requirements translates directly into exposure they cannot insure against or opt out of.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, herd_immunity_dependents, payer,
    powerless, biographical, trapped, national).

% Vaccinate on schedule and carry the obligations whenever requirements attach — the injection, the documentation, the workplace rules. In return they get assurance that compulsion is reserved for situations that have cleared a demanding evidentiary bar, and they vote as the majority whose tolerance sets the political ceiling on both official overreach and mass refusal.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, general_complying_public, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, general_complying_public, payer).

% Work in covered sectors — hospital systems, school districts, large employer rolls — where an active requirement attaches consequences to continued refusal: unpaid leave, termination, exclusion from campus. Exemption channels exist, but applications are adjudicated by employers and reviewing courts, and denials end in job loss.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, noncompliant_workers_under_active_mandates, payer,
    moderate, immediate, constrained, national).

% Track mandate litigation across jurisdictions, publish analyses of how the threshold inquiry is calibrated, and advise commissions drafting guidance. They hold no enforcement power and bear none of the risks; their stake is the coherence of the standard itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bioethics_and_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, religious_and_medical_exemption_holders).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, predictable standard for when collective medical compulsion is legitimate: it fixes ex ante evidentiary thresholds (disease severity, transmission risk, vaccine safety) and an exemption floor, so that each new outbreak does not restart a first-principles fight over bodily integrity versus collective protection.
% TRANSFER_FUNCTION: Moves decision-making authority over bodily intervention from discretionary state hands to a judicially administered evidentiary threshold; moves residual infection risk onto people who depend on community immunity when exemptions widen coverage gaps; moves compliance costs onto workers in covered sectors when thresholds are met and requirements activate.
% ABSENT_VOICES: Herd-immunity-dependent patients are almost never parties to exemption litigation — their interests enter as amicus statistics, not as rights-holders with counsel. Immunization-program managers in low-resource settings operating under different epidemiological baselines are absent from the case law that sets the thresholds. The parameters of future pathogens are unknowable at adjudication time, so no voice speaks for the next variant.
% DISAPPEARANCE_RATIONALE: If the proportionality gate vanished overnight, mandate legitimacy would collapse back into raw political contest: jurisdictions would polarize between compulsion-on-demand and prohibition-on-principle depending on which faction held power, every outbreak would relitigate first principles, and exemption law would oscillate state by state with each election cycle.
% FOUNDING_PROBLEM: The Jacobson-era problem: reconciling compulsory-vaccination statutes with liberal constitutional limits — preventing both epidemic catastrophe and unchecked medical coercion. The proportionality reading was built to institutionalize the middle position between the two absolutist resolutions of that problem.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholarship on proportionality doctrine and successive bioethics commission reports (Nuffield Council, national pandemic-ethics bodies) attest from outside the benefiting parties that the founding problem recurs with each novel pathogen. Notably, advocates of both sibling readings also attest the problem is live — they dispute the answer, not the question — which is itself corroboration that no resolution has been reached.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim rope because the gate solves a genuine collective-action problem (making coercive public-health power predictable and bounded), its participants are net beneficiaries across the calm-phase/crisis-phase cycle, its coercive overhead is judicial rather than carceral, and it suppresses no alternative — voluntary programs, incentives, and targeted workplace rules all remain open. The metrics describe actual operation: extractiveness 0.32 reflects that the gate's costs are conditional risk transfers and foreclosed discretion rather than collected rents; suppression 0.30 is the structural coercion embedded in the arrangement (adjudicated denial of exemptions ends in job loss), deliberately distinguished from suppression_requirement, which traces the enforcement effort courts must expend to hold the gate against crisis-driven expansion — the requirement (0.46 at interval end) exceeds realized suppression because enforcement frequently fails against legislative defiance. Theater 0.18: the balancing is mostly functional, with a performative residue in opinions that recite strict scrutiny while deferring. Accessibility_collapse 0.22: understanding the gate collapses almost no alternatives. Resistance 0.48: sustained pushback from emergency-mode agencies (thresholds feel like handcuffs during surges) and from autonomy absolutists (any compulsion is illegitimate). The temporal series runs on one shared eight-point grid (1905-2025) with every tracked metric authored at every point. The trajectories are episodic rather than monotonic: the 2020 bump in extractiveness and theater and the spike in suppression_requirement are pathogen-event shocks, not intermittent reinforcement — the oscillation is driven by external epidemiology, not by the arrangement harvesting its own crises.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is structural, not rhetorical. From the agency seat the gate is a procedural gauntlet that dissolves orders mid-campaign; from the exemption-holder seat it is a shield that holds regardless of political weather; from the herd-dependent seat it is an uncovered exposure that widens with each exemption granted; from the bench it is a maturing doctrine; from the complying majority it is a fair price schedule for compulsion. Same structure, divergent per-seat classifications — the engine computes this from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Exemption holders (beneficiary, mobile exit) derive near the beneficiary pole — correct: the gate subsidizes their liberty. Herd-immunity dependents (victim, trapped) derive near the target pole — correct and intentionally left to derivation: their cost-bearing is acute and their exit is epidemiologically impossible. Noncompliant workers (victim, constrained) derive high-target — correct for activated-requirement phases. Two corrections are warranted. First, general_complying_public is listed as beneficiary but is equally the population actually injected when requirements activate; the derivation would over-weight the benefit side, so an override to d=0.45 places the seat near symmetric. Second, public_health_agencies are listed as beneficiaries (they harvest legitimacy when review is passed) but bear the heaviest operational burden in crisis phases; the derivation would understate their cost side. Because overrides key on the power atom and cannot distinguish the two institutional seats (courts vs. agencies), I declined an institutional-level override — it would misfire onto the courts, whose referee position is already handled through the agenda_setter role — and routed the agency ambiguity to the omega agency_net_position_phase_variance instead. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is resolved here: the founding problem (bounding medical compulsion without paralyzing epidemic response) is live, corroborated from outside the benefiting parties, and re-instantiated by every novel pathogen. The classification work this reading performs is preventive: without the proportionality gate, crisis-phase operation reads as pure extraction (the dependent population pays, the refusing minority pays, nobody visibly coordinates) and calm-phase operation reads as pure coordination — the gate is what makes the type itself parameter-indexed rather than phase-indexed. The drift watch-item is theatrical rot: if the threshold inquiry degenerates into rote deference (as in the early Jacobson era, theater 0.30), the gate persists as performance while agencies and legislatures do as they please, and the structure would migrate toward inertia. The 2005-2025 theater decline is the evidence that this has not yet happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading (proportionality_reading) of the vaccine_mandate_balance kernel; which structural elements does it share with the sibling readings (public_health_primary, bodily_autonomy_primary), and at which elements does epsilon actually diverge?',
    'Compile all three sibling stories and compare victim sets, beneficiary sets, and per-seat chi: shared elements are the mandate apparatus itself; divergent elements are the burden of proof and the exemption treatment.',
    'If the three readings were analyzed as one merged constraint, epsilon would swing with whichever sibling''s referent the analyst defaulted to — decomposition into three stories with linked network edges is mandatory for stable classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel membership: this story is one of three readings; merger would destabilize epsilon.').

omega_variable(
    epsilon_pathogen_parameter_variance,
    'The reading makes mandate legitimacy conditional on disease parameters, so the epsilon of any CONCRETE mandate varies from smallpox-like to seasonal-flu-like pathogens — is this story''s epsilon fixed at the meta-rule level, or does each pathogen regime need its own story?',
    'Decompose further when concrete mandate regimes enter the corpus: author one story per pathogen-parameter regime (e.g., smallpox-era arrangement vs. influenza-era arrangement), each with its own stable epsilon, linked to this meta-rule story.',
    'Scoring a smallpox-era arrangement and a flu-era arrangement on this story''s single epsilon would date type transitions wrong and mask the reading''s core feature — that legitimacy is context-dependent rather than categorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_pathogen_parameter_variance, conceptual, 'Whether per-pathogen instances require separate stories beneath this meta-rule.').

omega_variable(
    threshold_calibration_source,
    'Who calibrates ''strict'' thresholds — what attack rate, what severity measure, what safety-surveillance standard counts as clearing the bar — and on what evidentiary basis?',
    'Comparative analysis of threshold application across jurisdictions and over time: which inputs courts actually weighed, which they recited without weighing.',
    'Loose calibration drifts the reading''s operation toward public_health_primary (bar clears easily); strict calibration drifts it toward bodily_autonomy_primary (bar rarely clears) — the reading''s realized position between its siblings is set here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_source, empirical, 'Calibration of the strictness thresholds determines the reading''s operative distance from each sibling.').

omega_variable(
    robust_exemption_scope_definition,
    'Does ''robust'' exemptions mean medical-plus-religious (the current constitutional floor in the leading jurisdiction) or must it extend to philosophical and personal-conviction objection?',
    'Doctrinal tracking of exemption-category litigation plus coverage-gap epidemiology: what exemption width produces what herd-immunity deficit in what population structure.',
    'A narrower definition shrinks the victim set attributable to exemption-driven coverage gaps and pulls the reading toward public_health_primary; a wider definition expands the dependent population''s exposure and sharpens the conditional cost transfer this story documents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(robust_exemption_scope_definition, preference, 'The scope of ''robust'' sets the size of the exemption-driven victim set.').

omega_variable(
    residual_risk_attribution,
    'Is the infection risk borne by herd-immunity dependents an attributable cost of this arrangement (produced by exemption width and threshold delay), or background epidemiological risk that exists under any governance regime?',
    'Counterfactual comparison across regimes with different exemption widths and threshold standards, holding pathogen constant: the attributable share is the difference in dependent-population incidence.',
    'Full attribution strengthens the victim declarations and raises computed extraction toward the dependent seat; zero attribution dissolves the victim set and leaves the gate as near-pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_attribution, empirical, 'Attribution of dependent-population risk to the arrangement versus background.').

omega_variable(
    agency_net_position_phase_variance,
    'Are public_health_agencies net beneficiaries or net cost-bearers of the gate — does the legitimacy harvested when review is passed outweigh the evidentiary burden and mid-campaign dissolutions?',
    'Phase-resolved accounting: compare agency outcomes in calm phases (orders survive, programs stabilize) against crisis phases (orders enjoined, campaigns dissolved), weighted by frequency.',
    'Net-beneficiary finding confirms the beneficiary listing and low derived directionality; net-cost-bearer finding would warrant treating agencies as a second victim class and raise computed extraction at the institutional seat. An atom-level directionality override was deliberately avoided because it cannot target agencies without also moving the courts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_net_position_phase_variance, empirical, 'Phase-dependent net position of the agency seat; unresolved pending phase-resolved data.').

omega_variable(
    cs_framing_doctrine_vs_ritual,
    'Is the commitment system here the constitutional doctrine itself (formalized kernel, lineage authority, judiciary as interpreter), or the practiced legitimacy ritual layered above political bargaining (implicit kernel, practice authority — the test as whatever courts actually do)?',
    'Compare declared framing against observed practice: if threshold application tracks the stated multi-prong test, the doctrine framing holds; if outcomes track political salience while opinions recite the test, the ritual framing fits and the kernel is implicit.',
    'Under the ritual framing, kernel_codification shifts to implicit and authority_grounding to practice, the theater_ratio reading rises, and the drift assessment shifts from practice_drift to codification_collapse — a materially different commitment-system classification from the same conduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_doctrine_vs_ritual, conceptual, 'Two coherent framings of the same commitment system yield different cs_structure classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t1905, vaccine_mandate_balance__proportionality_reading, theater_ratio, 1905, 0.3).
narrative_ontology:measurement_basis(vacc_tr_t1905, observed).
narrative_ontology:measurement(vacc_tr_t1944, vaccine_mandate_balance__proportionality_reading, theater_ratio, 1944, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t1944, observed).
narrative_ontology:measurement(vacc_tr_t1977, vaccine_mandate_balance__proportionality_reading, theater_ratio, 1977, 0.26).
narrative_ontology:measurement_basis(vacc_tr_t1977, observed).
narrative_ontology:measurement(vacc_tr_t1990, vaccine_mandate_balance__proportionality_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(vacc_tr_t1990, observed).
narrative_ontology:measurement(vacc_tr_t2005, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t2005, observed).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(vacc_tr_t2020, observed).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement_basis(vacc_tr_t2022, observed).
narrative_ontology:measurement(vacc_tr_t2025, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t1905, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 1905, 0.58).
narrative_ontology:measurement_basis(vacc_be_t1905, observed).
narrative_ontology:measurement(vacc_be_t1944, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 1944, 0.52).
narrative_ontology:measurement_basis(vacc_be_t1944, observed).
narrative_ontology:measurement(vacc_be_t1977, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 1977, 0.44).
narrative_ontology:measurement_basis(vacc_be_t1977, observed).
narrative_ontology:measurement(vacc_be_t1990, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement_basis(vacc_be_t1990, observed).
narrative_ontology:measurement(vacc_be_t2005, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement_basis(vacc_be_t2005, observed).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement_basis(vacc_be_t2020, observed).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2022, 0.38).
narrative_ontology:measurement_basis(vacc_be_t2022, observed).
narrative_ontology:measurement(vacc_be_t2025, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2025, 0.32).
narrative_ontology:measurement_basis(vacc_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t1905, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 1905, 0.35).
narrative_ontology:measurement_basis(vacc_su_t1905, observed).
narrative_ontology:measurement(vacc_su_t1944, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 1944, 0.38).
narrative_ontology:measurement_basis(vacc_su_t1944, observed).
narrative_ontology:measurement(vacc_su_t1977, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 1977, 0.42).
narrative_ontology:measurement_basis(vacc_su_t1977, observed).
narrative_ontology:measurement(vacc_su_t1990, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement_basis(vacc_su_t1990, observed).
narrative_ontology:measurement(vacc_su_t2005, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement_basis(vacc_su_t2005, observed).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement_basis(vacc_su_t2020, observed).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(vacc_su_t2022, observed).
narrative_ontology:measurement(vacc_su_t2025, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2025, 0.46).
narrative_ontology:measurement_basis(vacc_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate balance' covers three structurally distinct constraints instantiated by three readings of one kernel. This file is the proportionality_reading. Upstream/downstream structure: the proportionality gate sits upstream of any supremacy-based mandate program — public_health_primary's operating environment (what evidence must be produced, how wide exemptions must run) is conditioned by this reading's thresholds, hence the influences edge; bodily_autonomy_primary coexists as a live rival held by different parties with no logical elimination in either direction. Epsilon differs across the family by construction: public_health_primary authors high epsilon for refusal-side harms, bodily_autonomy_primary authors high epsilon for compulsion itself, this reading authors moderate epsilon over conditional risk transfers. Each member links to the other two via affects_constraints; orphaning any member would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
