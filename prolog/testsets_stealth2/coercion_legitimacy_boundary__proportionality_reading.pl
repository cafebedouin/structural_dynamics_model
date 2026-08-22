% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Coercion Legitimacy Boundary — Proportionality Reading (Severity-Scaled Medical Mandates)
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the coercion_legitimacy_boundary
 *   kernel: the proportionality_reading, under which the legitimacy of
 *   compelling medical intervention scales with measured disease severity and
 *   transmission dynamics — measles-class pathogens (high transmissibility,
 *   serious complication rates, no curative treatment) justify school-entry
 *   mandates and outbreak exclusion orders, while influenza-class pathogens
 *   (lower transmissibility, lower severity, partial pharmaceutical
 *   mitigation) do not justify general-population compulsion. Per the
 *   epsilon-invariance discipline, this file authors only this reading: one
 *   stable epsilon over one standing arrangement (the severity-calibrated
 *   mandate regime as it actually operates — school-entry laws, outbreak
 *   orders, employment conditions, and the case-by-case adjudication that
 *   governs them), assessed by this reading's own lights. CONSTRAINT FAMILY
 *   NOTE: the kernel decomposes into three readings — this
 *   proportionality_reading, a public_health_primary reading (coercion
 *   available wherever aggregate harm-prevention outweighs autonomy, no
 *   severity-scaling), and a bodily_autonomy_primary reading (non-consensual
 *   intervention categorically impermissible). Each is a separate constraint
 *   story with its own epsilon, victim set, and classification, linked via
 *   network.affects_constraints. The epsilons differ because the victim sets
 *   differ structurally: the collective-harm reading maximizes the coerced
 *   set, the autonomy reading empties it, and this reading sizes it per
 *   pathogen — hence the moderate, episode-dependent extraction authored
 *   here. The claim/metric gap is deliberate: claimed_type is stated from
 *   what I believe structurally true (a genuine coordination function joined
 *   to asymmetric, episodically-triggered extraction, actively enforced), and
 *   the metrics are authored from what I believe descriptively true; the
 *   engine computes per-seat classifications independently.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/constrained) — converts surveillance thresholds into enforceable orders; receives compliance as authority
 *   - constitutional_courts: agenda-setter (institutional/constrained) — defines where the boundary sits through precedent
 *   - immunocompromised_individuals: primary beneficiary (powerless/trapped) — protection depends wholly on others' mandated compliance
 *   - schoolchildren_in_outbreak_districts: beneficiary with secondary payer exposure (powerless/trapped) — protected by outbreak-triggered rules; unvaccinated classmates bear exclusion
 *   - conscientious_objector_parents: primary target (moderate/constrained) — bears exclusion and compulsion costs in triggered episodes
 *   - mandated_healthcare_workers: target (organized/constrained) — employment-conditioned vaccination during severe seasons
 *   - low_risk_adults_in_pandemic_episodes: target in gray-zone episodes (moderate/constrained) — coercion whose proportionality is contested case-by-case
 *   - minors_subject_to_exclusion: excluded voice (powerless/trapped) — acted upon with no procedural seat
 *   - public_health_ethicists: analytical observer (analytical/analytical) — sees the full tradeoff structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.52).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.6).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Coercion Legitimacy Boundary — Proportionality Reading (Severity-Scaled Medical Mandates)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '1b601d97-7230-40cc-8813-647804e46e79').
narrative_ontology:cs_kernel_codification('1b601d97-7230-40cc-8813-647804e46e79', formalized).
narrative_ontology:cs_authority_grounding('1b601d97-7230-40cc-8813-647804e46e79', lineage).
narrative_ontology:cs_interpretation_layer_present('1b601d97-7230-40cc-8813-647804e46e79').
narrative_ontology:cs_reading_relation('1b601d97-7230-40cc-8813-647804e46e79', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_reading_relation('1b601d97-7230-40cc-8813-647804e46e79', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('1b601d97-7230-40cc-8813-647804e46e79', foundational, coercion_proportional_to_epidemiological_threat).
narrative_ontology:cs_axiom_status(coercion_proportional_to_epidemiological_threat, holdable).
narrative_ontology:cs_axiom_grounding('1b601d97-7230-40cc-8813-647804e46e79', coercion_proportional_to_epidemiological_threat, instrumental).
narrative_ontology:cs_axiom('1b601d97-7230-40cc-8813-647804e46e79', secondary, case_by_case_pathogen_adjudication).
narrative_ontology:cs_axiom_status(case_by_case_pathogen_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('1b601d97-7230-40cc-8813-647804e46e79', case_by_case_pathogen_adjudication, empirically_contingent).
narrative_ontology:cs_reference_frame('1b601d97-7230-40cc-8813-647804e46e79', jacobson_reasonable_necessity_framework).
narrative_ontology:cs_drift_state('1b601d97-7230-40cc-8813-647804e46e79', contemporary_post_pandemic, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b601d97-7230-40cc-8813-647804e46e79', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, schoolchildren_in_outbreak_districts).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, conscientious_objector_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mandated_healthcare_workers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, low_risk_adults_in_pandemic_episodes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, schoolchildren_in_outbreak_districts).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, jacobson_reasonable_necessity_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, harm_principle_proportionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue and administer communicable-disease control orders: school-entry immunization requirements, outbreak exclusion rules, isolation and quarantine directives, and workplace vaccination conditions during severe seasons. When a pathogen's measured severity and transmissibility cross the thresholds the framework recognizes, these agencies convert surveillance data into enforceable orders; below the thresholds they rely on recommendation and education. They receive compliance and the legal authority behind it, and they bear the political cost when orders outrun perceived threat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Review challenges to specific mandates and exclusions, deciding whether a given order clears the reasonableness-and-necessity bar the doctrine sets. Their precedents define where the boundary sits for future episodes; they neither collect compliance nor bear it, but their dockets fill whenever an order is contested.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Cannot safely receive certain vaccines and depend on the immunity of the people around them. Proportionate mandates during high-threat outbreaks are the main protection available to them; they have no personal exit from exposure risk short of total isolation, and their protection depends entirely on others' compliance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Attend schools where a single case can close classrooms. During measles-type outbreaks, exclusion orders and school-entry rules interrupt transmission chains; compliant families receive protection they could not arrange individually, while unvaccinated classmates face barred doors until clearance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, schoolchildren_in_outbreak_districts, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, schoolchildren_in_outbreak_districts, payer).

% Hold religious or philosophical objections to vaccination and raise children in districts where outbreak-triggered rules convert objection into exclusion or compulsion. Available exits — medical or belief exemptions where granted, homeschooling, private school, relocation — all carry real costs in money, time, or community ties, and several jurisdictions have narrowed or removed the belief-based routes.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, conscientious_objector_parents, payer,
    moderate, biographical, constrained, regional).

% Work in hospitals and clinics that condition employment on vaccination during severe respiratory seasons and outbreak responses, on the theory that staff protect patients who cannot refuse exposure. Nursing associations and physician groups negotiate testing-and-masking alternatives where offered; refusal ends in reassignment or termination.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mandated_healthcare_workers, payer,
    organized, biographical, constrained, national).

% Face population-wide mandates during pandemics whose severity for their demographic is materially lower than for the elderly or chronically ill. The framework must decide case-by-case whether their coercion is justified; they experience the answer as unpredictable, since the same conduct is lawful in one season and penalized in the next, and their practical exits — remote work, jurisdictional differences — are unevenly available.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, low_risk_adults_in_pandemic_episodes, payer,
    moderate, biographical, constrained, national).

% Are the bodies the whole apparatus acts upon: vaccinated, excluded, or compelled by decisions made by parents, agencies, and courts. Adolescents old enough to hold views about their own medical care have no procedural seat — they cannot sue, vote, or contract — and their interests sometimes diverge from the adults speaking for them, in both directions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, minors_subject_to_exclusion, excluded,
    powerless, immediate, trapped, local).

% Analyze the tradeoff structure: which threats justify overriding refusal, how thresholds should be set, and where the case-by-case method produces consistency or whiplash. They publish in journals and advise committees; they hold no enforcement power and bear none of the costs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches the intensity of compulsory medical intervention to measured epidemiological threat: where voluntary uptake leaves a community-immunity deficit against a fast, dangerous pathogen, compulsion closes the free-rider gap; where threat is low, it stands down and preserves consent. The rule solves the infectious-disease collective-action problem while bounding official overreach.
% TRANSFER_FUNCTION: Transfers bodily autonomy and decisional liberty from non-consenting individuals in triggered episodes to the surrounding population's protection — most concretely to those who cannot be vaccinated — and transfers compliance costs and legal risk onto objecting parents and mandated workers; agencies receive the compliance itself as exercisable authority.
% ABSENT_VOICES: Minors subject to exclusion hold no procedural seat anywhere in the adjudication; parents speak for them and adolescent interests sometimes diverge from parental ones. Residents of jurisdictions that have eliminated belief exemptions have no forum once the legislature acts. Populations in low-income countries subject to donor-conditioned vaccination programs are outside the adjudicating conversation entirely.
% DISAPPEARANCE_RATIONALE: If the severity-scaling boundary vanished overnight, mandate law would collapse to one of two categorical poles — compulsion for any disease wherever aggregate harm-prevention wins, or no compulsion ever. School-entry statutes, outbreak exclusion protocols, and employer conditions would all be rewritten; the protection of the unvaccinable and the liberty of objectors would be repriced simultaneously. Named parties visibly depend on where the boundary sits, so the world rearranges.
% FOUNDING_PROBLEM: Compulsory smallpox vaccination in the early twentieth century posed the question the framework still answers: when may the state invade bodily integrity to prevent collective harm? The settled answer took the form of a reasonableness test — coercion is legitimate where disease prevalence makes the measure necessary, efficacious, and neither arbitrary nor oppressive.
% FOUNDING_PROBLEM_CORROBORATION: Judicial doctrine outside the health bureaucracy corroborates both the problem and its continuing life: the century-long case line running from Jacobson v. Massachusetts through modern heightened-scrutiny applications re-adjudicates the boundary in nearly every major episode. Independent bioethics scholarship grounding the scaling in the harm principle, and legislative hearing records from the exemption battles, attest from seats that collect no compliance.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end) because the arrangement's coercion is episodic and calibrated rather than constant: between outbreaks it extracts almost nothing, while in triggered episodes the non-consenting minority bears the full cost of compulsion or exclusion, and in gray-zone pandemics the case-by-case method leaves large populations facing coercion whose justification is contested. Suppression (0.60) reflects the active enforcement machinery — school exclusion, employment termination, quarantine powers, narrowed exemptions — tempered by the persistence of partial alternatives (belief exemptions in many jurisdictions, homeschooling, private schooling, jurisdictional variation), which is why accessibility_collapse is low (0.35): alternatives survive understanding of the constraint. Resistance (0.58) is real and sustained: litigation waves, parental-rights movements, legislative repeal fights, and healthcare-worker union pushback; payer-side coalition capacity is material and has repeatedly changed state law. Theater_ratio (0.30) captures a growing performative layer — ritualized risk communication, dashboard maintenance, 'following the science' framing — atop a substantively functional epidemiological adjudication. The temporal series run on ONE shared grid (T=0 approximates the 1905 Jacobson decision; T=115 the 2020 pandemic peak; T=120 the 2025 settlement) so every tracked metric is authored at every examined point. The trajectories are quasi-cyclical rather than monotonic: extraction and enforcement rise during epidemic episodes and relax between them. Critically, much of this oscillation is the reading FUNCTIONING as designed — coercion legitimately scales with threat — so the analysis must distinguish designed episodic variation from genuine drift; the residual upward creep in the troughs (0.38 to 0.46 between mid-century and the 2010s) and the post-peak plateau above prior baselines are the drift signal. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the payer seats, the boundary arrives as state power timed to peak fear: an objector parent experiences the measles-order as compulsion backed by classroom exclusion, and a mandated worker experiences the seasonal condition as a choice between bodily integrity and livelihood. From the beneficiary seats, the identical instrument is a lifeline that cannot be purchased any other way — the immunocompromised patient has no market, no lawsuit, and no exit that produces community immunity. From the agenda-setter seats, the same structure is professional duty under legal constraint: agencies see calibration, courts see doctrine. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidy end: immunocompromised individuals (trapped, powerless — maximal dependence, no exit) and outbreak-district schoolchildren derive protection they cannot self-provision, so their effective extraction is damped toward or below zero. Declared victims sit near the full-target end: objector parents, mandated healthcare workers, and gray-zone pandemic cohorts bear the transfer directly, with constrained (not arbitrage-grade) exit keeping them near the target pole — trapped or immobile targets amplify effective extraction, and the constrained-but-real exits here keep the amplification moderate rather than extreme. Public health agencies occupy a genuinely dual position the derivation alone under-captures: they RECEIVE the compliance (receipt seat for gain_flow) yet are simultaneously bounded by the very boundary they administer — their authority exists only inside its limits — placing them nearer symmetric than beneficiary. Constitutional courts are near-neutral administrators: they define the boundary, collect no compliance, and bear none of it. Ethicists are analytical. Scope amplification applies modestly: the regime operates nationally with verification difficulty varying by jurisdiction, supporting moderate rather than severe amplification of the payer-seat extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure extraction (because it compels bodies) would erase the genuine coordination function: free-riding on community immunity is a real collective-action failure that voluntary uptake demonstrably under-supplies exactly when threat peaks, and the severity-scaling rule is what prevents the coordination tool from becoming indiscriminate. Reading it as pure coordination (because the epidemiology is real) would erase the identifiable payers: objector parents, mandated workers, and gray-zone cohorts bear uncompensated, asymmetry-bearing costs enforced by schools, employers, and courts. The founding problem — when may the state invade bodily integrity for collective protection — is LIVE, re-adjudicated in every major episode, so there is no mandate-atrophy: the arrangement is not a vestige performing a dead function, and the founding_problem_status x disappearance_verdict pair (live x world_rearranges) raises no zombie flag. The theater_ratio trend bears watching: if performative risk-communication continues substituting for threshold adjudication, the arrangement would drift toward inertial maintenance, but the current record shows function leading performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (proportionality_reading) of the coercion_legitimacy_boundary kernel; what structurally changes if a sibling reading is adopted instead?',
    'Adoption of public_health_primary would delete the severity-scaling premise, making coercion available for any disease where aggregate harm-prevention prevails — the victim set expands toward everyone non-consenting in any episode and epsilon rises sharply. Adoption of bodily_autonomy_primary would delete triggered coercion entirely — the victim set empties for interventions, epsilon approaches zero on this referent, and protection costs migrate to the unvaccinable. The disagreement is located in whether legitimacy is graded by measured threat or fixed categorically.',
    'Victim-set membership, per-episode classification, and epsilon all flip across readings; any cross-reading comparison must treat the three files as distinct constraints over one kernel, not one constraint under uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: reading position within the coercion-legitimacy kernel and the structural delta each sibling would produce.').

omega_variable(
    threshold_location_drift,
    'Where exactly do the severity and transmissibility thresholds sit, and are they stable — measles-class pathogens trigger compulsion and influenza-class pathogens do not, but what fixes the line between classes as vaccines, treatments, and demographics change?',
    'Comparative analysis of mandate outcomes across pathogens and eras: if the operative line tracks treatability and vaccine effectiveness rather than intrinsic severity, the threshold is drifting with technology, not anchored in the pathogens.',
    'A technology-relative threshold means the victim set grows or shrinks with pharmaceutical progress rather than epidemiology, shifting epsilon per episode and potentially pulling borderline pathogens into the coerced class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_location_drift, empirical, 'Whether the measles/flu dividing line is stable or moves with treatment and vaccine technology.').

omega_variable(
    gray_zone_adjudication_consistency,
    'Does case-by-case adjudication of moderate-severity pathogens produce a consistent boundary, or does it oscillate with political attention — the same act lawful in one season and penalized in the next?',
    'Within-reading comparison of adjudicated outcomes for demographically similar exposures across episodes and jurisdictions; consistency would support the calibrated-coordination account, whiplash would indicate the adjudication layer is absorbing politics rather than epidemiology.',
    'Inconsistent adjudication raises effective extraction for gray-zone populations (unpredictable coercion carries its own burden), pushing the arrangement''s operation toward the extractive pole during pandemic episodes even if the written rule is moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_adjudication_consistency, empirical, 'Consistency versus politicized oscillation in the case-by-case method for moderate-severity diseases.').

omega_variable(
    exemption_pathway_accessibility,
    'Are exemption pathways (medical, religious, philosophical) a genuine exit for objectors, or formally available but practically inaccessible in the jurisdictions that matter?',
    'Track grant rates, processing burdens, and reversal of belief-exemption statutes across jurisdictions; post-repeal suppression trajectories show whether objectors retain workable alternatives or face a closed set.',
    'If exemptions are practically inaccessible, authored suppression understates the payer-seat reality, objector exit shifts from constrained toward trapped, and effective extraction for the payer seats rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_pathway_accessibility, empirical, 'Practical accessibility of exemption routes as the determinant of real payer exit options.').

omega_variable(
    occupational_setting_carveout,
    'Hospital influenza vaccination requirements for healthcare staff exist despite the reading''s headline that influenza-class pathogens do not justify compulsion — does the proportionality framework admit an occupational carve-out (patient vulnerability inside clinical settings), or does the carve-out falsify the simple severity-scaling premise?',
    'Doctrinal analysis of whether the reading''s grounding rationale (protection of those who cannot refuse exposure) extends the coerced class to occupational contacts of the vulnerable, and empirical study of whether staff mandates change patient outcomes enough to satisfy the necessity prong.',
    'If occupational carve-outs are admissible, the reading is more permissive than its headline and the routine victim set expands to include annually-mandated workers; if not, existing staff mandates are overreach the reading must condemn, and the observed practice departs further from the reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_setting_carveout, conceptual, 'Whether occupational-context mandates fit the severity-scaling premise or contradict it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_prop_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(clb_prop_tr_t0, observed).
narrative_ontology:measurement(clb_prop_tr_t20, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(clb_prop_tr_t20, observed).
narrative_ontology:measurement(clb_prop_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(clb_prop_tr_t40, observed).
narrative_ontology:measurement(clb_prop_tr_t60, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(clb_prop_tr_t60, observed).
narrative_ontology:measurement(clb_prop_tr_t80, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(clb_prop_tr_t80, observed).
narrative_ontology:measurement(clb_prop_tr_t100, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement_basis(clb_prop_tr_t100, observed).
narrative_ontology:measurement(clb_prop_tr_t115, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 115, 0.34).
narrative_ontology:measurement_basis(clb_prop_tr_t115, observed).
narrative_ontology:measurement(clb_prop_tr_t120, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement_basis(clb_prop_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(clb_prop_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(clb_prop_be_t0, observed).
narrative_ontology:measurement(clb_prop_be_t20, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(clb_prop_be_t20, observed).
narrative_ontology:measurement(clb_prop_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(clb_prop_be_t40, observed).
narrative_ontology:measurement(clb_prop_be_t60, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(clb_prop_be_t60, observed).
narrative_ontology:measurement(clb_prop_be_t80, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement_basis(clb_prop_be_t80, observed).
narrative_ontology:measurement(clb_prop_be_t100, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement_basis(clb_prop_be_t100, observed).
narrative_ontology:measurement(clb_prop_be_t115, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 115, 0.58).
narrative_ontology:measurement_basis(clb_prop_be_t115, observed).
narrative_ontology:measurement(clb_prop_be_t120, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 120, 0.52).
narrative_ontology:measurement_basis(clb_prop_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(clb_prop_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(clb_prop_su_t0, observed).
narrative_ontology:measurement(clb_prop_su_t20, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(clb_prop_su_t20, observed).
narrative_ontology:measurement(clb_prop_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement_basis(clb_prop_su_t40, observed).
narrative_ontology:measurement(clb_prop_su_t60, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement_basis(clb_prop_su_t60, observed).
narrative_ontology:measurement(clb_prop_su_t80, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement_basis(clb_prop_su_t80, observed).
narrative_ontology:measurement(clb_prop_su_t100, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(clb_prop_su_t100, observed).
narrative_ontology:measurement(clb_prop_su_t115, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 115, 0.68).
narrative_ontology:measurement_basis(clb_prop_su_t115, observed).
narrative_ontology:measurement(clb_prop_su_t120, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement_basis(clb_prop_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate legitimacy' conflates three structurally distinct commitments sharing one kernel (coercion_legitimacy_boundary). This file is the proportionality_reading: epsilon is moderate (0.52) because the victim set is sized per pathogen — high-transmission/high-severity diseases trigger coercion on a non-consenting minority, low-severity diseases trigger none, and gray-zone episodes are adjudicated case-by-case. The public_health_primary sibling deletes the scaling premise (victim set maximal, epsilon high); the bodily_autonomy_primary sibling deletes triggered coercion (victim set empty on this referent, epsilon near zero, with protection costs displaced onto the unvaccinable). The upstream/downstream structure runs from this reading toward public_health_primary: proportionality doctrine imposes evidentiary and threshold discipline on when collective-harm balancing may operate, changing the sibling's legitimacy conditions without eliminating it. Sibling IDs follow this file's naming convention; each sibling story links back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
