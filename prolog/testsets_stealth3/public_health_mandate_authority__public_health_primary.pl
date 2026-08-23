% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public-Health-Primary Reading of the Vaccination Mandate Regime
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   A layered vaccination-mandate regime — federal workplace rules, facility
 *   staff requirements, school entry laws, employer policies — is justified
 *   as a collective-protection obligation owed to the unprotectable:
 *   immunocompromised patients who cannot respond to vaccines, and hospital
 *   systems whose capacity collapses under surge. Enforcement runs through
 *   employment termination, clinical-placement loss, and service exclusion.
 *   This file instantiates ONE reading of that contested authority: the
 *   public-health-primary reading, which treats the obligation to protect the
 *   vulnerable commons as foundational and prices the coercion it imposes on
 *   refusers as a defensible cost of that duty. The claim and the metrics are
 *   authored independently: the type claim reflects the structure I believe
 *   true (genuine collective-action function bound to asymmetrically borne
 *   burdens under active enforcement), while the metric values describe the
 *   regime as it actually operated across the interval. Sibling readings of
 *   the same kernel are separate constraint files with their own epsilon and
 *   victim sets; nothing here averages across them. KEY AGENTS (by structural
 *   relationship): - immunocompromised_patients: dual-positioned seat
 *   (shielded beneficiary when coverage holds; exposed victim when it fails)
 *   — powerless, trapped - hospital_systems: agenda-setting beneficiary —
 *   imposes staff requirements, collects roster stability and surge relief -
 *   vaccinated_general_public: low-cost mass beneficiary — complies readily,
 *   receives community protection - mandate_resistant_workers: primary target
 *   seat — identity-locked refusal, bears termination and exclusion -
 *   public_health_agencies: institutional agenda-setter — drafts and defends
 *   rules, absorbs backlash - denied_exemption_patients: excluded voice —
 *   contraindicated class penalized as refusers -
 *   courts_constitutional_reviewers: analytical observer — adjudicates which
 *   mandates survive
 *
 * KEY AGENTS:
 *   - immunocompromised_patients: dual-positioned seat (beneficiary when uptake holds, victim when protection fails) — powerless/trapped, bears residual infection risk
 *   - hospital_systems: agenda-setting beneficiary — imposes staff mandates, absorbs surge risk, collects operational stability
 *   - vaccinated_general_public: low-cost beneficiary — complies readily, receives community protection
 *   - mandate_resistant_workers: primary target seat — identity-locked refusal, loses employment and service access
 *   - public_health_agencies: institutional agenda-setter — issues and defends mandate rules, absorbs political backlash
 *   - denied_exemption_patients: excluded voice — contraindicated patients penalized in the refuser tier
 *   - courts_constitutional_reviewers: observer seat — adjudicates mandate legality and redraws enforcement scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.62).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.68).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public-Health-Primary Reading of the Vaccination Mandate Regime").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a').
narrative_ontology:cs_kernel_codification('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', formalized).
narrative_ontology:cs_authority_grounding('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', lineage).
narrative_ontology:cs_interpretation_layer_present('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a').
narrative_ontology:cs_reading_relation('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', foundational, collective_immunity_supersedes_individual_refusal).
narrative_ontology:cs_axiom_status(collective_immunity_supersedes_individual_refusal, holdable).
narrative_ontology:cs_axiom_grounding('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', collective_immunity_supersedes_individual_refusal, instrumental).
narrative_ontology:cs_axiom('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', secondary, vulnerable_commons_protection_duty).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_duty, holdable).
narrative_ontology:cs_axiom_grounding('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', vulnerable_commons_protection_duty, deontological).
narrative_ontology:cs_reference_frame('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', commons_protection_police_power).
narrative_ontology:cs_drift_state('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', post_emergency_narrowing_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ed7e1cb-1e5e-4e8e-b0cf-09f71e430e7a', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, vaccinated_general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot mount adequate vaccine responses or are contraindicated outright; depend on the uptake of everyone around them for protection. When community coverage holds they move through workplaces, clinics, and schools at tolerable risk; when mandates lapse or exemptions hollow out coverage they absorb exposure they did not choose and cannot reliably exit — isolation is their only dependable shield.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, immunocompromised_patients, payer).

% Operate ICUs and staffing rosters sized to a normal census; epidemic surges break both. They imposed staff vaccination requirements ahead of and alongside government rules, citing patient safety. The requirements stabilized rosters, cut recurring testing overhead, and lowered admissions among staff. They spend political capital defending the policy and absorb attrition among departing employees.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_systems, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, hospital_systems, beneficiary).

% Accepted vaccination at low personal cost and continued ordinary life; receive reduced transmission risk from their neighbors' compliance. Bear diffuse costs — program funding, periodic boosters, brief restrictions during waves — and hold the voting weight that sustains or repeals the rules.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, vaccinated_general_public, beneficiary,
    organized, biographical, mobile, national).

% Refuse mandated vaccination on conscience, religious, or medical-skeptic grounds. Lost jobs, clinical placements, or service access when deadlines passed; litigation rarely reinstated them; some relocated to lenient jurisdictions or shifted into informal work. Compliance would cost them the identity and community standing they have built around refusal, so most absorb the penalties rather than take the shot.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    moderate, biographical, identity_locked, national).

% Drafted and defended the rules: workplace vaccine-or-testing orders, facility staff requirements, school entry rules. Defended them in court, watched key instruments struck down, and recalibrated scope to surviving authorities. Their published protective statistics anchor their credibility, and their staff identity is fused with the protective mission.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Hold contraindications or conditions documented too loosely for exemption panels to accept; they then faced the same penalties as voluntary refusers. They are the very class the protective rationale names, yet had no seat in rule drafting and surface in enforcement records mainly as terminations.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, denied_exemption_patients, excluded,
    powerless, biographical, trapped, national).

% Adjudicate the challenges: struck down the broadest federal workplace order, upheld the facility-staff rule, and left employer-level mandates largely to state law. Their rulings determine which mandates survive and on what stated justification.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, courts_constitutional_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns millions of individual vaccination decisions — each privately rational to skip — onto the coverage band at which transmission chains break and the unprotectable stay shielded; additionally standardizes screening, exemption adjudication, and outbreak response across thousands of institutions that would otherwise fragment.
% TRANSFER_FUNCTION: Moves employment security and service access away from individuals who decline vaccination, converting their compliance or exit into transmission-risk reduction distributed across patients, coworkers, and the public; moves roster stability to healthcare institutions and enforcement discretion to agencies.
% ABSENT_VOICES: Denied-exemption patients and disabled service users bear refusal-tier penalties without having refused anything and held no seat in rule drafting; care-home residents' families, dissenting clinicians, and workers in zero-recourse jurisdictions were likewise outside the conversation. Their objections reach policy only through litigation dockets and comment periods, after the rules are fixed.
% DISAPPEARANCE_RATIONALE: Overnight repeal drops coverage below outbreak thresholds in exposed regions, puts immunocompromised patients back at direct risk within weeks, forces hospitals onto surge staffing plans, dumps exemption disputes into ordinary HR process, and unwinds state preemption statutes — the risk distribution that employers, insurers, and households currently plan around would reorganize.
% FOUNDING_PROBLEM: Recurrent respiratory-virus waves hospitalized and killed the immunocompromised disproportionately and repeatedly collapsed ward capacity; voluntary uptake plateaued below the threshold at which the unprotectable stay safe, leaving a gap that only coordinated compulsion appeared able to close.
% FOUNDING_PROBLEM_CORROBORATION: Peer-reviewed epidemiology and ICU-occupancy records from pre-vaccine waves attest the founding vulnerability independently of any mandate beneficiary; historical mortality archives from the pre-immunization era (polio, measles, influenza) attest the baseline hazard. No attesting profession sits wholly outside the arrangement's success conditions — clinicians and public-health researchers prosper with it — so corroboration rests on independent data sources rather than disinterested persons, and that limitation is stated plainly.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the burden concentrates sharply on a refusing minority (job loss, placement loss, service exclusion) while compliance is cheap for the majority — a steeply asymmetric cost profile atop a genuinely productive arrangement. Suppression 0.68 is a raw structural property, unscaled by power or scope: the regime's persistence depended on active machinery — deadline enforcement, exemption-panel gatekeeping, testing-option withdrawal — not on voluntary preference. Theater 0.22: most activity was functional (real doses administered, real coverage achieved), with a growing symbolic layer as enforcement outlived the acute emergency. Accessibility collapse 0.45: exemption channels, testing regimens, remote arrangements, and interstate moves kept alternatives partly alive in general, but for roster-bound employees the option space collapsed nearly completely. Resistance 0.6: sustained litigation, protests, resignation waves, and state-level prohibition statutes met the mandates throughout. Identity-lock dynamics: the resistant seat's exit is identity_locked — refusal is fused with conscience, community, and self-concept such that compliance is experienced as self-betrayal; if that fusion broke, the seat would read as merely constrained and the coercion profile would soften measurably. The measurement series run on one shared grid (t=0,4,8,12,16,20,24): extractiveness climbs as mandates bite, peaks mid-interval, and eases only slightly after judicial narrowing and partial rescission; suppression builds to a peak then decays as enforcement capacity is struck down or retired; theater creeps up and plateaus — the signature of a regime beginning to maintain itself symbolically after its acute justification recedes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seats compute different types from identical facts. From mandate_resistant_workers' position the regime is experienced as uncompensated imposition — penalties attached to identity, exits foreclosed, litigation futile — a strongly extractive profile. From public_health_agencies' and hospital_systems' positions the same structure is duty execution they built and defend, with real protective output. The vaccinated majority experiences a mild bargain: trivial compliance for meaningful spillover protection. The engine computes this divergence from the structural data; the authored claim does not adjudicate which seat's experience is the truth of the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: mandate_resistant_workers are declared victims with identity_locked exit — derivation places them near the full-target end, amplified by national scope making verification and exception-handling harder. Immunocompromised patients are declared BOTH beneficiary (protection received) and victim (exposure when protection fails); their net position hinges on the failure-mode contingency carried in the omegas. Vaccinated_general_public derives near-symmetric-low: trivial costs, diffuse benefit. Hospital_systems and public_health_agencies carry overrides: the derivation would read hospital_systems as near-pure beneficiaries (declared beneficiary, powerful), but they bear implementation costs, attrition, and litigation exposure, so d is corrected upward to 0.28; agencies would fall to a canonical fallback that reads administration as collection, but they expend capital defending the regime and absorb political injury, and their mission-fused staff internalize its failures, so d is corrected to 0.30. Both corrections reflect the same fact: administrators of this regime are partially inside its blast radius, unlike a pure rent-collector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — epidemic collapse of the unprotectable — is contested rather than dead: the acute emergency receded, but the underlying vulnerability persists in attenuated form, so no mandatrophy resolution is declared. The classification work here is boundary-keeping in both directions. Against snare mislabeling: the coordination function is real and verifiable (coverage thresholds demonstrably shielded the contraindicated), so the arrangement cannot be read as extraction wearing a health costume. Against rope whitewashing: the burden asymmetry is steep and the enforcement machinery was load-bearing, so the regime cannot pass as frictionless coordination. The forward risk is piton drift — the theater series plateaus while suppression decays, the classic signature of an apparatus maintained past the threat that justified it; the T17-style accumulation hypothesis would fire if extractiveness resumed climbing during the plateau.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading of the public_health_mandate_authority kernel — what happens to the classification if a sibling reading governs instead?',
    'Cross-file comparison against the sibling stories: classify the same mandate regime under each sibling''s structural declarations and diff the victim sets, epsilon, and computed types.',
    'A categorical bodily-sovereignty reading moves mandate-resistant workers into the primary victim set and drives epsilon toward snare territory; a sliding-scale reading makes victim membership conditional on threat-severity variables rather than fixed, dissolving this story''s stable structure into a parameterized one. Classification is reading-indexed; no reading-neutral verdict exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint instantiates the public_health_primary reading; sibling readings instantiate materially different constraints.').

omega_variable(
    free_rider_framing_validity,
    'Does the free-rider framing of the unvaccinated describe a genuine externality-imposer, or does it relabel structural penalty-bearers as moral wrongdoers — excluding them from the victim set by definition rather than by demonstrated risk?',
    'Compare transmission contribution against penalty incidence: if penalties attach to vaccination status irrespective of actual transmission risk (remote workers, naturally immune, waned-immunity compliers), the framing is definitional; if penalties track demonstrated externality, it is substantive.',
    'If the framing is definitional, mandate-resistant workers belong in the structural victim set and the arrangement grades toward the snare side; if risk-tracking, the mixed structure stands and the exclusion of the unvaccinated from the victim set is earned rather than asserted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_framing_validity, conceptual, 'Whether this reading''s central victim-set construction tracks risk or launders burden into fault.').

omega_variable(
    immunocompromised_failure_conditions,
    'Under what measurable conditions do the mandates fail the immunocompromised they exist to protect — flipping that seat from shielded beneficiary to exposed victim?',
    'Serosurveillance and breakthrough-hospitalization data correlated with mandate coverage levels, waning intervals, variant immune escape, and exemption-gap geography.',
    'If failure is common — coverage insufficient against escaping variants — the immunocompromised become primary victims and the protective justification weakens toward cover-story status; if rare, the dual-position seat stays beneficiary-dominated and the coordination claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_failure_conditions, empirical, 'Contingency of the reading''s central promise: protection of the vulnerable commons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__public_health_primary, theater_ratio, 4, 0.16).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__public_health_primary, theater_ratio, 8, 0.18).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.21).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__public_health_primary, theater_ratio, 16, 0.22).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__public_health_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__public_health_primary, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__public_health_primary, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__public_health_primary, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__public_health_primary, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__public_health_primary, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__public_health_primary, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.77).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__public_health_primary, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the vaccine mandate debate' covers one kernel with three structurally distinct readings, decomposed per the epsilon-invariance principle into three files. This reading (public_health_primary) fixes the victim set around failed protection and casts refusers as externality-imposers; the bodily-autonomy sibling fixes the victim set around coerced bodies and admits no protective offset; the proportionality sibling parameterizes the entire structure on threat-severity variables. Epsilon differs across the family because each reading assesses the same standing arrangement by its own lights. This reading exerts structural pressure on the proportionality sibling (court practice under this reading's regime forced the proportionality analysis into jurisprudence) and stands in categorical contradiction with the bodily-autonomy sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, powerful, 0.28).
constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
