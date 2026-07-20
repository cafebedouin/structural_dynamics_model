% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalized Substance Control with Third-Party Harm Limitation
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the legalization_reading of the
 *   substance_control_legitimacy kernel. Under this reading, the state's
 *   authority to control substances is bounded by a liberal autonomy
 *   principle: competent adults may use substances, and the state may
 *   intervene only to prevent third-party harms such as impaired driving,
 *   secondhand exposure, and workplace impairment. The structural delta from
 *   sibling readings is that adult users exit the victim
 *   setâdecriminalization removes user-directed carceral extractionâand
 *   the primary remaining coercion is directed at unlicensed supply and
 *   harm-causing use patterns. However, the legal market introduces a new
 *   extraction vector: corporate entities capture licensing regimes,
 *   generating oligopoly rents and regressive taxation that re-victimizes
 *   low-income communities and unlicensed operators. The constraint is
 *   therefore a hybrid: it coordinates liberty and third-party protection
 *   while extracting through market enclosure.
 *
 * KEY AGENTS:
 *   - Licensed corporations: Primary beneficiary (powerful/constrained) â capture legal market rents via regulatory barriers.
 *   - State regulatory apparatus: Agenda-setter (institutional/mobile) â administers licensing, taxation, and harm enforcement.
 *   - Competent adult users: Beneficiary (organized/mobile) â gain autonomy, pay regulated prices.
 *   - Unlicensed operators: Primary target (powerless/trapped) â displaced and criminalized to protect licensed market share.
 *   - Low-income communities: Secondary target (powerless/trapped) â priced out and disproportionately enforced.
 *   - Public health advocates: Observer (organized/analytical) â assess harm outcomes and corporate capture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.65).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.6).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalized Substance Control with Third-Party Harm Limitation").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '4f42c7f1-1af3-421d-98d6-f0d9725a407e').
narrative_ontology:cs_kernel_codification('4f42c7f1-1af3-421d-98d6-f0d9725a407e', formalized).
narrative_ontology:cs_authority_grounding('4f42c7f1-1af3-421d-98d6-f0d9725a407e', lineage).
narrative_ontology:cs_interpretation_layer_present('4f42c7f1-1af3-421d-98d6-f0d9725a407e').
narrative_ontology:cs_reading_relation('4f42c7f1-1af3-421d-98d6-f0d9725a407e', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('4f42c7f1-1af3-421d-98d6-f0d9725a407e', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('4f42c7f1-1af3-421d-98d6-f0d9725a407e', foundational, adult_autonomy_over_bodily_substance_use).
narrative_ontology:cs_axiom_status(adult_autonomy_over_bodily_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('4f42c7f1-1af3-421d-98d6-f0d9725a407e', adult_autonomy_over_bodily_substance_use, deontological).
narrative_ontology:cs_axiom('4f42c7f1-1af3-421d-98d6-f0d9725a407e', foundational, state_authority_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('4f42c7f1-1af3-421d-98d6-f0d9725a407e', state_authority_limited_to_third_party_harm, deontological).
narrative_ontology:cs_reference_frame('4f42c7f1-1af3-421d-98d6-f0d9725a407e', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('4f42c7f1-1af3-421d-98d6-f0d9725a407e', mature_legal_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f42c7f1-1af3-421d-98d6-f0d9725a407e', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, unlicensed_operators).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, low_income_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate legally within state-issued licenses to produce and sell substances; benefit from regulatory caps on market entry that suppress competition; capture monopoly rents and lobby for continued enforcement against unlicensed sellers to protect market share and pricing power.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_corporations, beneficiary,
    powerful, biographical, constrained, national).

% Drafts and enforces licensing rules, excise tax schedules, and third-party harm statutes; collects tax revenue; sets the boundary between permitted personal autonomy and prohibited public harm; adjusts enforcement thresholds in response to corporate lobbying and fiscal demands.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Purchase and use substances legally under autonomy protections; choose from licensed retail options; pay taxes and regulated prices; remain subject to fines and criminal charges for impaired driving, public consumption, and unlicensed sharing.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    organized, biographical, mobile, national).

% Produce or sell outside the licensed system; excluded from legalization by high fees, zoning restrictions, and license caps; face criminal penalties and asset forfeiture that persist even after user decriminalization; primary enforcement target under the new regime.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, unlicensed_operators, payer,
    powerless, immediate, trapped, local).

% Experience continued police contact for public-use and unlicensed-possession violations; often priced out of the legal market by combined taxation and corporate pricing; bear the costs of market transition and enforcement displacement without equal access to licensed channels.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, low_income_communities, payer,
    powerless, biographical, trapped, local).

% Monitor population-level harm indicators such as overdose rates, DUI incidence, and youth access; criticize corporate marketing and regulatory capture; advocate for price controls or public ownership models that the current licensed-market framework excludes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_corporations).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces total prohibition with a bounded liberty regime: competent adults gain legal autonomy over substance use, black-market violence is reduced, and state resources are redirected from punishing possession toward preventing impaired driving, workplace exposure, and secondhand effects.
% TRANSFER_FUNCTION: Moves monopoly rents and excise taxes from consumers and unlicensed producers to licensed corporations and the state treasury; moves carceral risk from all users to unlicensed suppliers and specific harm-causing use patterns.
% ABSENT_VOICES: Advocates of non-commercial decriminalization who would abolish licensing and corporate sale entirely are structurally excluded from policy tables; prohibitionists who reject any legal market lack a seat in the post-legalization apparatus; unlicensed operators are criminalized and lack political voice.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, licensed corporations would lose their legal monopoly and state-enforced market enclosure; the unlicensed market would immediately expand; state excise tax revenue would collapse; user autonomy would revert to contested legal territory; and enforcement priorities would lose their current harm-prevention framing.
% FOUNDING_PROBLEM: Total prohibition criminalized all users, generated violent black markets, diverted enforcement resources from genuine harm prevention, and produced mass incarceration without reducing substance availability.
% FOUNDING_PROBLEM_CORROBORATION: Libertarian constitutional scholars and criminal justice reform advocates attest to prohibition's failure from outside the corporate and state-tax beneficiary sets; public health economists corroborate the shift from carceral to harm-prevention metrics from outside the licensing bureaucracy.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the mature legal market's corporate enclosure: licensing scarcity, vertical integration, and tax levels that exceed harm-prevention costs. Suppression (0.60) remains significant because the regime depends on actively suppressing unlicensed supply to protect licensed market share and tax revenue. Theater ratio (0.45) captures the growing gap between public health and autonomy rhetoric and the revenue-maximizing, oligopoly-protecting practice. Accessibility collapse (0.50) is moderate: the black market persists as an alternative but is actively suppressed; home production or non-market sharing is often criminalized or heavily restricted. Resistance (0.45) comes from prohibitionist holdouts, unlicensed operators, and public health advocates who view the market capture as a betrayal of harm-reduction goals. Measurements share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The licensed corporate seat and the state regulatory seat experience the constraint as coordination that generates legitimate revenue and order. The unlicensed operator and low-income community seats experience the same structure as extraction maintained by active enforcement. The competent adult user seat sits near symmetric: autonomy gains are real, but so are inflated prices and continued criminal liability for behavior patterns such as public use and unlicensed sharing that wealthier users can avoid.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed corporations and competent adult users are declared beneficiaries: corporations capture legal rents via regulatory barriers, and users capture autonomy from criminalization. Unlicensed operators and low-income communities are declared victims: they bear the costs of enforcement displacement and market enclosure. The state regulatory apparatus is agenda_setter; its tax revenue is treated as administrative flow rather than rent capture. Directionality derived from these roles places corporations and users toward the beneficiary end and unlicensed operators and low-income communities toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprohibition's catastrophic failure and mass incarcerationâis substantially solved for users, but the arrangement persists beyond that solution by generating tax revenue and corporate profits. The R5 genealogy (founding_problem_status: contested) signals that the constraint's persistence may now serve extraction rather than the original coordination function. Mandatrophy is not fully resolved because the harm-prevention function remains live, but the market enclosure layer behaves as a zombie element: it would not survive without active enforcement against unlicensed competition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corporate_capture_vs_autonomy,
    'Does the legal market structure genuinely serve adult autonomy, or has it been captured by corporate interests to create a state-enforced oligopoly?',
    'Comparative analysis of market concentration and pricing in legalized versus decriminalized jurisdictions; regulatory capture indicators such as lobbying expenditure and revolving-door appointments.',
    'If capture is dominant, the constraint is more extractive than coordinative for users; if autonomy is preserved, the extraction is a side effect rather than the primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_vs_autonomy, empirical, 'Whether legal market extraction is structural capture or coordination cost').

omega_variable(
    third_party_harm_pretext,
    'Is third-party harm enforcement applied proportionally to actual risk, or used as a pretext to maintain state revenue and corporate market share?',
    'Disparate enforcement data comparing DUI and public-use arrest rates across income levels and jurisdictions; correlation between enforcement intensity and tax revenue shortfalls or corporate lobbying cycles.',
    'If used as pretext, suppression is higher than structural harm prevention requires and the constraint slides toward snare; if proportional, enforcement is a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_pretext, empirical, 'Whether harm prevention is genuine or pretextual enforcement').

omega_variable(
    practice_drift_magnitude,
    'How far have existing legal-market regimes drifted from the liberal autonomy reference frame toward regulatory enclosure?',
    'Policy comparison against the axioms: licensing density, tax rates relative to harm-prevention spending, and barriers to home production or non-market sharing.',
    'Substantial drift would indicate the reading has been hollowed out by practice, leaving a piton or tangled rope where a scaffold or rope was intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_magnitude, conceptual, 'Gap between legalization theory and regulatory practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_legitimacy kernel, decomposed per the Îµ-invariance principle because the three readings (legalization, prohibition, harm reduction) have incompatible beneficiary and victim structures, enforcement targets, and extraction profiles. This reading focuses on adult autonomy and third-party harm limitation; the prohibition reading focuses on total criminalization; the harm reduction reading focuses on public health minimization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
