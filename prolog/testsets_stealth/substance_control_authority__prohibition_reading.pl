% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition_reading of the
 *   substance_control_authority kernel: the state's authority to criminalize
 *   drug use and possession, justified as protection of third parties from
 *   drug-related crime and social disorder. The arrangement coordinates a
 *   real collective-action problem (residents facing open markets and
 *   disorder) while extracting through the same structure from users and
 *   disproportionately from racially patrolled communities, sustained by
 *   continuous enforcement. Per the committer frame, only this reading is
 *   classified here; the harm_reduction_reading and legalization_reading are
 *   separate constraints with their own victim sets and epsilon values,
 *   linked via network.affects_constraints. The epsilon referent is the
 *   standing criminalization arrangement itself, assessed by this reading's
 *   own lights — which credits the protective function and treats user
 *   punishment as a justified price, yielding a lower epsilon than a
 *   harm-reduction or legalization reading would author over the identical
 *   referent. KEY AGENTS (by structural relationship): -
 *   police_and_prosecutorial_offices: Agenda setter and primary receipt seat
 *   (institutional/arbitrage) — writes enforcement priorities, collects
 *   budgets and forfeited assets - third_party_residents: Declared
 *   beneficiary (organized/constrained) — receives the deterrence promise
 *   while absorbing patrol saturation locally - convicted_drug_users: Primary
 *   target (powerless/trapped) — bears arrest, incarceration, fines, and
 *   lifetime record consequences - racially_disparate_patrolled_communities:
 *   Secondary target (moderate/constrained) — pays enforcement costs
 *   disconnected from usage rates while also suffering market disorder -
 *   correctional_enforcement_contractors: Pure beneficiary (powerful/mobile)
 *   — sells prison, supervision, testing, and forfeiture capacity priced to
 *   enforcement volume - harm_reduction_advocates: Excluded voice
 *   (organized/constrained) — would redesign the arrangement around health
 *   interventions - addiction_medicine_bodies: Analytical observer
 *   (institutional/analytical) — publishes the evidence base both sides cite,
 *   holds no enforcement lever
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.68).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.8).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '843df7a0-ae42-4533-bd70-66011b3ff4c9').
narrative_ontology:cs_kernel_codification('843df7a0-ae42-4533-bd70-66011b3ff4c9', formalized).
narrative_ontology:cs_authority_grounding('843df7a0-ae42-4533-bd70-66011b3ff4c9', lineage).
narrative_ontology:cs_interpretation_layer_present('843df7a0-ae42-4533-bd70-66011b3ff4c9').
narrative_ontology:cs_reading_relation('843df7a0-ae42-4533-bd70-66011b3ff4c9', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_reading_relation('843df7a0-ae42-4533-bd70-66011b3ff4c9', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('843df7a0-ae42-4533-bd70-66011b3ff4c9', foundational, criminal_deterrence_necessary_for_third_party_protection).
narrative_ontology:cs_axiom_status(criminal_deterrence_necessary_for_third_party_protection, holdable).
narrative_ontology:cs_axiom_grounding('843df7a0-ae42-4533-bd70-66011b3ff4c9', criminal_deterrence_necessary_for_third_party_protection, empirically_contingent).
narrative_ontology:cs_axiom('843df7a0-ae42-4533-bd70-66011b3ff4c9', secondary, user_punishment_legitimate_price_of_public_order).
narrative_ontology:cs_axiom_status(user_punishment_legitimate_price_of_public_order, holdable).
narrative_ontology:cs_axiom_grounding('843df7a0-ae42-4533-bd70-66011b3ff4c9', user_punishment_legitimate_price_of_public_order, instrumental).
narrative_ontology:cs_reference_frame('843df7a0-ae42-4533-bd70-66011b3ff4c9', protective_deterrence_statutory_framework).
narrative_ontology:cs_drift_state('843df7a0-ae42-4533-bd70-66011b3ff4c9', contemporary_overdose_and_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('843df7a0-ae42-4533-bd70-66011b3ff4c9', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_residents).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, police_and_prosecutorial_offices).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, correctional_enforcement_contractors).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, convicted_drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racially_disparate_patrolled_communities).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_doctrine).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, punitive_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes enforcement priorities, charges possession cases, and operates asset forfeiture. Budgets, headcount, and career advancement scale with enforcement volume, and conviction statistics are the currency of promotion. The offices could redirect resources toward treatment and diversion but rarely do, because the current allocation feeds the institutions themselves. Exit is ordinary career mobility: transfer, retirement, or movement into private security and compliance work.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, police_and_prosecutorial_offices, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, police_and_prosecutorial_offices, beneficiary).

% Live near open drug markets and want the disorder, property crime, and street dealing stopped. They receive the arrangement's protective promise and generally support enforcement in the abstract, while absorbing its local externalities: saturated patrols, arrested neighbors and family members, and the militarized texture of enforcement in their own blocks. Housing costs and ties make relocation expensive, so their main lever is voice at community meetings, which shapes patrol intensity at the margin.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_residents, beneficiary,
    organized, biographical, constrained, local).

% Use controlled substances and carry the arrangement's direct sanctions: arrest, incarceration, fines and fees, and a permanent record that forecloses employment, housing, benefits, and in many jurisdictions the vote. Dependence makes simple cessation unavailable, and the criminal record itself blocks access to the treatment and stable housing that recovery requires. Coalition power is weak because stigma isolates them and disenfranchisement removes the standard channels of redress.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, convicted_drug_users, payer,
    powerless, immediate, trapped, national).

% Experience arrest and incarceration rates sharply out of proportion to their usage rates, because patrol allocation concentrates enforcement where they live. They pay twice: the market's disorder lands hardest in their neighborhoods, and the enforcement response removes working-age adults, destabilizes families, and brands a generation with records. Moving away is possible for some households at significant cost; collective voice runs through churches, civic associations, and periodic reform candidacies.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racially_disparate_patrolled_communities, payer,
    moderate, generational, constrained, regional).

% Sell the arrangement its capacity: prison beds, probation supervision, drug-testing panels, electronic monitoring, and forfeiture processing. Revenue tracks enforcement volume directly, and the industry lobbies for sentencing stability. Unlike the offices, contractors hold no rule-writing pen; they price what the enforcement demand curve gives them and can pivot contracts to adjacent government demand if enforcement shrinks.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, correctional_enforcement_contractors, beneficiary,
    powerful, biographical, mobile, national).

% Would rebuild the arrangement around health interventions: needle exchange, supervised consumption, treatment-on-demand, decriminalization. They are kept outside formal rule-setting in prohibition-dominant forums, operate services under legal ambiguity, and watch their evidence dismissed at statute-drafting time. Their exclusion is maintained by the same enforcement machinery whose budgets they threaten.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Professional and scientific bodies that publish the comparative evidence on treatment versus deterrence and testify in hearings. Both sides cite their output. They hold no enforcement lever, and their recommendations are routinely overridden by statute and treaty positions they did not write.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, addiction_medicine_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, police_and_prosecutorial_offices).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes the response to open drug markets: by criminalizing use and possession, the state interposes itself between residents and street-level markets, so individuals need not confront dealers, organize private defense, or absorb disorder unaided. Deterrence through sanction is the offered substitute for self-help.
% TRANSFER_FUNCTION: Moves liberty, years of life in custody, money (fines, fees, forfeited assets), and future opportunity (records, disenfranchisement, employment and housing exclusion) from convicted users and heavily patrolled communities to the enforcement apparatus and its contractors; delivers an asserted condition of safety and order to third-party residents.
% ABSENT_VOICES: Current users and formerly incarcerated people are structurally absent from legislative design — disenfranchised, stigmatized, or unheard; harm reduction advocates and affected-family groups sit outside formal rule-setting and would object that the design criminalizes illness and concentrates its costs on racial minorities.
% DISAPPEARANCE_RATIONALE: Overnight repeal would force simultaneous reorganization of policing priorities, prosecutorial caseloads, prison and jail populations, court-mandated treatment pipelines, international treaty compliance postures, and illicit market structure; millions of active sentences, criminal records, agency budgets, and vendor contracts depend on the arrangement continuing as-is.
% FOUNDING_PROBLEM: Early twentieth-century drug panics and, later, the crack-era crisis: visible open markets, property crime driven by untreated addiction, and public disorder that residents demanded government suppress. The answer was to make use and possession crimes so that the state, rather than neighbors or vigilantes, confronted the market.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the enforcement beneficiary set attest the underlying problem persists: municipal council testimony from neighborhoods with open markets, epidemiological literature documenting market violence and disorder, and historical scholarship on the founding panics. Harm reduction advocates — opponents of the chosen remedy — concede the problem is real while disputing the instrument, which is corroboration of the problem independent of the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68: even by this reading's own lights, the arrangement takes liberty, custody time, assets, and lifetime opportunity from users and patrolled communities at substantial scale (mass incarceration, collateral consequences, forfeiture), partially offset by the credited protective function. Suppression is authored at 0.80 as a raw structural property — criminal law backed by arrest and imprisonment, with alternatives (harm reduction services, legalization experiments) actively constrained by statute and international treaty; it is deliberately NOT scaled by power or scope, unlike extractiveness, which the engine scales by directionality and spatial scope. Theater ratio 0.30: enforcement activity is predominantly real (arrests, prosecutions, custody are costly acts, not performances), with a growing symbolic share — announcement-driven sweeps, seizure statistics deployed rhetorically. Accessibility collapse 0.50: alternatives persist and are visibly practiced in pockets (decriminalization jurisdictions, tolerated harm reduction services), so understanding the constraint does not collapse the option space. Resistance 0.60: a sustained reform movement, legalization ballot measures, litigation over sentencing disparities, and professional dissent meet the arrangement continuously. The three temporal series run on one shared grid (t=0..30 at step 6) so every tracked metric is authored at every examined time point; all three rise together, tracing an enforcement ratchet: extraction accumulating, suppression machinery hardening, and a modest theatrical accretion as statistics replace outcomes in public justification.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda_setter seat the arrangement is a functioning protective system it administers — coordination first, costs incidental. From the convicted_drug_users seat the same structure is enforced extraction with no exit. From the third_party_residents seat it is a mixed good: protection received, policing burden borne next door. From the observer seat both halves are visible simultaneously. The engine derives these per-seat classifications from the structural data (roles, power, exit options); the authored claimed_type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. convicted_drug_users derive near-full-target directionality: declared victims, powerless, trapped (dependence plus record effects close exit). racially_disparate_patrolled_communities are declared victims but are NOT full targets — they also suffer the market disorder the arrangement claims to address, a dual exposure the bare victim declaration misses; an override sets d=0.75 for the moderate power atom accordingly. third_party_residents are declared beneficiaries but bear real enforcement externalities (patrol saturation, neighbor arrests), so the near-zero d a clean beneficiary derivation would give is corrected upward to d=0.30 for the organized power atom. police_and_prosecutorial_offices sit near the beneficiary end (they collect budgets and forfeitures and write the rules); correctional_enforcement_contractors are pure beneficiaries with arbitrage-grade mobility, placing them nearest the subsidized end. The overrides exist because the automatic chain reads only declared role plus exit, and two seats here hold genuinely dual positions the declarations alone under-describe.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — third-party exposure to drug-market crime and disorder — is still live, corroborated from outside the beneficiary set, so this is not a mandatrophy case and no resolution is declared. The tangled_rope classification is what prevents the two symmetrical mislabels: reading the arrangement as pure rope would erase the identifiable victims (criminalized users, disparately patrolled communities) whose costs sustain it; reading it as pure snare would deny the genuine coordination function (centralized third-party protection) that gives the arrangement its durable political support among residents. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag — because the problem the arrangement was built for has not gone away; what is contested is the instrument, which is precisely the kernel contest recorded in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'Is this constraint the whole of substance-control authority, or one reading of a shared kernel whose sibling readings (harm_reduction_reading, legalization_reading) instantiate different constraints with different victim sets?',
    'Compile the sibling stories and compare victim sets, epsilon, and enforcement profiles over the same referent arrangement; the decomposition holds if each reading yields a single stable epsilon over the criminalization arrangement.',
    'If the readings decompose cleanly, this classification applies only to the criminalization arrangement; averaging across readings would misattribute the siblings'' lower extraction to this one and blur the victim-set delta that distinguishes them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: this story is one reading of the substance_control_authority kernel, not the kernel entire.').

omega_variable(
    deterrence_efficacy_contest,
    'Does criminal deterrence actually reduce the drug-related crime and disorder third parties experience, or does prohibition generate the black-market violence it claims to prevent?',
    'Natural experiments across jurisdiction pairs differing in enforcement intensity (decriminalization and legalization episodes) with difference-in-differences on market violence, property crime, and disorder indicators.',
    'If deterrence fails, the coordination half of the tangled_rope collapses toward snare — extraction without the protective function; if it succeeds, part of the measured extraction is the price of genuine protection and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_contest, empirical, 'Whether the protective coordination function is real or cover for extraction.').

omega_variable(
    racial_disparity_source,
    'Are racial disparities in application an implementation failure correctable within the prohibition framework, or a structural product of discretionary enforcement design?',
    'Track disparity persistence across decades of within-framework reforms (sentencing-guideline equalization, deprioritization directives, recording-sealing programs).',
    'If structural, the disparity belongs inside the constraint''s epsilon itself; if implementational, the designed constraint''s epsilon is lower than the measured operation and reform claims gain credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_source, empirical, 'Source of racialized application disparities: design or administration.').

omega_variable(
    counterfactual_third_party_outcome,
    'Would third parties be better protected under the sibling readings'' arrangements, and how should liberty costs to users be weighted against order benefits to others?',
    'Longitudinal comparison of third-party harm indicators across regimes combined with explicit normative weighting; no purely empirical resolution exists because the weighting is a value question.',
    'Determines whether this reading''s protective justification survives contact with its alternatives — the practical boundary between tangled_rope and snare for this arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_third_party_outcome, conceptual, 'Counterfactual protection and value weighting across sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohibition_reading_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prohibition_reading_tr_t6, substance_control_authority__prohibition_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(prohibition_reading_tr_t12, substance_control_authority__prohibition_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(prohibition_reading_tr_t18, substance_control_authority__prohibition_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(prohibition_reading_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(prohibition_reading_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(prohibition_reading_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prohibition_reading_be_t6, substance_control_authority__prohibition_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(prohibition_reading_be_t12, substance_control_authority__prohibition_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(prohibition_reading_be_t18, substance_control_authority__prohibition_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(prohibition_reading_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(prohibition_reading_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prohibition_reading_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prohibition_reading_su_t6, substance_control_authority__prohibition_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(prohibition_reading_su_t12, substance_control_authority__prohibition_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(prohibition_reading_su_t18, substance_control_authority__prohibition_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(prohibition_reading_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(prohibition_reading_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'state drug control authority' conflates three structurally distinct claims sharing one kernel. This story (prohibition_reading) authors epsilon for the criminalization arrangement by the prohibition reading's own lights (0.68 — substantial extraction acknowledged as the price of credited protection). The sibling stories author epsilon over the same referent by their own lights: harm_reduction_reading sees criminalization as harm amplification (higher epsilon, different victim emphasis), legalization_reading sees it as commerce suppression (highest epsilon, users removed from its own endorsed arrangement's victim set entirely). The upstream reading in empirical-confidence terms is prohibition (longest statutory lineage); it structurally influences the harm reduction sibling (treaty and enforcement machinery constrain harm-reduction implementation) while coexisting with the legalization sibling as opposed factions of an unresolved dispute. Each member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.3).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
