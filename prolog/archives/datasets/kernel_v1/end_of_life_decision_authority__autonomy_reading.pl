% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: End-of-Life Decision Authority — Autonomy Reading
 *   domain: medical_ethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'end-of-life decision authority.' The autonomy reading asserts that
 *   competent individuals possess sovereign authority to determine when and
 *   how their own death occurs. Under this reading, denial of access to
 *   chosen death constitutes extraction: the constraint appropriates
 *   decision-making power over the most intimate and final choice an
 *   individual can make. The constraint exhibits genuine coordination
 *   (enabling end-of-life planning, reducing prolonged suffering, honoring
 *   patient dignity) layered with asymmetric enforcement: healthcare
 *   institutions and legal regimes that historically opposed autonomy-reading
 *   frameworks are required to facilitate choices they philosophically
 *   resisted. This hybrid structure — real coordination function + active
 *   enforcement against institutional resistance — defines tangled rope at
 *   the analytical level. Measurement trajectory shows rising extractiveness
 *   (0.32 → 0.58) as medical consensus shifts toward autonomy recognition but
 *   legal restriction persists, creating increasing gap between clinical
 *   practice and formal prohibition. Suppression requirement remains high
 *   (0.68–0.72) because legal prohibition, institutional liability exposure,
 *   and professional sanctions remain in force despite eroding legitimacy.
 *   Theater ratio (0.25 → 0.35) reflects that the autonomy reading demands
 *   functional facilitation rather than performative compliance — the
 *   constraint's enforcement requires actual clinical action (providing
 *   information, enabling access, managing medical aspects of chosen death),
 *   not merely institutional theater.
 *
 * KEY AGENTS:
 *   - Competent Individuals Seeking End-of-Life Choice: Primary beneficiary (moderate/mobile) — autonomy reading coordinates their end-of-life planning and dignity; also victims of suppression when access denied
 *   - Patients Experiencing Prolonged Suffering: Primary victim (powerless/trapped) — denied chosen death, bear extraction of autonomous decision-making; no exit from suffering except involuntary
 *   - Healthcare Providers (Conscientious Facilitators): Secondary actor (moderate/constrained) — facilitate autonomy reading; bear costs of legal/professional liability and institutional resistance; benefit from coordinating patient wishes and professional integrity
 *   - Autonomy Rights Coalition (Medical Organizations, Patient Advocates): Organized beneficiary (institutional/arbitrage) — advocate for framework establishing sovereign choice; benefit from expanded authority and professional autonomy; arbitrage available via jurisdiction selection
 *   - Restrictive Medical Establishment: Institutional resistance (institutional/constrained) — maintain restrictive legal/professional authority; see autonomy reading as institutional threat; bear costs of facilitating choices they oppose; suppression is their enforcement mechanism
 *   - Legal Prohibition Regime: Institutional constraint mechanism (institutional/mobile at regime level) — enforces suppression through criminal law, professional sanctions, liability exposure; maintains restrictive authority through legal coercion
 *   - Vulnerable Populations (Elderly, Disabled, Economically Marginalized): Potential secondary victims (powerless/trapped) — slippery slope risk: if autonomy reading realized without adequate safeguards, could face coercion to choose death; slippery slope externalized to this group
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.68).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "End-of-Life Decision Authority — Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '87d99e33-8abb-4bfb-ae18-02701a56efbd').
narrative_ontology:cs_kernel_codification('87d99e33-8abb-4bfb-ae18-02701a56efbd', formalized).
narrative_ontology:cs_authority_grounding('87d99e33-8abb-4bfb-ae18-02701a56efbd', extraction).
narrative_ontology:cs_interpretation_layer_present('87d99e33-8abb-4bfb-ae18-02701a56efbd').
narrative_ontology:cs_reading_relation('87d99e33-8abb-4bfb-ae18-02701a56efbd', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('87d99e33-8abb-4bfb-ae18-02701a56efbd', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('87d99e33-8abb-4bfb-ae18-02701a56efbd', foundational, individual_sovereign_authority_over_death).
narrative_ontology:cs_axiom_status(individual_sovereign_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('87d99e33-8abb-4bfb-ae18-02701a56efbd', individual_sovereign_authority_over_death, deontological).
narrative_ontology:cs_axiom('87d99e33-8abb-4bfb-ae18-02701a56efbd', secondary, prolonged_suffering_constitutes_harm).
narrative_ontology:cs_axiom_status(prolonged_suffering_constitutes_harm, holdable).
narrative_ontology:cs_axiom_grounding('87d99e33-8abb-4bfb-ae18-02701a56efbd', prolonged_suffering_constitutes_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('87d99e33-8abb-4bfb-ae18-02701a56efbd', individual_self_determination_framework).
narrative_ontology:cs_drift_state('87d99e33-8abb-4bfb-ae18-02701a56efbd', contemporary_medical_consensus_shift, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('87d99e33-8abb-4bfb-ae18-02701a56efbd', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_exercising_autonomy).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_providers_facilitating_end_of_life_care).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_to_chosen_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, patients_experiencing_prolonged_suffering).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Competent individual subject to restrictive end-of-life laws perceives pure extraction: authority over own death is systematically denied, suffering is prolonged, no meaningful exit from the constraint exists. High suppression (legal prohibition, medical gatekeeping) prevents all alternatives. Victim status is total — the constraint extracts bodily autonomy without coordination benefit.
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Healthcare provider navigates mixed coordination and extraction: the autonomy reading coordinates genuine end-of-life care and patient dignity (facilitation function), but is constrained by legal prohibition, institutional liability exposure, and professional sanctions. Benefits from coordinating patient wishes; bears costs of legal/professional risk. Exit is costly (relocation, career change) but possible.
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Organized advocates (medical organizations, patient rights groups, ethicists supporting autonomy reading) perceive coordination: establishing sovereign authority over death coordinates genuine end-of-life planning, reduces prolonged suffering, enables dignity-preserving choices. Net beneficiary — the constraint, from their perspective, solves a coordination problem with minimal coercive overhead. Arbitrage exit (can shift to jurisdictions permitting assisted death) available at organization level if not individual level.
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Institutional actors committed to restrictive end-of-life norms (medical boards, hospital ethics committees, liability-averse administrators) experience the autonomy reading as a threat to institutional authority rather than a constraint. From their position, they perceive their restrictive authority as a natural law: medicine's core commitment is to preserve life; permitting death contradicts medicine's foundational purpose. This perspective risks false summit classification — naturalizing a contingent institutional choice (life-preservation-at-all-costs) as immutable principle.
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% Prohibition on assisted death persists through legal/institutional inertia despite eroding legitimacy (medical consensus shifting, public support rising, international jurisdictions permitting choice). Theater ratio elevated: formal procedures (terminal sedation, withdrawal of care) perform prohibition's intent while circumventing its letter. The regime sees its own restrictions as increasingly theatrical — actual clinical practice diverges from formal rules, creating de facto workarounds. Maintenance through theater rather than functional necessity (piton signature).
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From civilizational/analytical perspective, the autonomy reading instantiates a genuine commitment: individual sovereignty over existential choice is coordinated (enables end-of-life planning, reduces suffering) AND requires active enforcement against institutional/sanctity alternatives (medical gatekeeping, religious objection). The classification reflects both genuine coordination and asymmetric enforcement — institutions bear costs of facilitating choice they historically opposed. This perspective shows why false mountains (restrictive framing as natural law) misclassify the actual constraint structure.
constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(end_of_life_decision_authority__autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The autonomy reading appropriates decision-making authority over death from individuals and lodges it in institutional/legal frameworks that historically opposed individual choice. This is genuine extraction of sovereign authority. However, the extraction is not total (snare-level) because the autonomy reading's core function IS coordination — enabling end-of-life planning and reducing suffering. The moderate-high value reflects that extraction is real (authority appropriation) but layered with functional coordination. Rising trajectory (0.32 → 0.58) models the increasing gap between medical consensus shifting toward autonomy recognition and legal restriction persisting — each year of delayed implementation increases extraction as more individuals face denial of access. Suppression (0.68): High. Legal prohibition, professional sanctions against facilitators, institutional liability exposure, and cultural/religious resistance create multiple layers of suppression. However, suppression is not at snare maximum (0.85+) because jurisdictions do exist where autonomy reading is legally implemented, creating arbitrage options and demonstrating suppression is contingent, not immutable. Declining trajectory (0.72 → 0.68) reflects gradual norm shifts and institutional pressure eroding the suppression regime's legitimacy, though formal mechanisms remain in place. Theater Ratio (0.35): Low-moderate. The autonomy reading demands functional clinical action (genuine facilitation, information provision, medical management) rather than performative compliance. This is why theater_ratio is lower than the restrictive legal regime's piton perspective (which shows theater_ratio 0.50+) — the autonomy reading requires real work, not ritual. The low ratio also distinguishes this tangled_rope from pure snare (which could have higher theater in the form of performative justifications for denial).
 *
 * PERSPECTIVAL GAP:
 *   The autonomy reading produces profound perspectival divergence. Competent individuals denied choice perceive snare (pure extraction without coordination benefit — their authority is simply appropriated). Healthcare providers navigating mixed compliance/conscience perceive tangled_rope (genuine coordination of patient dignity, mixed with enforcement costs). Autonomy advocates perceive rope (solving a coordination problem — matching patient wishes with professional facilitation). The restrictive medical establishment perceives its own authority as mountain (life preservation as natural law) — a false summit that naturalizes contingent institutional commitment. The analytical observer perceives tangled_rope with false-summit risk in the institutional perspective — the establishment's 'natural law' framing masks active enforcement of a particular normative reading. This gap shows exactly how the same constraint structure yields different types based on the observer's power, exit options, and structural relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from its structural position. Competent individuals denied choice: powerless/trapped status + victim in denied-access sense produces d ≈ 0.95 (full target), yielding f(d) ≈ 1.42 (maximum experienced extraction). Healthcare providers: moderate power, constrained exit (career risk if they facilitate; conscience burden if they refuse) + secondary victim status (required to enforce restrictions they oppose) produces d ≈ 0.60, yielding f(d) ≈ 0.80 (moderate extraction). Autonomy coalition: institutional power, arbitrage exit (jurisdiction selection, advocacy scale), beneficiary status produces d ≈ 0.15, yielding f(d) ≈ -0.01 (near-zero or slightly negative effective extraction — they benefit). Restrictive establishment: institutional power, arbitrage exit (can maintain restrictions in sympathetic jurisdictions), beneficiary of status quo produces d ≈ 0.10, yielding f(d) ≈ -0.10 (institutional subsidy — the constraint protects their authority). The landscape shows extraction flowing FROM powerless individuals TO institutional beneficiaries, mediated by constrained intermediate actors (providers), with institutional resistance (establishment) subsidized by the restriction regime. This is the signature directionality of tangled_rope: coordination (patient dignity) + extraction (authority appropriation) + enforcement (against institutional resistance).
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading resolves mandatrophy by showing that tangled_rope classification is analytically distinct from both pure rope (coordination-only, which would deny the real extraction of individual authority) and pure snare (extraction-only, which would deny the genuine function of enabling patient dignity and end-of-life planning). The constraint genuinely coordinates end-of-life care and reduces suffering (rope function) AND genuinely extracts decision-making authority from individuals (snare function). Both are real and structural. The constraint persists because institutional resistance makes it necessary to actively enforce facilitation norms against the restrictive status quo — this active enforcement is what prevents the coordination from being 'pure rope.' The mandatrophy is resolved by showing that the mixed classification is not ambiguous — it accurately reflects a hybrid mechanism where coordination and extraction are genuinely both present and structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_empirical_realization,
    'Does permitting competent individual choice for end-of-life decisions empirically lead to systematic coercion of vulnerable populations, or is the slippery slope risk overstated?',
    'Longitudinal study of jurisdictions with permissive autonomy-reading frameworks (Netherlands, Belgium, Canada, Oregon) tracking demographic patterns of assisted death requests and approvals; comparison of vulnerable population access vs restriction rates; analysis of safeguard effectiveness and erosion patterns over 15+ year periods',
    'If slope realizes: the autonomy reading creates actual victims (vulnerable populations subject to coercion) — reclassifies from tangled_rope to snare for vulnerable subpopulations. If slope does not realize: empirical data supports the reading''s classification; protective safeguards are functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_realization, empirical, 'Whether permissive frameworks empirically induce coercion of vulnerable populations').

omega_variable(
    sanctity_axiom_foreclosure,
    'Does the autonomy axiom (individuals possess sovereignty over death) logically foreclose the sanctity axiom (life possesses intrinsic value independent of will), or can both be held within a single normative framework?',
    'Philosophical analysis: can one affirm both individual sovereignty AND intrinsic life value? Empirical check: do jurisdictions with permissive autonomy frameworks (Netherlands, Canada) legally recognize life''s intrinsic value in other contexts (homicide law, protection of unconscious patients)? If yes, then coexistence is demonstrated. If no, then foreclosure is real.',
    'If coexistence: the sanctity and autonomy readings occupy different parties'' frameworks — the kernel exhibits genuine plural reading (coexists_with relation). If foreclosure: the readings are logically incompatible — the autonomy reading rules out sanctity within any coherent framework, or vice versa.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_axiom_foreclosure, conceptual, 'Whether autonomy and sanctity axioms are logically foreclosed or coexistent').

omega_variable(
    institutional_accommodation_trajectory,
    'As medical consensus shifts toward autonomy reading, do institutions (hospitals, medical boards, ethics committees) adapt by incorporating facilitation norms, or do they maintain restrictive theater while practice diverges?',
    'Ethnographic and policy analysis: track institutional responses in restrictive jurisdictions (US states, UK private clinics, Vatican healthcare systems). Do formal policies shift to accommodation, or remain restrictive while actual practice (terminal sedation, aggressive withdrawal of care) performs the autonomy reading informally? Measure institutional theater ratio over 10-year intervals.',
    'If institutions adapt: theater_ratio decreases, piton classification weakens, constraint evolves toward rope or scaffold. If institutions maintain theater: piton classification confirmed; restriction persists as performative maintenance of decrepit authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_accommodation_trajectory, empirical, 'Whether institutions accommodate autonomy reading through policy reform or maintain restrictive theater').

omega_variable(
    kernel_contestation_reading_choice,
    'This constraint is one reading (autonomy_reading) of the contested kernel end_of_life_decision_authority. The sibling readings (sanctity_reading, vulnerability_protection_reading) instantiate structurally different constraints with different epsilon values and victim sets. Is this kernel genuinely three distinct constraints, or a single constraint under-specified by colloquial language (''end of life decision authority'' as label)?',
    'Decomposition test: if measured via different empirical observables or normative lenses, do the readings produce substantially different epsilon and victim sets? Autonomy reading places prolonged suffering in victim set; sanctity reading places coerced death in victim set; vulnerability protection reading places both denial AND coercion in victim set. If epsilon values differ by >0.15 when measuring the same phenomenon via different readings, the ε-invariance principle requires decomposition into separate constraint stories (which is already done — three files, three constraint IDs).',
    'Diagnostic only. Confirms that colloquial label ''end of life decision authority'' masks three structurally distinct constraints, each with its own epsilon, classification, beneficiary/victim structure, and authorization logic. This omega documents the labeling problem, not a resolvable empirical uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_reading_choice, conceptual, 'Kernel decomposition: is end_of_life_decision_authority one constraint or three?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_auto_tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eol_auto_tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(eol_auto_tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(eol_auto_be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eol_auto_be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eol_auto_be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eol_auto_su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(eol_auto_su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(eol_auto_su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_decision_authority kernel decomposes into three constraint stories, each instantiating one reading of the contested principle. The autonomy_reading (this file) asserts individual sovereignty and places prolonged suffering in the victim set. The sanctity_reading places coerced death in the victim set and grounds legitimacy in intrinsic life value. The vulnerability_protection_reading places both denial and coercion in the victim set and distributes authority across institutional checkpoints. These are not alternative framings of one constraint — they are three structurally distinct constraints with different epsilon values, victim sets, enforcement mechanisms, and falsifiability conditions. Network edges show mutual influence: the autonomy reading challenges the sanctity reading's authority (forecloses via axiom conflict); the vulnerability reading responds to both by proposing institutional mediation. All three stories should be linked via network.affects_constraints for complete analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
