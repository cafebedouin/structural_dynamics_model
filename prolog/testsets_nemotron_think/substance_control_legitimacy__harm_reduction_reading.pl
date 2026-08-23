% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Drug Policy Framework
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   Harm reduction emerged as a public health response to HIV/AIDS among
 *   injection drug users, positioning substance use as a health issue
 *   warranting medical intervention rather than criminal punishment. Over
 *   three decades it has expanded into a comprehensive policy framework
 *   including syringe services, medication-assisted treatment (MAT), overdose
 *   prevention sites, and decriminalization of possession in some
 *   jurisdictions. The framework coordinates genuine public health gains
 *   (reduced transmission, overdose reversal) while extracting autonomy
 *   through treatment mandates, civil commitment, and probation conditions
 *   that make clinical compliance a condition of liberty. A persistent black
 *   market — sustained by supply-side prohibition that harm reduction does
 *   not challenge — continues to generate violence and adulterated supply,
 *   harming both users and communities. The treatment industry has grown into
 *   a significant financial beneficiary of mandated treatment slots. The
 *   constraint is claimed as rope (pure coordination) by its architects but
 *   operates as tangled rope: coordination function is real but extraction is
 *   structural and enforcement-dependent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.55).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Drug Policy Framework").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'a30d92e5-c8df-4d35-ac35-8345fa16654f').
narrative_ontology:cs_kernel_codification('a30d92e5-c8df-4d35-ac35-8345fa16654f', formalized).
narrative_ontology:cs_authority_grounding('a30d92e5-c8df-4d35-ac35-8345fa16654f', extraction).
narrative_ontology:cs_interpretation_layer_present('a30d92e5-c8df-4d35-ac35-8345fa16654f').
narrative_ontology:cs_reading_relation('a30d92e5-c8df-4d35-ac35-8345fa16654f', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('a30d92e5-c8df-4d35-ac35-8345fa16654f', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('a30d92e5-c8df-4d35-ac35-8345fa16654f', foundational, substance_use_is_health_issue).
narrative_ontology:cs_axiom_status(substance_use_is_health_issue, holdable).
narrative_ontology:cs_axiom_grounding('a30d92e5-c8df-4d35-ac35-8345fa16654f', substance_use_is_health_issue, empirically_contingent).
narrative_ontology:cs_axiom('a30d92e5-c8df-4d35-ac35-8345fa16654f', foundational, state_duty_minimize_harm_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_minimize_harm_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('a30d92e5-c8df-4d35-ac35-8345fa16654f', state_duty_minimize_harm_without_criminalization, instrumental).
narrative_ontology:cs_reference_frame('a30d92e5-c8df-4d35-ac35-8345fa16654f', public_health_harm_reduction_framework).
narrative_ontology:cs_drift_state('a30d92e5-c8df-4d35-ac35-8345fa16654f', contemporary_fentanyl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a30d92e5-c8df-4d35-ac35-8345fa16654f', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_establishment).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, state_health_agencies).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, communities_affected_by_black_market).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, public_health_approach_reduces_overdose_deaths).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, criminalization_increases_harm).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, treatment_on_demand_saves_lives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to treatment mandates, coercive diversion programs, and surveillance. Medicalized rather than criminalized but still lacks full autonomy over substance use. Exit requires either sustained abstinence (which may not be desired or achievable) or navigating a system that treats their identity as pathological. Black market persists, exposing them to adulterated supply and violence.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, identity_locked, national).

% Sets policy frameworks, defines treatment standards, controls funding streams. Benefits from expanded mandate, professional authority, and resource allocation. Can move between government, academia, and NGO sectors. Frames the constraint as evidence-based coordination.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, public_health_establishment, beneficiary).

% Receives public funding (Medicaid, block grants, insurance mandates) for mandated treatment slots. Includes both nonprofit and for-profit providers. Benefits from coercive referral pipelines (drug courts, civil commitment, probation conditions). Market position depends on maintaining treatment-as-mandate rather than treatment-on-demand.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_industry, beneficiary,
    organized, biographical, mobile, national).

% Administers licensing, regulation, and funding for treatment system. Enforces treatment mandates through regulatory authority. Balances public health mandate with political pressure from both prohibition and legalization forces. Institutional survival depends on demonstrating harm reduction outcomes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, state_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear costs of persistent illicit supply networks: violence, property crime, community destabilization. Harm reduction reduces but does not eliminate these harms because supply-side prohibition remains. Cannot exit the geographic exposure; political voice diluted by stigma and structural disinvestment.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, communities_affected_by_black_market, payer,
    moderate, generational, constrained, regional).

% Diverted from supply-side enforcement to 'public safety' roles supporting treatment mandates (warrant checks at clinics, probation compliance). Loses asset forfeiture revenue from low-level possession but gains institutional legitimacy through partnership framing. Resists full decriminalization that would shrink institutional footprint.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, law_enforcement, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, law_enforcement, beneficiary).

% Advocate for abstinence-only recovery; view harm reduction as enabling. Excluded from policy tables where medication-assisted treatment (MAT) is treated as gold standard. Their lived experience of recovery through abstinence is marginalized in evidence hierarchies that prioritize population-level metrics.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, abstinence_recovery_communities, excluded,
    moderate, biographical, mobile, national).

% Argue harm reduction retains state control over bodies and sustains black market. Excluded from implementation tables because their frame threatens the treatment-industry revenue model and public health authority. Would redirect resources from coercive treatment to regulated supply and autonomous user organizations.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose death and disease transmission by connecting people who use drugs to medical services, sterile supplies, and low-threshold care without requiring abstinence as precondition. Solves the collective action problem of public health response to stigmatized behavior.
% TRANSFER_FUNCTION: Moves public funds (Medicaid, SAMHSA block grants, state appropriations) to treatment providers and public health agencies. Moves autonomy from people who use drugs to clinical/state authorities via treatment mandates, civil commitment, and probation conditions. Moves enforcement resources from incarceration to clinical surveillance.
% ABSENT_VOICES: People who use drugs who reject treatment (especially MAT), communities that want regulated supply not just treatment, abstinence-based recovery advocates who see harm reduction as abandonment. They are absent because the policy framework defines 'evidence' in ways that exclude their epistemic claims and because funding structures require institutional intermediaries.
% DISAPPEARANCE_RATIONALE: If harm reduction framework vanished overnight: overdose deaths would spike as low-threshold access to naloxone, syringe services, and MAT evaporated; treatment system would lose its mandate and funding rationale; law enforcement would revert to pure supply-side enforcement; black market would expand unchecked; the public health infrastructure built over 30 years would collapse.
% FOUNDING_PROBLEM: HIV/AIDS crisis among injection drug users in 1980s-90s; overdose epidemic driven by criminalization barriers to care; mass incarceration for possession destroying communities without reducing supply or demand.
% FOUNDING_PROBLEM_CORROBORATION: Public health establishment attests all three problems persist (rising overdoses, ongoing incarceration, treatment gaps). Legalization advocates attest founding problems are misdiagnosed — the root is prohibition itself, not lack of treatment. Abolitionist drug policy researchers (e.g., Drug Policy Alliance, Harm Reduction International) corroborate from outside the treatment-industry beneficiary set that criminalization harms persist under harm reduction because supply-side prohibition remains intact.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects moderate but structural transfer: public funds to treatment industry, autonomy to clinical authority. Not high because users gain real services (naloxone, MAT, sterile supply) and avoid incarceration. Suppression (0.55) reflects active enforcement of treatment mandates, civil commitment, and supply-side prohibition that maintains black market. Theater (0.25) rising: increasing share of enforcement activity defends treatment-industry revenue model (prior authorization, counseling mandates, clinic zoning) rather than direct harm reduction. Accessibility collapse (0.4) moderate: alternatives exist (autonomous user unions, regulated supply models) but are politically suppressed. Resistance (0.5) from both prohibition forces (want more enforcement) and legalization/autonomy forces (want less state control).
 *
 * PERSPECTIVAL GAP:
 *   From agenda_setter seats (public health, state agencies), the constraint is genuine coordination solving overdose and disease. From payer seats (people who use drugs, affected communities), the same structure operates as managed extraction: autonomy traded for services they didn't choose, black market harms persist because supply-side prohibition is untouched. The engine computes this divergence from structural data — the claimed type 'rope' reflects the agenda_setter's authentic framing, while 'tangled_rope' metrics describe the payer's lived reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health establishment and state agencies are agenda_setters with institutional power and arbitrage exit — they set rules and can move between sectors. Treatment industry is beneficiary with organized power and mobile exit — captures funding but could pivot to other healthcare markets. People who use drugs are payers with powerless power and identity_locked exit — their identity as 'patient' is imposed by the system; exiting requires either accepting the patient identity or facing black market harms. Communities affected by black market are payers with moderate power and constrained exit — geographically trapped. Law enforcement is dual payer/beneficiary: loses some revenue but gains institutional legitimacy. Abstinence recovery and legalization advocates are excluded — would object but are structurally kept from policy tables.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (HIV/AIDS, overdose, mass incarceration) is contested as live/dead. Public health says all three persist; legalization advocates say the root cause (prohibition) is unchanged so harm reduction is a band-aid. The constraint persists not because the founding problem is solved but because it created a self-sustaining institutional ecology (treatment industry, public health mandates, law enforcement partnerships) that would lose revenue and authority under either full prohibition or full legalization. This is mandatrophy: the mandate (reduce harm) has outlived its original crisis but the arrangement persists through institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_reduction_vs_autonomy_boundary,
    'Does the harm reduction framework''s acceptance of treatment mandates and civil commitment structurally require the continuation of supply-side prohibition, or could it exist under regulated legal supply?',
    'Natural experiment from jurisdictions implementing both harm reduction and regulated supply (e.g., Canada''s safer supply pilots, Switzerland''s heroin-assisted treatment): if treatment mandates persist without supply prohibition, the two are separable.',
    'If inseparable, harm reduction is structurally coupled to prohibition and cannot be a true alternative — it is a modulation of prohibition. If separable, the measured extraction (mandates) is a policy choice not a structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_vs_autonomy_boundary, conceptual, 'Whether harm reduction''s extraction component depends on prohibition''s persistence').

omega_variable(
    treatment_industry_capture,
    'To what extent does the treatment industry''s financial dependence on mandated treatment slots shape clinical guidelines, zoning regulations, and political lobbying in ways that expand coercion beyond clinical justification?',
    'Comparative analysis of treatment mandate expansion vs. overdose outcomes in states with high vs. low for-profit treatment market share; lobbying expenditure tracking.',
    'If capture is substantial, the constraint''s extraction is beneficiary-driven rather than coordination-driven — moves toward snare classification for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treatment_industry_capture, empirical, 'Whether treatment industry profit motive drives mandate expansion beyond evidence').

omega_variable(
    black_market_harm_attribution,
    'How much of the harm attributed to ''substance use'' in harm reduction metrics is actually attributable to black market conditions (adulteration, violence, unpredictable potency) that persist because harm reduction does not challenge supply prohibition?',
    'Decomposition of overdose and morbidity data by supply-source characteristics in jurisdictions with varying degrees of supply-side enforcement and safer supply access.',
    'If most measured harm is black-market-mediated, harm reduction''s coordination function is partial — it treats symptoms of prohibition while leaving the cause intact. Reframing would shift extraction accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_harm_attribution, empirical, 'Proportion of harm reduction''s target harms that are prohibition-generated').

omega_variable(
    kernel_framing_underdetermination,
    'Does the substance_control_legitimacy kernel admit a fourth reading — abolitionist_decriminalization — that rejects both state medicalization and state criminalization in favor of user-led autonomous organizations and regulated supply?',
    'Genealogical analysis of drug user union movements (VANDU, INPUD, local collectives) and their policy proposals; comparison to harm reduction institutional frameworks.',
    'If a coherent fourth reading exists with distinct beneficiary/victim structure, the three-reading kernel model is incomplete and this story''s structural delta omits a live alternative. Would require new constraint story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s reading space is exhausted by the three declared readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 1988, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1988, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(subs_tr_t1996, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement(subs_tr_t2004, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(subs_tr_t2012, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(subs_tr_t2020, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2020, 0.23).
narrative_ontology:measurement(subs_tr_t2024, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t1988, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1988, 0.2).
narrative_ontology:measurement(subs_be_t1996, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1996, 0.25).
narrative_ontology:measurement(subs_be_t2004, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2004, 0.3).
narrative_ontology:measurement(subs_be_t2012, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(subs_be_t2020, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(subs_be_t2024, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1988, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1988, 0.3).
narrative_ontology:measurement(subs_su_t1996, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1996, 0.35).
narrative_ontology:measurement(subs_su_t2004, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(subs_su_t2012, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2012, 0.48).
narrative_ontology:measurement(subs_su_t2020, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(subs_su_t2024, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, drug_supply_prohibition).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, civil_commitment_statutes).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, medicaid_treatment_mandates).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the substance_control_legitimacy kernel. The prohibition_reading (criminalization) and legalization_reading (autonomy) are sibling constraints with distinct ε, beneficiaries, and victims. All three share the kernel question: what legitimizes state authority over substance use? This reading's ε (0.45) is moderate — higher than prohibition's for users (who avoid prison but lose autonomy) but lower than prohibition's for communities (black market persists). The legalization_reading would have near-zero ε for users but higher for state revenue agencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, organized, 0.35).
constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
