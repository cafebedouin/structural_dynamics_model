% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: State Authority to Criminalize Drug Use/Possession to Protect Third Parties
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the prohibition_reading of the
 *   contested kernel substance_control_authority. The state claims authority
 *   to criminalize drug possession and use on the grounds that this protects
 *   third parties from drug-related crime and social disorder. The structural
 *   reality: criminalization creates a victim set (people who use drugs,
 *   disproportionately Black and Latinx communities, low-income defendants)
 *   who bear incarceration, collateral consequences, and police violence;
 *   beneficiaries include law enforcement agencies (budgets, asset
 *   forfeiture, overtime), the prison-industrial complex (per-bed revenue),
 *   political incumbents (tough-on-crime electoral capital), and mandated
 *   treatment providers (captive referrals). The deterrence rationale is
 *   empirically contested — jurisdictions that decriminalized possession show
 *   no increase in third-party harms. Enforcement costs are high and rising;
 *   racial disparities are structural, not incidental. The constraint
 *   persists through active suppression of alternatives (harm reduction,
 *   regulated markets) and internalized stigma that persists after formal
 *   legal barriers.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: Primary target (powerless/trapped) — bears incarceration, collateral consequences, stigma
 *   - black_and_latinx_communities: Primary target (powerless/identity_locked) — targeted enforcement, generational impact
 *   - law_enforcement_agencies: Primary beneficiary (institutional/arbitrage) — budgets, asset forfeiture, institutional mission
 *   - prison_industrial_complex: Primary beneficiary (institutional/arbitrage) — per-bed revenue, political influence
 *   - political_incumbents_tough_on_crime: Beneficiary (institutional/arbitrage) — electoral capital from enforcement rhetoric
 *   - families_of_incarcerated: Secondary victim (powerless/constrained) — economic loss, relational rupture
 *   - communities_subject_to_policing: Secondary victim (organized/constrained) — over-policing, erosion of trust
 *   - public_health_authorities: Excluded (institutional/analytical) — would implement harm reduction but structurally blocked
 *   - competition_authorities: Observer (institutional/analytical) — not directly applicable but analytical seat for market distortion analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.85).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Authority to Criminalize Drug Use/Possession to Protect Third Parties").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac').
narrative_ontology:cs_kernel_codification('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', formalized).
narrative_ontology:cs_authority_grounding('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', extraction).
narrative_ontology:cs_interpretation_layer_present('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac').
narrative_ontology:cs_reading_relation('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', foundational, criminalization_protects_third_parties).
narrative_ontology:cs_axiom_status(criminalization_protects_third_parties, holdable).
narrative_ontology:cs_axiom_grounding('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', criminalization_protects_third_parties, empirically_contingent).
narrative_ontology:cs_axiom('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', secondary, state_monopoly_on_violence_justifies_prohibition).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_justifies_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', state_monopoly_on_violence_justifies_prohibition, deontological).
narrative_ontology:cs_reference_frame('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', classical_punitive_authority).
narrative_ontology:cs_drift_state('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', contemporary_harm_reduction_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c315c2b-6c3a-4eb1-8e2c-8a858d70eeac', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, political_incumbents_tough_on_crime).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, drug_treatment_mandated_providers).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, black_and_latinx_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_defendants).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, communities_subject_to_policing).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_theory_of_crime_control).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, state_monopoly_on_violence_justifies_prohibition).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, criminalization_protects_third_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalized for possession/use; face arrest, incarceration, collateral consequences (housing, employment, voting, immigration), overdose risk from unregulated supply, stigma. Exit from the constraint requires either ceasing use (often impossible without support the constraint blocks) or moving to a decriminalized jurisdiction (resource-intensive, not universally available). The criminal record follows them.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Targeted for enforcement at rates far exceeding usage rates; generational impact through incarceration of fathers/mothers, wealth extraction via fines/fees, police violence, community destabilization. Identity-locked because racialization fuses the targeting with communal identity — exit from the constraint would require exiting the racialized social position, which is structurally impossible.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, black_and_latinx_communities, payer,
    powerless, generational, identity_locked, national).

% Cannot afford private counsel, bail, or treatment alternatives; coerced into plea bargains; public defense systems overwhelmed. Trapped by resource poverty — the constraint's costs scale inversely with resources.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_defendants, payer,
    powerless, biographical, trapped, national).

% Set enforcement priorities, control asset forfeiture revenue, receive federal grants tied to drug arrests, define 'drug-related crime' statistically. Beneficiaries of budgets, overtime, equipment, mission justification. Arbitrage exit: can pivot to other enforcement priorities (trafficking, guns, cyber) if drug enforcement were defunded.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary).

% Private prisons, phone/commissary vendors, prison labor contractors, construction firms receive per-bed revenue and captive markets. Lobby for mandatory minimums and against decriminalization. Arbitrage exit: can pivot to immigration detention, electronic monitoring, re-entry services if drug incarceration declines.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, prison_industrial_complex, beneficiary,
    institutional, generational, arbitrage, national).

% Extract electoral capital from enforcement rhetoric; receive law enforcement union endorsements and campaign contributions. Benefit from the constraint's symbolic power. Arbitrage exit: can shift to other 'tough on' platforms (border, gangs, retail theft) if drug war rhetoric loses salience.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, political_incumbents_tough_on_crime, beneficiary,
    institutional, biographical, arbitrage, national).

% Receive court-mandated referrals (captive client base); often abstinence-only models that exclude evidence-based medications (MOUD). Benefit from guaranteed census. Mobile exit: can accept insurance/Medicaid patients voluntarily if mandates end.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_treatment_mandated_providers, beneficiary,
    organized, biographical, mobile, regional).

% Bear economic costs (commissary, phone, travel, lost income), relational rupture, child welfare involvement, housing instability. Constrained exit: tied to incarcerated person by kinship and geography; cannot easily relocate or sever ties.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, families_of_incarcerated, payer,
    powerless, biographical, constrained, local).

% Experience over-policing, erosion of trust, stop-and-frisk, pretextual stops, cooperative witness pressure. Organized through community groups but constrained by geography and political economy — cannot exit the jurisdiction easily.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, communities_subject_to_policing, payer,
    organized, generational, constrained, local).

% Would implement harm reduction (syringe service programs, safe consumption sites, MOUD, drug checking) but are structurally blocked by scheduling, paraphernalia laws, and political interference. Their evidence-based alternatives are suppressed by the constraint. Analytical exit: they observe the constraint's failure but cannot enact alternatives within it.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_authorities, excluded,
    institutional, generational, analytical, national).

% Study deterrence effects, racial disparities, collateral consequences, alternative models. Provide evidence that the constraint's claimed function (deterrence) is not empirically supported. Analytical seat: neither collecting nor paying, but their evidence is contested by beneficiaries.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, academic_criminologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate deterrence of drug-related crime and social disorder by criminalizing possession/use — a single rule intended to suppress supply and demand simultaneously.
% TRANSFER_FUNCTION: Moves liberty, labor, life outcomes, and public funds from criminalized populations (disproportionately Black/Latinx/low-income) to law enforcement agencies (budgets, asset forfeiture), prison-industrial complex (per-bed revenue), political incumbents (electoral capital), and mandated treatment providers (captive referrals).
% ABSENT_VOICES: People who use drugs (especially those not in treatment), families of incarcerated, communities subject to policing, public health practitioners implementing harm reduction — structurally excluded from legislative hearings, drug policy commissions, and media narratives that center law enforcement perspectives.
% DISAPPEARANCE_RATIONALE: If criminalization vanished overnight: possession/use would shift to regulated or decriminalized frameworks; law enforcement budgets would lose drug-war earmarks; prisons would depopulate; asset forfeiture revenue would collapse; political rhetoric would shift; public health infrastructure would expand to absorb demand; racial disparities in policing would lose their primary driver. The world would rearrange substantially.
% FOUNDING_PROBLEM: Protect third parties from drug-related crime (property crime, violence) and social disorder (public intoxication, visible markets, overdose deaths in public spaces) by suppressing drug availability through criminal penalties.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (law enforcement, politicians) attest the problem is live and requires continued criminalization. Corroboration from outside the beneficiary set: Portugal (2001 decriminalization) saw overdose deaths and HIV drop without crime increase; Oregon (2020 Measure 110) showed no third-party harm increase; WHO/UNODC now endorse harm reduction over criminalization; National Academies (2019) found no evidence that criminalization reduces use prevalence. The founding problem is at minimum contested; evidence suggests it is substantially addressed by alternatives.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   High extractiveness (0.78) reflects the massive transfer of liberty, labor, and life outcomes from criminalized populations to enforcement and carceral institutions. Suppression (0.85) is extreme: the constraint actively suppresses harm reduction, safe supply, and regulated markets through scheduling, international treaties, and police power. Theater ratio (0.42) is substantial: deterrence rhetoric and 'protecting communities' framing perform a coordination function that the constraint does not empirically deliver. Accessibility collapse (0.72) is high but not total — regulated markets exist for alcohol/tobacco, and decriminalization jurisdictions demonstrate alternatives. Resistance (0.58) is significant: reform movements, ballot initiatives, judicial challenges, and international pressure create real friction but have not displaced the constraint. The claimed type is snare: the coordination story (deterrence) is cover; persistence depends on coercion and suppressing exits.
 *
 * PERSPECTIVAL GAP:
 *   From the law enforcement/institutional seat, the constraint appears as necessary coordination against a genuine threat (drug markets, overdose deaths, property crime) — a rope-like framing. From the criminalized populations' seat, it is pure extraction: liberty taken, labor exploited, families ruptured, with no demonstrated third-party protection. The engine computes this divergence from structural data: agenda_setter/beneficiary roles with institutional power and arbitrage exit vs. payer/victim roles with powerless/identity_locked/trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: law_enforcement_agencies, prison_industrial_complex, political_incumbents_tough_on_crime, drug_treatment_mandated_providers — all collect rents (budgets, revenue, votes, captive clients) and have arbitrage-grade exit (can pivot missions, close facilities, shift rhetoric). Victims: people_who_use_drugs (trapped/identity_locked — criminal record bars exit from stigma), black_and_latinx_communities (identity_locked — racialized targeting fuses identity with criminalization), low_income_defendants (trapped — cannot afford defense/alternatives), families_of_incarcerated (constrained — economic/relational tether), communities_subject_to_policing (constrained — geographic lock-in). Directionality derives from beneficiary/victim declarations + exit options: beneficiaries → d ≈ 0.1; victims → d ≈ 0.85-0.95.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting third parties from drug-related crime and disorder) is contested: beneficiaries claim it is live; public health researchers and affected communities attest it is substantially solved by alternatives (harm reduction, regulation) or was never effectively addressed by criminalization. Corroboration from outside the beneficiary set exists: Portugal's decriminalization (2001) reduced overdose deaths and HIV without increasing crime; Oregon's Measure 110 (2020) showed no third-party harm increase; WHO and UNODC now endorse harm reduction. The constraint persists despite the founding problem being at minimum contested — classic mandatrophy. The mandatrophy_resolved flag is true: the mandate has outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is this constraint one reading of a contested kernel (substance_control_authority) where sibling readings (harm_reduction_reading, legalization_reading) instantiate different constraints with different victim sets and ε values?',
    'Committer frame: the kernel_id and reading_id are declared in the SCOPE manifest. The structural delta between readings is the disagreement locus. This omega records the committer structure rather than inventing a schema field.',
    'If confirmed, each reading gets its own constraint story with its own ε, beneficiaries, victims, and classification — not a single story with measurement-dependent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'This constraint is the prohibition_reading of kernel substance_control_authority; sibling readings are separate constraint stories.').

omega_variable(
    deterrence_empirical_validity,
    'Does criminalization of possession/use actually deter third-party harms at the margin, or is the deterrence claim a cover for extraction?',
    'Natural experiments from jurisdictions that decriminalized possession (Portugal, Oregon, Thailand): compare third-party harm rates pre/post controlling for confounders.',
    'If deterrence fails empirically, the constraint''s coordination function collapses and the snare classification strengthens; if deterrence holds, tangled_rope becomes structurally plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_validity, empirical, 'Whether the claimed protective function (deterrence of third-party harm) is empirically substantiated.').

omega_variable(
    racial_disparity_mechanism,
    'Are racial disparities in enforcement an incidental bias or a structural feature that sustains the constraint''s political economy?',
    'Compare enforcement patterns across jurisdictions with different demographic compositions but similar laws; trace political rhetoric to budget allocations.',
    'If structural, the constraint is a racialized extraction mechanism — identity_locked exit for targeted populations deepens; if incidental, reform could reduce disparity without changing the constraint''s type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_mechanism, empirical, 'Whether racial disparity is structural or incidental to the constraint''s operation.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the suppression experienced by people who use drugs primarily structural (legal barriers, policing) or internalized (stigma, self-exclusion from services, identity fusion with ''criminal'' label)?',
    'Post-decriminalization suppression trajectory: if suppression metrics persist after legal barriers are removed, the internalized component is confirmed.',
    'If internalized suppression is substantial, effective suppression is higher than the structural measure suggests — targets carry the constraint with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism for the primary victim population.').

omega_variable(
    mandatrophy_founding_problem_status,
    'Is the founding problem (protecting third parties from drug-related crime and social disorder) still live, dead, or contested?',
    'Corroborated genealogy: testimony from outside the beneficiary set — public health researchers, affected communities, criminologists not funded by enforcement agencies.',
    'If dead but constraint persists, mandatrophy_resolved = true and piton/snare classification sharpens; if live, the coordination claim retains structural weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_founding_problem_status, conceptual, 'Founding problem status and its corroboration from non-beneficiary sources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t0, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t10, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t20, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t30, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t40, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_tr_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t0, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t10, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t20, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t30, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t40, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_be_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t0, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t10, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t20, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t30, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t40, observed).
narrative_ontology:measurement(substance_control_authority__prohibition_reading_su_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement_basis(substance_control_authority__prohibition_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, mass_incarceration_authority).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, policing_authority_asset_forfeiture).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, international_drug_control_treaties).

% DUAL FORMULATION NOTE:
% Constraint family: substance_control_authority kernel decomposes into three readings — prohibition_reading (this story, snare), harm_reduction_reading (rope/tangled_rope), legalization_reading (rope). The ε values differ substantially: prohibition extracts via incarceration (high ε); harm_reduction coordinates health services (low ε); legalization coordinates markets (low-moderate ε). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, institutional, 0.1).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, powerless, 0.92).
constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
