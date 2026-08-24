% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Prohibition Reading: Substance Use as Moral Transgression Requiring State Punishment
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control frames drug use as inherent
 *   moral transgression requiring state punishment to protect social order.
 *   This reading instantiates the substance_control_kernel by placing users
 *   in a criminal victim set (high extraction), making the enforcement
 *   apparatus the primary beneficiary (budgets, legitimacy, asset
 *   forfeiture), generating black market violence as a secondary externality
 *   borne by communities, and exercising coercive state authority rather than
 *   service provision. The claimed coordination function — social order
 *   through deterrence — operates alongside massive asymmetric extraction.
 *   The engine will compute per-seat classifications from the structural
 *   data; the divergence between the agenda_setter seat (which experiences
 *   genuine coordination) and the payer seats (which experience enforced
 *   extraction) is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.82).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Prohibition Reading: Substance Use as Moral Transgression Requiring State Punishment").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f').
narrative_ontology:cs_kernel_codification('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', formalized).
narrative_ontology:cs_authority_grounding('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', extraction).
narrative_ontology:cs_interpretation_layer_present('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f').
narrative_ontology:cs_reading_relation('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', state_punishment_protects_social_order, deontological).
narrative_ontology:cs_axiom('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', secondary, deterrence_justifies_criminalization).
narrative_ontology:cs_axiom_status(deterrence_justifies_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', deterrence_justifies_criminalization, empirically_contingent).
narrative_ontology:cs_reference_frame('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', prohibition_as_moral_order).
narrative_ontology:cs_drift_state('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', contemporary_overdose_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2caff3e9-882f-4fba-9fdb-7bd1dcd5c46f', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, moral_entrepreneurs).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, asset_forfeiture_recipients).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, communities_impacted_by_black_market_violence).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, families_of_incarcerated).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, harm_reduction_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, public_health_authorities).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, moral_order_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, deterrence_theory).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, supply_side_control_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalized for possession and use; face arrest, incarceration, loss of housing/employment/benefits, and permanent collateral consequences. Exit is identity-locked because criminal record and stigma fuse with self-concept; treatment access is gatekept by the same enforcement apparatus. Black market exposure adds violence and overdose risk.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, biographical, identity_locked, national).

% Police, DEA, prosecutors, courts, and corrections departments that design, implement, and enforce prohibition. Budgets, personnel, asset forfeiture revenue, and institutional legitimacy depend on continued criminalization. They set enforcement priorities, define 'trafficking' thresholds, and control access to diversion programs.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Private prison operators, prison labor contractors, phone/commissary vendors, and correctional officer unions that extract revenue from incarcerated populations. Drug offenses constitute a large share of admissions; sentencing enhancements and mandatory minimums guarantee bed occupancy. Exit is arbitrage-grade — capital redirects to other carceral revenue streams if drug enforcement wanes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, prison_industrial_complex, beneficiary,
    powerful, biographical, arbitrage, national).

% Advocacy groups, religious organizations, and political figures who build status and fundraising on 'protecting society' from drugs. They supply the moral narrative that legitimizes enforcement; their influence depends on prohibition remaining the only thinkable framework. They do not operate enforcement directly but shape the political conditions that sustain it.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_entrepreneurs, beneficiary,
    organized, generational, analytical, national).

% Local police departments and task forces that retain seized cash, vehicles, and property under civil forfeiture laws. Revenue directly funds equipment, overtime, and discretionary budgets with minimal oversight. Exit is mobile — they can shift to other revenue sources but fight to preserve this stream.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, asset_forfeiture_recipients, beneficiary,
    organized, immediate, mobile, local).

% Neighborhoods where prohibition creates illicit markets, turf violence, and police occupation. Residents bear homicide risk, property devaluation, and aggressive policing without political power to change the regime. Exit is constrained — relocation is costly and the regime follows them to new jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, communities_impacted_by_black_market_violence, payer,
    moderate, biographical, constrained, local).

% Children, partners, and parents of those imprisoned for drug offenses. They absorb lost income, caregiving burdens, visitation costs, and intergenerational trauma. No exit from the constraint's effects; the arrangement structurally produces their immiseration.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, families_of_incarcerated, payer,
    powerless, biographical, trapped, local).

% Syringe service programs, overdose prevention sites, medication-assisted treatment providers, and peer support workers. They operate in legal gray zones or outright illegality; their evidence-based interventions are suppressed because they contradict the moral transgression frame. They would object to prohibition's harms but are structurally excluded from policy tables.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% CDC, SAMHSA, state health departments, and WHO. They produce data showing prohibition's health harms and endorse harm reduction, but their funding and authority often flow through the same prohibition framework (e.g., drug-free workplace mandates, grant conditions). They occupy an analytical seat but collect institutional benefits from the status quo.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, public_health_authorities, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains social order by criminalizing substance use, deterring use through punishment, and expressing societal moral condemnation of intoxication as such.
% TRANSFER_FUNCTION: Moves liberty, resources, community stability, and public health outcomes from substance users and affected communities to enforcement apparatus, carceral system, moral entrepreneurs, and asset forfeiture recipients via criminal penalties, incarceration, civil forfeiture, and black market violence externalities.
% ABSENT_VOICES: Substance users themselves (criminalized and silenced), communities devastated by black market violence and mass incarceration, harm reduction practitioners excluded from policy tables, and families of the incarcerated who bear intergenerational costs without political representation.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, black markets would collapse within months as legal supply chains replace them; incarceration rates for drug offenses would plummet; enforcement resources would redirect to violent crime; a regulated market or harm reduction framework would emerge; overdose deaths would decline as supply becomes known-dose; the moral entrepreneurs' political platform would dissolve.
% FOUNDING_PROBLEM: Early 20th century moral panic about substance use threatening social order, racialized fears of minority communities (Chinese opium, Mexican cannabis, Black cocaine), and international treaty commitments (1912 Hague Opium Convention, 1961 Single Convention) that codified prohibition as the only legitimate framework.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Musto, Courtwright, Alexander, Baum) documents the racialized origins, moral entrepreneurship of figures like Anslinger, and the absence of evidence for deterrence efficacy. No independent corroboration exists that the founding problem — moral panic about social order collapse from substance use — persists; the arrangement persists as rent-seeking and institutional inertia.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers liberty, wealth, health, and life from users and communities to enforcement and carceral actors. Suppression (0.88) is very high because alternatives (harm reduction, regulated supply, decriminalization) are actively suppressed through criminal law, treaty obligations, and funding conditions. Theater ratio (0.42) reflects that enforcement rituals (raids, seizures, 'drug-free' declarations) increasingly exceed functional deterrence — the 'war on drugs' rhetoric persists despite evidence of failure. Accessibility collapse (0.78) is high because the legal regime makes harm reduction illegal or precarious in most jurisdictions. Resistance (0.68) is substantial: reform movements, litigation, ballot initiatives, and public opinion shifts have not yet displaced the core arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement seat, prohibition is a necessary coordination mechanism against a genuine threat; from the user and community seats, it is a predatory extraction machine. The engine computes this divergence from the declared roles, power, and exit options — the claimed_type (tangled_rope) reflects the prohibition reading's own framing, while the metrics describe the constraint's actual operation. The gap between claim and measurement is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Enforcement apparatus and prison industrial complex are structural beneficiaries (d near 0.0) — they collect budgets, revenue, and legitimacy from the constraint. Substance users are full targets (d near 1.0) — criminalized, incarcerated, identity-locked. Communities impacted by black market violence are targets (d ~0.8) — they bear violence and policing without political exit. Moral entrepreneurs and asset forfeiture recipients are beneficiaries (d ~0.1-0.2). Harm reduction practitioners are excluded (no directionality — they are not in the constraint's seat set). Public health authorities sit near symmetric (d ~0.5) — they produce counter-evidence but collect institutional benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (moral panic about social order collapse) is dead — the original racialized fears and treaty commitments no longer animate the arrangement. Yet the constraint persists and has intensified (extraction rising from 0.45 to 0.82 over the interval). This is classic mandatrophy: the mandate (protect social order) has atrophied while the apparatus (enforcement, incarceration, forfeiture) has grown. The prohibition reading prevents mislabeling this as pure coordination (it extracts heavily) or pure extraction (it claims a coordination function). The tangled_rope classification captures the hybrid: a coordination story that has become a vehicle for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the prohibition_reading a distinct constraint from the harm_reduction_reading and legalization_reading, or do they share an ε-invariant core that measurement could isolate?',
    'Decompose each reading into separate constraint stories with their own ε, beneficiaries, victims, and metrics. If ε values differ substantially (as expected: prohibition ε_high, harm_reduction ε_low, legalization ε_minimal), they are distinct constraints linked by network.affects_constraints. If a single ε fits all three, the kernel is one constraint with observer-dependent classification.',
    'If distinct, the engine treats them as a constraint family with contamination propagation. If unified, the classification depends on which reading''s metrics dominate — a measurement choice, not a structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings of the substance_control_kernel are structurally distinct constraints or one constraint with three observational frames.').

omega_variable(
    coordination_extraction_boundary,
    'Is the claimed coordination function (social order protection through deterrence) structurally genuine, or is it cover for the extraction function (enforcement budgets, carceral revenue, moral entrepreneurship)?',
    'Counterfactual: if enforcement resources were redirected to harm reduction and regulated supply while maintaining the same level of social order (measured by overdose deaths, property crime, public disorder), would the prohibition reading''s advocates accept the substitution? If no, coordination is cover.',
    'If coordination is genuine, the constraint is a tangled_rope with a real coordination component. If cover, it is a snare with a coordination pretext. The engine''s classification will reflect the metrics; this omega documents the irreducible ambiguity in the prohibition reading''s own self-presentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the prohibition reading''s claimed social order coordination function is structurally real or a pretext for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.88) primarily structural (criminal law, treaty obligations, funding bans) or does it include a substantial internalized component (stigma, self-concept as ''criminal'', moral internalization of the transgression frame)?',
    'Post-decriminalization suppression trajectory: in jurisdictions that decriminalized (Portugal, Oregon), measure whether suppression experienced by users declines to the structural remainder (regulatory barriers only) or persists at high levels via stigma and internalized moral judgment.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression after formal exit. This would increase effective extraction for identity-locked users beyond what the engine computes from structural data alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the prohibition reading''s operation on substance users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__prohibition_reading, theater_ratio, 1990, 0.41).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(subs_tr_t2020, substance_control_kernel__prohibition_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__prohibition_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement(subs_be_t2020, substance_control_kernel__prohibition_reading, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(subs_su_t2020, substance_control_kernel__prohibition_reading, suppression_requirement, 2020, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, mass_incarceration_regime).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, civil_asset_forfeiture_system).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, international_drug_control_treaties).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three constraint stories (prohibition_reading, harm_reduction_reading, legalization_reading) per the ε-invariance principle. Each has distinct ε, beneficiaries, victims, and claimed_type. The prohibition_reading has ε≈0.82 (high extraction on users), harm_reduction_reading has ε≈0.15 (low extraction, service provision), legalization_reading has ε≈0.05 (minimal extraction, regulated market). They are linked here because the prohibition_reading's enforcement apparatus actively suppresses the other readings' policy space — a structural influence edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, powerful, 0.1).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, organized, 0.2).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, powerless, 0.95).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
