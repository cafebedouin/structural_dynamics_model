% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Criminal Prohibition of Substance Use as Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the prohibition_reading of the
 *   substance_control_kernel: the claim that substance use is a moral
 *   transgression requiring state punishment to protect social order. The
 *   standing arrangement under contest is the criminal prohibition regime —
 *   possession laws, mandatory minimums, asset forfeiture, and the
 *   enforcement apparatus that sustains them. The prohibition_reading
 *   assesses this arrangement's extractiveness (ε=0.82) from its own lights:
 *   the enforcement apparatus (police, prisons, prosecutors, political
 *   officeholders) extracts resources, labor, and liberty from people who use
 *   drugs and their communities, while black market violence emerges as a
 *   secondary externality. The harm_reduction_reading and
 *   legalization_reading are separate constraints (other files) with
 *   different ε referents — this story does not average or hedge across them.
 *   The ε referent is fixed: the criminal prohibition arrangement as the
 *   prohibition_reading sees it, not the health-centered or liberty-centered
 *   arrangements the siblings would instantiate.
 *
 * KEY AGENTS:
 *   - law_enforcement_agencies: Primary beneficiary (institutional/arbitrage) — budgets, assets, authority derive from prohibition enforcement
 *   - prison_industrial_complex: Primary beneficiary (institutional/arbitrage) — incarceration revenue, construction contracts, labor extraction
 *   - political_officeholders_drug_war_platform: Primary beneficiary (institutional/arbitrage) — electoral capital from 'tough on crime' positioning
 *   - asset_forfeiture_programs: Primary beneficiary (organized/arbitrage) — direct revenue extraction from targeted communities
 *   - people_who_use_drugs: Primary victim (powerless/trapped) — criminalization, incarceration, loss of housing/employment/benefits
 *   - low_income_communities_of_color: Primary victim (powerless/constrained) — disproportionate enforcement, community destabilization, intergenerational harm
 *   - families_of_incarcerated: Secondary victim (powerless/trapped) — economic loss, child welfare involvement, trauma
 *   - public_health_systems: Secondary victim (organized/constrained) — resources diverted to enforcement, barriers to evidence-based care
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
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Criminal Prohibition of Substance Use as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '14dac15b-ffea-4850-beb5-b664843928a9').
narrative_ontology:cs_kernel_codification('14dac15b-ffea-4850-beb5-b664843928a9', formalized).
narrative_ontology:cs_authority_grounding('14dac15b-ffea-4850-beb5-b664843928a9', extraction).
narrative_ontology:cs_interpretation_layer_present('14dac15b-ffea-4850-beb5-b664843928a9').
narrative_ontology:cs_reading_relation('14dac15b-ffea-4850-beb5-b664843928a9', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('14dac15b-ffea-4850-beb5-b664843928a9', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('14dac15b-ffea-4850-beb5-b664843928a9', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('14dac15b-ffea-4850-beb5-b664843928a9', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('14dac15b-ffea-4850-beb5-b664843928a9', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('14dac15b-ffea-4850-beb5-b664843928a9', state_punishment_protects_social_order, deontological).
narrative_ontology:cs_axiom('14dac15b-ffea-4850-beb5-b664843928a9', secondary, prohibition_is_necessary_condition_for_social_cohesion).
narrative_ontology:cs_axiom_status(prohibition_is_necessary_condition_for_social_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('14dac15b-ffea-4850-beb5-b664843928a9', prohibition_is_necessary_condition_for_social_cohesion, instrumental).
narrative_ontology:cs_reference_frame('14dac15b-ffea-4850-beb5-b664843928a9', classical_prohibition_authority).
narrative_ontology:cs_drift_state('14dac15b-ffea-4850-beb5-b664843928a9', contemporary_evidence_based_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14dac15b-ffea-4850-beb5-b664843928a9', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, political_officeholders_drug_war_platform).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, asset_forfeiture_programs).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, low_income_communities_of_color).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, families_of_incarcerated).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, public_health_systems).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, state_moral_authority_over_body).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, prohibition_as_social_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set enforcement priorities, allocate personnel to drug units, receive federal grant funding tied to drug arrest metrics, operate asset forfeiture programs that directly fund agency budgets. They define what counts as a 'drug problem' and control the data used to justify prohibition's continuation. Exit would mean pivoting to other enforcement priorities but institutional incentives and revenue streams anchor them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Private prison contractors, prison labor programs, construction firms, and supply vendors receive guaranteed revenue from drug incarceration ( ~450,000 people incarcerated for drug offenses on any given day in US). They lobby for mandatory minimums and against sentencing reform. Exit is arbitrage — they could serve other carceral populations but drug prohibition provides stable, high-volume demand.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, prison_industrial_complex, beneficiary,
    institutional, generational, arbitrage, national).

% Elected officials and candidates who build platforms on 'tough on crime' and 'protecting communities from drugs.' They receive campaign contributions from law enforcement unions and prison contractors. Their exit is constrained: pivoting risks primary challenges and loss of institutional support, but some have successfully repositioned as reformers.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, political_officeholders_drug_war_platform, beneficiary,
    institutional, biographical, constrained, national).

% Civil asset forfeiture allows seizure of cash, vehicles, property without criminal conviction. Proceeds flow directly to seizing agencies (equitable sharing with federal). This creates a direct financial incentive for enforcement. Exit is arbitrage — the legal mechanism exists by statute and could be reformed, but the revenue dependency is structural.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, asset_forfeiture_programs, beneficiary,
    organized, immediate, arbitrage, national).

% Face arrest, incarceration, criminal records, loss of housing/employment/public benefits, child removal, and barriers to healthcare. The criminal record creates permanent structural exclusion. Exit is trapped: they cannot 'choose' to stop being targeted without ceasing use (which is not simple for dependent users) or leaving the jurisdiction (which is not feasible for most). Even in legalized states, federal prohibition and collateral consequences persist.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, national).

% Experience disproportionate enforcement (similar use rates, 2-10x arrest/incarceration rates). Community destabilization: removal of working-age adults, erosion of trust in institutions, intergenerational poverty, violence from illegal markets. Exit is constrained: geographic mobility is limited by structural poverty; political voice is suppressed by felony disenfranchisement and surveillance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, low_income_communities_of_color, payer,
    powerless, generational, constrained, national).

% Bear economic costs (lost income, legal fees, commissary, phone calls), child welfare system involvement, housing instability, and trauma. Children of incarcerated parents face elevated risks of poverty, homelessness, and future incarceration. Exit is trapped: they cannot exit the relationship to the incarcerated person, and the constraint's effects cascade across generations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, families_of_incarcerated, payer,
    powerless, biographical, trapped, national).

% Resources diverted from evidence-based treatment (methadone, buprenorphine, syringe services, overdose prevention) to enforcement. Legal barriers prevent implementation of harm reduction (syringe possession laws, crack house statutes). Funding streams (SAMHSA, CDC) are dwarfed by enforcement budgets. Exit is constrained: institutional mandates and funding structures are locked to prohibition framework; reform requires legislative change.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_systems, payer,
    organized, generational, constrained, national).

% Provide syringe services, overdose prevention, low-threshold treatment — directly contradicting prohibition's moral logic. They are criminalized (paraphernalia laws), defunded, and excluded from policy tables. Their exclusion is structural: the prohibition_reading defines their work as 'enabling' rather than saving lives. Exit is constrained: they operate in legal grey zones, dependent on philanthropic funding and local political cover.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% UNODC, INCB, CND monitor compliance with UN drug conventions (1961, 1971, 1988) that mandate criminalization. They produce the global data framework that prohibition_reading cites as legitimacy. Their analytical seat sees the global regime; they have no enforcement power but their classifications shape national policy space.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, international_drug_control_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition_reading claims to coordinate social order by defining and punishing moral transgression — creating a shared moral boundary that (it asserts) prevents social dissolution. In practice, the arrangement coordinates resource flows to enforcement institutions and manages surplus populations through carceral control.
% TRANSFER_FUNCTION: Moves liberty, labor, assets, and public resources from people who use drugs and their communities to law enforcement agencies, prison systems, and political officeholders. Asset forfeiture transfers property directly. Incarceration transfers labor (prison labor at cents/hour) and public funds ($80B+/year). Federal grants transfer tax revenue to local enforcement. The black market transfers violence risk to communities.
% ABSENT_VOICES: People who use drugs are structurally excluded from policy formation — criminalized, disenfranchised, stigmatized. Their voices appear only as 'addicts' in testimony, not as rights-bearing citizens. Communities most affected by enforcement and black market violence are excluded from legislative hearings where prohibition is renewed. International voices from countries that have decriminalized (Portugal, Czechia) or legalized (Uruguay, Canada) are dismissed as 'not applicable.'
% DISAPPEARANCE_RATIONALE: If criminal prohibition vanished overnight: (1) 450,000+ people would be released from incarceration for drug offenses; (2) $80B+ annual enforcement expenditure would be unallocated; (3) illegal markets would face competition from legal supply, collapsing violence profits; (4) public health systems could implement evidence-based care without legal barriers; (5) asset forfeiture revenue would cease; (6) political platforms built on drug war would collapse. The world would rearrange profoundly — the constraint organizes massive resource flows and institutional structures.
% FOUNDING_PROBLEM: The arrangement was built to address: (1) perceived moral decay from substance use (temperance/progressive era); (2) racialized anxieties about drug use in minority communities (anti-opium, anti-cocaine, anti-marijuana campaigns 1900s-1930s); (3) international trade control (Shanghai Commission 1909, Hague 1912). The 'social order' justification was always entangled with racial and class control.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Courtwright, Musto, Alexander, Herzberg) documents that the founding problems — moral panic about 'drug fiends,' racialized fears of interracial contact, international opium trade control — are historically specific and substantially resolved or transformed. The enforcement apparatus's own data (rising overdose deaths, stable use rates, increasing potency) corroborates that the arrangement fails at its stated protective function. No independent public health or criminology body attests that criminal prohibition protects social order; the corroboration for 'dead' comes from the constraint's own failure metrics.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness is high (0.82) because the prohibition regime transfers massive resources from targeted communities to enforcement institutions — asset forfeiture ($2B+ annually federal alone), incarceration costs ($80B+ annually), and the labor of incarcerated people — while suppressing alternatives (harm reduction, treatment, regulation). Suppression is very high (0.88) because the constraint's persistence depends on active coercion: arrest, incarceration, surveillance, and the elimination of legal supply alternatives. Theater ratio (0.42) reflects that enforcement rituals (drug courts, 'treatment instead of incarceration' programs) often function as elaborate processing mechanisms that preserve the carceral logic while appearing reformist. Accessibility collapse (0.78) is high because legal alternatives are structurally foreclosed — the constraint makes legal supply impossible by definition. Resistance (0.65) is significant: harm reduction movements, drug policy reform advocacy, jury nullification, and community organizing persist despite repression. The claimed type is snare: the coordination story (protecting social order through moral punishment) is cover; the constraint persists through coercion and suppression of exits (decriminalization, legal regulation, harm reduction).
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement apparatus seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as necessary coordination — the state fulfilling its moral duty. From the victim seats (powerless, trapped/constrained exit), the same structure operates as pure extraction: liberty, resources, and community stability are transferred to the enforcement apparatus with no reciprocal benefit. The engine computes this per-seat divergence from the structural data: beneficiaries declared in base_properties map to low directionality (d→0), victims map to high directionality (d→1). The black market violence externality creates a third seat — communities experiencing violence — who are neither beneficiaries nor direct targets of enforcement but bear extracted costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (law_enforcement_agencies, prison_industrial_complex, political_officeholders, asset_forfeiture_programs) collect the constraint's gains directly: budgets, revenue, electoral capital, asset seizures. Their exit is arbitrage — they can pivot to other enforcement priorities but choose to maintain prohibition because it is institutionally lucrative. Victims (people_who_use_drugs, low_income_communities_of_color, families_of_incarcerated) bear the costs: incarceration, fines, collateral consequences, community violence. Their exit is trapped (criminal record barriers) or constrained (limited mobility, structural poverty). Public_health_systems are secondary victims: organized power but constrained exit (funding streams tied to enforcement priorities). The enforcement apparatus is the primary gain_flow recipient — not the stated 'social order' abstraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (addressing substance-related harm) is dead or contested — evidence shows prohibition increases harm (overdose, violence, disease transmission) while failing to reduce use. The arrangement persists because the enforcement apparatus extracts enough benefit to maintain it, not because it solves the founding problem. This is mandatrophy: the mandate (protect social order) has atrophied into a self-sustaining extraction mechanism. The prohibition_reading's axioms (moral transgression, state punishment as protection) are holdable deontological claims that foreclose empirical falsification — the constraint does not update on evidence of failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_kernel_commitment,
    'Is this constraint one reading of the contested substance_control_kernel, instantiated as prohibition_reading, with harm_reduction_reading and legalization_reading as sibling readings?',
    'Structural decomposition per ε-invariance principle: each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and classification. The kernel structure is confirmed when sibling readings produce non-overlapping ε profiles and distinct victim sets.',
    'If confirmed, this constraint must not average or hedge across readings; its ε refers solely to the standing arrangement of criminal prohibition as the prohibition_reading assesses it. The sibling readings are separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_kernel_commitment, conceptual, 'Confirms this story is a single kernel reading, not a conflation of the kernel''s contested framings.').

omega_variable(
    moral_transgression_vs_harm_reduction_boundary,
    'Does the prohibition_reading''s core premise (substance use as moral transgression requiring punishment) logically foreclose the harm_reduction_reading''s premise (use as health condition requiring pragmatic intervention), or do they coexist as competing frameworks?',
    'Analyze whether a single legal-political framework can simultaneously hold that use is categorically a moral crime meriting punishment AND that the primary state response should be health-centered harm reduction without requiring cessation. Historical evidence: jurisdictions that adopt harm reduction (e.g., Portugal, Switzerland) formally decriminalize or deprioritize punishment; the two premises operate in different frameworks.',
    'If forecloses, the readings cannot coexist in one framework — the engine''s cs_axiom_contradiction will detect this via axiom grounding_type (deontological vs. empirically_contingent) and drift_state. If coexists_with, both remain live positions held by different institutional coalitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_transgression_vs_harm_reduction_boundary, conceptual, 'Structural relationship between prohibition_reading and harm_reduction_reading premises.').

omega_variable(
    prohibition_legalization_foreclosure,
    'Does the prohibition_reading''s core premise (state must punish use to protect social order) logically foreclose the legalization_reading''s premise (state intervenes only for third-party harm)?',
    'Examine whether a framework can hold both that use itself is a moral transgression meriting state punishment AND that the state has no legitimate interest in punishing use absent third-party harm. The premises are contradictory: one asserts intrinsic wrongness of use; the other denies state authority over self-regarding conduct.',
    'If forecloses, no single framework holds both — they are mutually exclusive readings of the kernel. If influences, prohibition_reading''s enforcement apparatus creates structural pressure (resource allocation, institutional momentum) that makes legalization_reading harder to instantiate without foreclosing it logically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_legalization_foreclosure, conceptual, 'Structural relationship between prohibition_reading and legalization_reading premises.').

omega_variable(
    enforcement_extraction_measurement,
    'How much of the measured extractiveness (ε=0.82) derives from the enforcement apparatus''s self-sustaining budgetary and institutional interests versus the stated moral-protection function?',
    'Disaggregate enforcement budgets: asset forfeiture proceeds, federal grant dependencies (Byrne JAG, HIDTA), prison construction/operation contracts, overtime pay structures tied to drug arrests. Compare to public health expenditure on the same substances. Cross-reference with arrest data showing possession vs. distribution ratios.',
    'If enforcement self-interest dominates, the constraint is a snare with the enforcement apparatus as primary beneficiary. If moral-protection function dominates, extraction is lower and the constraint may classify as tangled_rope (coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_extraction_measurement, empirical, 'Disentangling enforcement self-interest from stated function in extractiveness measurement.').

omega_variable(
    black_market_violence_attribution,
    'Is the black market violence (secondary externality) structurally attributable to the prohibition_reading''s enforcement regime, or is it an independent criminal phenomenon?',
    'Compare violence rates pre/post prohibition regimes (e.g., alcohol prohibition 1920-1933), cross-jurisdiction comparisons (legal vs. prohibited markets for same substance), and economic models of illegal market violence as enforcement-induced scarcity rent.',
    'If attributable to prohibition, the constraint''s extraction extends beyond direct enforcement costs to include violence externalities borne by communities — strengthening snare classification. If independent, the constraint''s measured suppression may overstate its responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(black_market_violence_attribution, empirical, 'Causal attribution of black market violence to the prohibition constraint.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the high suppression (0.88) primarily structural (arrest, incarceration, legal barriers) or does it include internalized suppression (stigma, self-concept as criminal, community surveillance)?',
    'Post-decriminalization suppression trajectory: if suppression persists after legal barriers are removed (measured via stigma scales, employment discrimination, healthcare avoidance), the internalized component is confirmed. Longitudinal studies of communities after reform.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry suppression post-exit. This routes to omega classification impact for interpersonal/institutional constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in drug prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(substance_control_kernel__prohibition_reading_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, mass_incarceration_kernel__drug_war_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, policing_kernel__asset_forfeiture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the substance_control_kernel family. The prohibition_reading (this story) claims substance use is moral transgression requiring state punishment. The harm_reduction_reading claims use is health condition requiring pragmatic intervention. The legalization_reading claims use is liberty issue with state intervention only for third-party harm. These are not the same constraint measured differently — they have different ε (0.82 vs ~0.35 vs ~0.15), different victim sets, different beneficiary structures, and different classifications. The kernel's label 'drug policy' conflates them; the framework disambiguates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, institutional, 0.1).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, powerless, 0.95).
constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
