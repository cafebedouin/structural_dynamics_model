% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 Positive Entitlement Reading: State Provision of Material Conditions for Life and Security
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states: 'Everyone
 *   has the right to life, liberty and security of person.' The positive
 *   entitlement reading interprets 'life' and 'security' as obligating the
 *   state to provide material conditions — healthcare, housing, food
 *   security, social protection — necessary for their realization. This
 *   reading grounds the postwar welfare state and justifies progressive
 *   taxation, wealth redistribution, and speech constraints on expression
 *   that undermines vulnerable populations' dignity and material security.
 *   The constraint is CLAIMED as tangled_rope (real coordination function in
 *   material provision + asymmetric extraction from property owners and
 *   high-income earners) and the authored metrics describe substantially
 *   extractive, actively enforced operation with rising theater over time as
 *   welfare bureaucracies expand while founding-problem justifications
 *   weaken.
 *
 * KEY AGENTS:
 *   - vulnerable_populations: beneficiaries of material provision mandates (powerless, trapped exit)
 *   - welfare_dependent_groups: recipients of income support and services (moderate power, constrained exit)
 *   - property_owners and high_income_earners: bear extraction burden via progressive taxation (powerful, mobile/arbitrage exit)
 *   - expression_constrained_groups: face speech restrictions justified as protecting vulnerable populations (moderate power, constrained exit)
 *   - state_welfare_apparatus: agenda-setter, administrator, institutional beneficiary (institutional power, analytical exit)
 *   - constitutional_courts: observer, arbiter of Article 3 interpretation (institutional power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 Positive Entitlement Reading: State Provision of Material Conditions for Life and Security").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '1fdd38b8-e0a0-4d63-bab4-20772471abf2').
narrative_ontology:cs_kernel_codification('1fdd38b8-e0a0-4d63-bab4-20772471abf2', fixed_text).
narrative_ontology:cs_authority_grounding('1fdd38b8-e0a0-4d63-bab4-20772471abf2', lineage).
narrative_ontology:cs_interpretation_layer_present('1fdd38b8-e0a0-4d63-bab4-20772471abf2').
narrative_ontology:cs_reading_relation('1fdd38b8-e0a0-4d63-bab4-20772471abf2', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('1fdd38b8-e0a0-4d63-bab4-20772471abf2', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('1fdd38b8-e0a0-4d63-bab4-20772471abf2', foundational, material_conditions_constitutive_of_life).
narrative_ontology:cs_axiom_status(material_conditions_constitutive_of_life, holdable).
narrative_ontology:cs_axiom_grounding('1fdd38b8-e0a0-4d63-bab4-20772471abf2', material_conditions_constitutive_of_life, deontological).
narrative_ontology:cs_axiom('1fdd38b8-e0a0-4d63-bab4-20772471abf2', foundational, state_obligation_active_provision).
narrative_ontology:cs_axiom_status(state_obligation_active_provision, holdable).
narrative_ontology:cs_axiom_grounding('1fdd38b8-e0a0-4d63-bab4-20772471abf2', state_obligation_active_provision, deontological).
narrative_ontology:cs_reference_frame('1fdd38b8-e0a0-4d63-bab4-20772471abf2', postwar_material_provision_mandate).
narrative_ontology:cs_drift_state('1fdd38b8-e0a0-4d63-bab4-20772471abf2', contemporary_developed_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1fdd38b8-e0a0-4d63-bab4-20772471abf2', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_dependent_groups).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_income_earners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_constrained_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on state-provided healthcare, housing, food security, and social protection. Without these material conditions, their life and security cannot be realized. The positive entitlement reading obligates the state to provision these goods as a matter of human right, not charity. Their exit option is essentially nonexistent — they cannot opt out of needing material sustenance.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Receive income support, unemployment benefits, child allowances, and other transfers mandated by welfare state architecture grounded in the positive entitlement reading. The reading legitimizes ongoing redistribution as a constitutional obligation rather than discretionary policy. They have some exit (labor market participation, migration) but depend materially on the continuance of the entitlement.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_dependent_groups, beneficiary,
    moderate, biographical, constrained, national).

% Bear the tax burden financing state provision of material conditions through progressive taxation, wealth taxes, and property levies. The positive entitlement reading constrains their property rights — they cannot refuse to contribute to welfare provision on the grounds that it violates their liberty. Their exit option is capital mobility or jurisdictional arbitrage, but relocation carries costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_owners, payer,
    powerful, biographical, mobile, national).

% Face confiscatory marginal tax rates and wealth confiscation justified by the positive entitlement reading as necessary to fund material provision. They have stronger exit options than property owners — capital flight, expatriation, or relocation to lower-tax jurisdictions. The reading's enforcement depends partly on suppressing these exits.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_income_earners, payer,
    powerful, biographical, arbitrage, global).

% Experience speech restrictions (hate speech law, defamation enforcement, incitement prohibitions) justified under the positive entitlement reading as necessary to protect vulnerable populations' dignity and security. The reading treats expression as constrained by material welfare obligations — speech that undermines vulnerable groups' material security is treated as a threat to Article 3 itself. Their exit from these constraints is limited by the state's enforcement.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_constrained_groups, payer,
    moderate, biographical, constrained, national).

% Administers the material provision system: taxation, benefit distribution, healthcare delivery, housing allocation, food security programs. The positive entitlement reading grounds the apparatus's legitimacy and justifies its continuous expansion and enforcement. The apparatus is both the enforcer of the constraint and its primary institutional beneficiary.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_welfare_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Interpret Article 3 and determine whether state provision of material conditions is constitutionally mandated or merely permitted. Different courts embrace or reject the positive entitlement reading, creating interstate variance in Article 3's operation. They see testimony from property owners claiming violation, from vulnerable populations claiming inadequate provision, and from the state.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Monitor state compliance with Article 3 and enforce accountability for inadequate material provision. They receive complaints from vulnerable groups claiming the state fails to provide healthcare, housing, food; they issue interpretations and soft-law recommendations endorsing the positive entitlement reading. They have no enforcement power but set the interpretive norm.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, international_human_rights_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, state_welfare_apparatus).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of ensuring material conditions sufficient for life and security across a population: healthcare access, housing safety, food security, and basic income support are coordinated at the state level rather than left to market or familial provision. The positive entitlement reading makes this coordination a human right obligation, not optional social policy.
% TRANSFER_FUNCTION: Moves income, property, labor, and wealth from high-income and property-owning groups to vulnerable and welfare-dependent groups via progressive taxation, social transfers, and state-provided services. The constraint also moves expression rights from speech-protected groups to speech-constrained groups (restrictions justified as protecting vulnerable populations' dignity and material security).
% ABSENT_VOICES: Market-fundamentalist property-rights advocates and libertarian political philosophers who would reject the positive entitlement reading are excluded from the reading itself — their rejection of positive entitlements is the disagreement. They would argue for negative liberty, property rights protection, and voluntary provision. International economic institutions (World Bank, IMF) that endorse austerity and retrenchment of welfare states are structurally excluded from the reading's legitimacy frame.
% DISAPPEARANCE_RATIONALE: If the positive entitlement reading of Article 3 disappeared overnight — replaced by the negative liberty reading or procedural hybrid reading — welfare state architecture would face delegitimization. Progressive taxation loses its constitutional grounding; material provision reverts to charity status; vulnerable populations lose the human-rights basis for claims on state resources. The redistributive state would need to be rebuilt on different legitimacy grounds (efficiency, reciprocity, voluntary preference) or would shrink. Healthcare, housing, and income support systems currently justified by the positive entitlement reading would reorganize around means-testing, market mechanisms, or philanthropic models.
% FOUNDING_PROBLEM: The founding problem is the material deprivation of vulnerable populations in the wake of World War II: concentration camp survivors, refugees, stateless persons, the poor, the ill, and the disabled had no access to basic material conditions required for life and security. The postwar human rights movement read Article 3 as mandating state action to prevent material deprivation, treating it as a human right rather than relying on voluntary provision or charity.
% FOUNDING_PROBLEM_CORROBORATION: The UNHRC and international human rights bodies attest the founding problem is still live — material deprivation persists globally and the state provision obligation remains necessary. Wealthy countries' welfare administrators attest the problem motivates ongoing provision. Property-rights advocates and market-oriented economists attest the founding problem has been solved (basic material survival is no longer threatened by mass deprivation in developed economies) and the entitlement obligation is no longer necessary; legislative evidence from retrenchment debates in neoliberal states shows explicit rejection of the living, foundational problem claim. The corroboration divides cleanly: those who benefit from and administer the positive entitlement reading attest the founding problem is live; those who bear the extraction cost attest it is dead.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The positive entitlement reading couples two distinct structural elements: (1) a genuine coordination function — material provision at the state level solves collective action problems that markets and families cannot handle efficiently (healthcare, housing, income security); (2) substantial extraction — the coordination is used to justify extracting from property owners and high-income earners beyond the marginal cost of providing the goods. Base extractiveness is 0.68 because the constraint transfers significant wealth and property rights. Suppression is moderate (0.52) because resistance from property-owners and libertarian critics is substantial and requires active state enforcement (tax collection, capital controls, speech restrictions). Theater ratio rises over the interval (0.22 to 0.41) because the founding problem (postwar material deprivation) becomes less salient — the material provision function persists but its justification becomes increasingly theatrical, maintained by bureaucratic momentum and benefit-dependency rather than by ongoing crisis. The measurement series shows extractiveness plateauing by year 25–35 as resistance hardens and further extraction becomes difficult; theater continues rising as the state elaborates justificatory narratives while the actual material crisis justifying the entitlement fades.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable populations and welfare administrators perceive the constraint as life-enabling coordination — without state material provision they face deprivation. Property owners perceive the same constraint as confiscatory extraction justified by a reading they reject. Constitutional courts split: courts in high-welfare jurisdictions compute the constraint as coordination with necessary distributional costs; courts in low-welfare or property-rights-oriented jurisdictions compute it as pure extraction masked by rights language. The engine should compute divergent types across these seats from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are beneficiaries with powerless status and trapped exit (d near 0.0, full subsidy). Welfare-dependent groups are beneficiaries with moderate power and constrained exit (d near 0.2). Property owners are victims with powerful status and mobile exit (d near 0.75, substantial target). High-income earners are victims with powerful status and arbitrage exit (d near 0.65, target but with exit options). Expression-constrained groups are victims with moderate power and constrained exit (d near 0.8, high target). The state apparatus is the agenda-setter (d = analytical). This directionality structure shows a constraint that benefits the powerless while extracting from the powerful, which should ordinarily be classified as Robin-Hood-style redistribution; the tangled_rope claim captures that the coordination function is real (beneficiaries gain genuine material security) while the extraction is also real and enforced (property rights are constrained, speech is suppressed, wealth is transferred). Mandatrophy arises when the founding problem (material deprivation crisis) dies but the extraction and suppression persist for bureaucratic reasons.
 *
 * MANDATROPHY ANALYSIS:
 *   The positive entitlement reading faces a live mandatrophy threat: the founding problem (postwar material deprivation, concentration camp survivors, refugees, stateless persons with no access to basic survival goods) has been substantially solved in developed economies via 70+ years of welfare state operation. Vulnerable populations in wealthy countries today face relative deprivation and inequality rather than absolute survival threats. Once the founding problem is solved, the constraint's mandate to extract from property owners to provide material goods becomes increasingly difficult to justify — the extraction persists (theater_ratio rises) but the coordination function becomes less salient. The measuring constraint is whether high-income earners and property owners will accept the positive entitlement reading's extraction claim once the crisis justification fades. If they reject the reading, the constraint degrades into a piton (enforced by bureaucratic inertia and institutional beneficiaries, not by ongoing coordination necessity). The theater_ratio plateau at 0.41 by year 25–35 suggests the constraint has entered a mandatrophy zone where extraction continues but justification becomes thinner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (postwar material deprivation) still live, or has the positive entitlement reading''s own success solved it, leaving the extraction without justification?',
    'Empirical measurement of material deprivation in high-welfare states: if absolute poverty, mortality from preventable causes, homelessness, and healthcare access disparities have fallen to negligible levels among welfare-provision beneficiaries, the founding problem is dead in those jurisdictions. Survey data on whether vulnerable populations attribute survival and security to state provision (live founding problem) vs. perceive the state as unnecessary rent-extraction (dead founding problem).',
    'If the founding problem is dead in high-welfare economies, the constraint should reclassify from tangled_rope (coordination + extraction) to piton (extraction without living coordination function), triggering mandatrophy resolution and potential delegitimization of the positive entitlement reading. If the founding problem is still live, the constraint remains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether material deprivation crisis still justifies the positive entitlement reading''s extraction.').

omega_variable(
    positive_vs_negative_liberty_irreconcilability,
    'Are the positive entitlement reading''s obligations to provide material goods logically incompatible with the negative liberty reading''s protection of property rights and freedom from state interference?',
    'Formal analysis of whether a single constitutional framework can simultaneously protect property ownership and obligate confiscatory taxation for material provision; investigation of whether states have achieved stable equilibrium between the readings or continuously generate conflict.',
    'If the readings logically foreclose each other, the constraint should trigger foreclosure-relation reclassification and one reading cannot be held within a single legal system without the other being rejected. If they can coexist as pragmatic compromises (mixed constitutional orders with both property protection and welfare obligation), the coexists_with relation holds and both can persist as contested but not logically incompatible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_vs_negative_liberty_irreconcilability, conceptual, 'Logical compatibility of positive entitlement vs. negative liberty readings of Article 3.').

omega_variable(
    speech_suppression_legitimacy,
    'Can speech restrictions (hate speech law, defamation enforcement, incitement prohibitions) be legitimately grounded in the positive entitlement reading, or are they a separate constraint using human dignity as cover for viewpoint suppression?',
    'Investigation of whether speech restrictions actually protect vulnerable populations'' material security and human dignity, or whether they function to suppress political criticism of the welfare state and wealth redistribution. Comparative analysis of jurisdictions with strong positive entitlement readings but weak speech restrictions vs. weak entitlements but strong speech restrictions.',
    'If speech restrictions are structurally dependent on the positive entitlement reading (i.e., removing the reading would remove the suppression justification), they are part of the same constraint and the extraction should be counted as including expression costs. If they are independent constraints, the positive entitlement reading should exclude them and focus on material provision; current authored_victims should split into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_suppression_legitimacy, empirical, 'Whether speech suppression is structurally dependent on the positive entitlement reading or a separate constraint.').

omega_variable(
    reading_specificity_vs_generality,
    'Is the positive entitlement reading an authentic interpretation of Article 3 as written, or a political reading that projects 1940s-50s welfare-state ideology onto an ambiguous text?',
    'Historical analysis of Article 3''s drafting (UDHR working group debates, preparatory documents, original intent statements from drafters); comparison of the positive entitlement reading with alternative readings endorsed by different national constitutional courts and human rights bodies.',
    'If the reading is grounded in authentic textual meaning endorsed by the drafters and majority international consensus, it should be treated as the canonical reading and sibling readings as departures. If the reading is a political projection, the constraint''s legitimacy rests on power rather than interpretive integrity, and the constraint should carry an omega documenting this uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_vs_generality, conceptual, 'Authenticity of the positive entitlement reading as an interpretation of Article 3''s text and original intent.').

omega_variable(
    welfare_apparatus_capture,
    'To what extent has the state welfare apparatus been captured by its own institutional interests, defending the positive entitlement reading not because it coordinates material provision but because it defends bureaucratic budgets and personnel?',
    'Investigation of welfare bureaucracy expansion independent of material need; measurement of administrative overhead vs. direct provision; analysis of whether welfare agencies prioritize beneficiary welfare or agency growth; observation of jurisdictions that reduced welfare provision without corresponding material deprivation (indicating the reading can be abandoned without crisis).',
    'If the apparatus is substantially captured, the constraint''s enforcement depends on suppressing exit and alternatives (raising effective extraction). The theater ratio becomes the key signal — as founding-problem justification fades, captured bureaucracies perform escalating justification theater. This supports the piton reclassification if the founding problem is solved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_apparatus_capture, empirical, 'Institutional capture of the welfare apparatus by its own perpetuation interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(udhr_tr_t5, udhr_article_3__positive_entitlement_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(udhr_tr_t10, udhr_article_3__positive_entitlement_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(udhr_tr_t20, udhr_article_3__positive_entitlement_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(udhr_tr_t25, udhr_article_3__positive_entitlement_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(udhr_tr_t35, udhr_article_3__positive_entitlement_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(udhr_be_t5, udhr_article_3__positive_entitlement_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(udhr_be_t10, udhr_article_3__positive_entitlement_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(udhr_be_t20, udhr_article_3__positive_entitlement_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(udhr_be_t25, udhr_article_3__positive_entitlement_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(udhr_be_t35, udhr_article_3__positive_entitlement_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(udhr_su_t5, udhr_article_3__positive_entitlement_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(udhr_su_t10, udhr_article_3__positive_entitlement_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(udhr_su_t20, udhr_article_3__positive_entitlement_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(udhr_su_t25, udhr_article_3__positive_entitlement_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(udhr_su_t35, udhr_article_3__positive_entitlement_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.18).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR is a contested kernel instantiated by three structurally distinct constraints. The positive_entitlement_reading interprets 'life' and 'security' as obligating state material provision; the negative_liberty_reading interprets them as prohibiting state deprivation except via procedure; the procedural_hybrid_reading treats Article 3 as guaranteeing due-process protections without resolving substantive liberty/welfare content. Each reading has distinct epsilon values, beneficiary/victim structures, and types. The positive_entitlement_reading shows high extraction (0.68) because it justifies wealth redistribution; the negative_liberty_reading shows extraction in suppression of redistribution; the procedural reading shows enforcement overhead without asymmetric transfer. These are distinct constraints (different epsilon, different structural data) linked by common kernel origin and mutual logical influence via reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, powerless, 0.05).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
