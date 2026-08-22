% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Legalization with Externality Regulation
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the LEGALIZATION READING of the
 *   substance-control kernel. Under this reading, substance use is framed as
 *   an individual liberty issue; state intervention is justified only to
 *   prevent third-party externalities (secondhand exposure, impaired driving,
 *   fetal harm) and to capture externality costs through taxation and
 *   regulation. Users are removed from the victim set by the reading's core
 *   premise (autonomy primacy); third parties enter the victim set as bearers
 *   of externality costs. The legal distribution industry emerges as a new
 *   beneficiary; the black market becomes the loser. The state transitions
 *   from criminalizer to tax collector and externality regulator — a
 *   structural shift in its role within the constraint.
 *
 * KEY AGENTS:
 *   - substance_users: primary beneficiary (exiting victim set under liberty framing)
 *   - legal_distribution_industry: new beneficiary (capturing market from prohibition regime)
 *   - state_as_tax_collector: agenda-setter and secondary beneficiary (collecting externality tax and licensing revenue)
 *   - third_parties_exposed_to_externalities: primary victim (externality bearers under legalization regulation)
 *   - dependent_populations_in_high_exposure_zones: deepest victim (powerless, trapped exposure concentration)
 *   - black_market_operators: payer (losing market viability to legal alternative)
 *   - prohibition_advocates: excluded (their normative premise is foreclosed by legalization's liberty-priority frame)
 *   - harm_reduction_advocates: observer (skeptical of whether externality regulation protects adequately)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.38).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.22).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Legalization with Externality Regulation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '73b97423-6f57-4ff5-b028-265cc102def7').
narrative_ontology:cs_kernel_codification('73b97423-6f57-4ff5-b028-265cc102def7', formalized).
narrative_ontology:cs_authority_grounding('73b97423-6f57-4ff5-b028-265cc102def7', extraction).
narrative_ontology:cs_interpretation_layer_present('73b97423-6f57-4ff5-b028-265cc102def7').
narrative_ontology:cs_reading_relation('73b97423-6f57-4ff5-b028-265cc102def7', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('73b97423-6f57-4ff5-b028-265cc102def7', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('73b97423-6f57-4ff5-b028-265cc102def7', foundational, individual_liberty_primacy_in_use_choice).
narrative_ontology:cs_axiom_status(individual_liberty_primacy_in_use_choice, holdable).
narrative_ontology:cs_axiom_grounding('73b97423-6f57-4ff5-b028-265cc102def7', individual_liberty_primacy_in_use_choice, deontological).
narrative_ontology:cs_axiom('73b97423-6f57-4ff5-b028-265cc102def7', foundational, state_intervention_limited_to_externality_prevention).
narrative_ontology:cs_axiom_status(state_intervention_limited_to_externality_prevention, holdable).
narrative_ontology:cs_axiom_grounding('73b97423-6f57-4ff5-b028-265cc102def7', state_intervention_limited_to_externality_prevention, instrumental).
narrative_ontology:cs_reference_frame('73b97423-6f57-4ff5-b028-265cc102def7', individual_liberty_primacy_with_third_party_protection).
narrative_ontology:cs_drift_state('73b97423-6f57-4ff5-b028-265cc102def7', contemporary_regulatory_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73b97423-6f57-4ff5-b028-265cc102def7', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_distribution_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_as_tax_collector).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, criminal_justice_diversion_beneficiaries).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, dependent_populations_in_high_exposure_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, black_market_operators).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, individual_autonomy_primacy).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, state_harm_prevention_mandate).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, externality_internalization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can purchase and consume substances legally without criminal penalty, incarceration risk, or felony record. Under legalization reading, their autonomy over their own body is vindicated as a foundational liberty claim. They pay taxes/fees on purchases but avoid criminal system involvement. Exit option is legality itself — if enforcement returned to prohibition, they would face sudden recriminalization.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    organized, biographical, mobile, national).

% Licensed retailers, cultivators, manufacturers, and testing labs operate openly and profitably. They collect rents from a legalized market, operate within regulatory frameworks they help shape, and coordinate with state licensing. Their beneficiary position is tied to the sustained legalization regime — a return to prohibition would eliminate their legitimate operation entirely.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_distribution_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Collects excise taxes, licensing fees, and sales tax from the legalized market. Sets regulatory rules (potency caps, labeling, age restrictions, packaging) and enforces them. Redeploys enforcement resources from criminalization to externality management and tax collection. The state both governs the constraint and benefits from tax revenue — an ambiguous position that under legalization reading is theoretically justified by capturing externality costs.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_as_tax_collector, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_as_tax_collector, beneficiary).

% Communities historically targeted by drug criminalization (low-income, racial minorities) experience reduced incarceration, reduced felony records, reduced police presence in their neighborhoods, and reduced destabilization of families and employment. Legalization reading frames this as correcting an injustice; their beneficiary position is precarious because it depends on continued non-recriminalization and on equitable regulatory design.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, criminal_justice_diversion_beneficiaries, beneficiary,
    moderate, generational, constrained, national).

% Bear costs of secondhand smoke, fetal exposure, impaired driving incidents, and public consumption. Under legalization reading, they are the primary justification for state intervention: the state steps in to regulate and internalize these externalities (age restrictions, consumption location bans, impaired driving enforcement, prenatal exposure liability). Their victim status is asymmetric: they did not choose the exposure, and their exit options are constrained (cannot move entire neighborhoods, cannot prevent fetal exposure to partners' use).
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities, payer,
    powerless, biographical, trapped, regional).

% Children, pregnant people, and other vulnerable groups in high-substance-use environments face concentrated secondhand/fetal exposure, normalization pressures, and limited geographic mobility. Under legalization reading, they are theoretically protected by regulatory restrictions on sales to minors, prenatal liability rules, and consumption location bans — but enforcement and design equity are contestable. Their position is deeply asymmetric: victim status without choice and without exit.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, dependent_populations_in_high_exposure_zones, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, dependent_populations_in_high_exposure_zones, excluded).

% Lose market share, revenue, and operational capacity as legalized alternatives undercut their prices and remove legal risk from customers. Some operators persist in gray zones (unlicensed high-potency products, untaxed sales) or in geographic areas where legalization is incomplete. Their payer status is structural: legalization directly attacks their revenue model and replaces it with a legal alternative.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, payer,
    organized, biographical, trapped, national).

% Observe legalization reading skeptically: they argue it prioritizes market access over health infrastructure and that externality regulation may prove insufficient to protect vulnerable populations. They see the constraint as a policy choice that reflects one normative frame (liberty) over another (health mitigation). Their observational position allows them to document divergence between legalization's stated externality-protection mechanism and actual regulatory enforcement.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, harm_reduction_advocates, observer,
    moderate, generational, analytical, national).

% Are structurally excluded from the legalization reading's policy framework: they argue substance use is inherently harmful and should be criminalized regardless of externality. Their exclusion is not accidental — the legalization reading's core premise (individual liberty primacy, state intervention only on externality) forecloses their premise (state should prohibit based on intrinsic harm). They remain active in political opposition but are outside the constraint's legitimacy structure.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, prohibition_advocates, excluded,
    powerful, generational, constrained, national).

% Enforce age restrictions, potency limits, labeling rules, consumption location bans, and impaired driving statutes. Under legalization reading, their role is narrow: protect third parties from externalities, not punish autonomous use. Enforcement intensity and equity across communities become contested — the constraint's structural claim (state acts as externality arbiter) is separate from whether enforcement actually does this.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, regulatory_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_as_tax_collector).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal market channel for substance distribution under potency, labeling, and age-restriction controls; redirects state enforcement from criminalization to externality management (secondhand exposure, impaired driving, prenatal harm). Solves the collective-action problem of preventing criminal-market expansion and unregulated product harm while preserving user autonomy.
% TRANSFER_FUNCTION: Moves tax revenue and licensing fees from substance purchases to the state; moves enforcement resources from criminal prosecution to regulatory monitoring; moves market share from black-market suppliers to licensed distributors. Individual users transfer some autonomy constraint (age, location, potency disclosure) in exchange for legal status and criminal-system exit.
% ABSENT_VOICES: Individuals in heavily affected communities who experience secondhand exposure and lack exit options (geographic mobility, family relocation) — they are structurally excluded because legalization's regulatory design is authored by policy makers and industry, not by those bearing the externality concentration. Harm-reduction practitioners argue the framework omits health infrastructure they see as essential.
% DISAPPEARANCE_RATIONALE: If legalization framework disappeared and recriminalization returned, the legal distribution industry would collapse, users would face recriminalization, tax revenue would evaporate, incarceration would resume, and enforcement resources would shift back to criminal prosecution — the entire political economy would reorganize around punishment rather than regulation.
% FOUNDING_PROBLEM: Mass incarceration for substance possession (disproportionately affecting low-income and racial-minority communities) without corresponding reduction in substance use; criminal markets operating with unregulated products and violence; criminal records destabilizing employment and families; state resources consumed by prosecution rather than harm reduction.
% FOUNDING_PROBLEM_CORROBORATION: Incarceration data, criminology research, and community testimony from jurisdictions with legalization confirm that mass criminalization persisted without reducing use. Legalization jurisdictions (Colorado, Portugal, Canada) provide external corroboration that the founding problem — unnecessary incarceration — is soluble by legal framework change. Harm-reduction and criminal-justice reform advocates outside the beneficiary set corroborate the problem statement but dispute the legalization reading's adequacy as a solution.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.28) because the constraint transfers wealth and autonomy. Users gain legal status and criminal-system exit (beneficiary flow), but third parties bear new concentrated externality costs that were previously socialized as criminal harms (victim flow). The legal industry captures rents from market monopoly (beneficiary flow). The state's position is ambiguous: it benefits from tax revenue but theoretically acts as externality arbiter. Extractiveness rises over the interval as implementation reveals gaps between regulatory design and enforcement equity — the constraint's functional performance degrades relative to its stated purpose (protecting third parties while preserving user autonomy). Suppression is low (0.22) because legalization reading's core premise is liberty, not coercion; enforcement targets specific harms (age limits, impaired driving) rather than the status of use itself. Theater ratio is low (0.18) because regulatory enforcement is relatively functional — there is less performative activity than in pure prohibition regimes where enforcement defends moral doctrine. The rise in theater over the interval reflects growing regulatory theater around potency caps and marketing restrictions that may exceed what externality reduction requires. Accessibility collapse is moderate (0.45) because alternatives persist: users could shift to unregulated products or jurisdictions; third parties could advocate for tighter restrictions. Resistance is elevated (0.58) because prohibition advocates remain active, harm-reduction practitioners contest regulatory design adequacy, and third-party constituencies challenge whether externality internalization is genuinely occurring.
 *
 * PERSPECTIVAL GAP:
 *   A user's seat computes the constraint as enabling (low d, high subsidy); a third-party exposed to secondhand smoke computes it as imposed (high d, high extraction). The agenda-setter (state) computes from mixed motivations (revenue collection, externality management, political pressure from multiple constituencies). An observer seat (harm-reduction practitioner or prohibition advocate) computes the constraint's type divergence from its stated purpose as evidence of drift — theater rising, externality regulation proving selective or unequally enforced. The metrics model this divergence: as theater rises and extractiveness rises (despite rhetoric of liberty and harm reduction), the computed per-seat types should diverge. The claim (rope) and the metrics (rising extraction, rising theater) should diverge, generating the engine's delta signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the legalization reading's frame, users transition from victims (of criminalization) to beneficiaries (of liberty vindication). This is not a metric shift — it is a frame shift. The ENGINE computes directionality from structural data (power, exit options, beneficiary/victim declarations); the reading declares users as beneficiaries and authorizes the lower-d computation. Third parties enter the victim set because the reading's externality focus explicitly names them as the constraint's target bearers — they have trapped exit options (cannot move neighborhoods, cannot prevent fetal exposure to partners' use) and face concentrated costs (secondhand exposure, normalization pressure). The legal industry is beneficiary by market rent; the state is ambiguously both agenda-setter and rent collector. Prohibition advocates are excluded because the reading's core axiom (individual liberty primacy) logically forecloses their axiom (state should protect through prohibition regardless of externality). Their structural exclusion is not a power imbalance — it is incompatibility of premises.
 *
 * MANDATROPHY ANALYSIS:
 *   Legalization reading does NOT resolve mandatrophy; it relocates it. The founding problem (mass incarceration) is live and solvable via decriminalization. But the regulatory promise (protect third parties via externality internalization) may outlive its functional justification as enforcement equity diverges from stated purpose. Mandatrophy could emerge if the state maintains potency caps, consumption location bans, and licensing restrictions ostensibly for third-party protection but actually for revenue and control — then the constraint's founding problem (correcting criminalization injustice) becomes dead, but the regulatory architecture persists. The omega variables on enforcement equity and regulatory capture track this divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_trajectory,
    'Will the legalization regime''s stated externality-protection purpose persist, or will it drift toward maximizing state tax revenue and controlling user behavior independent of externality?',
    'Monitor enforcement equity across communities (do potency caps and location restrictions apply equally or concentrate on low-income areas?), compare regulatory strictness to empirically measured externality costs, audit licensing decisions for capture by incumbent industry players.',
    'If regulatory capture occurs (enforcement becomes decoupled from externality minimization), the constraint reclassifies from rope toward snare — the theater ratio will rise, extractiveness will rise, and the foundational mandate will become dead (correcting injustice) while the architecture persists (capturing value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_trajectory, empirical, 'Will legalization''s externality-protection mechanism persist or drift toward revenue maximization?').

omega_variable(
    third_party_externality_internalization_adequacy,
    'Does the regulatory architecture actually internalize third-party externality costs, or does it redistribute them to spatially concentrated vulnerable populations?',
    'Compare secondhand exposure rates, prenatal exposure rates, and normalization effects pre- and post-legalization in equitably regulated zones versus capture-affected zones; assess whether dependent populations in high-exposure zones experience protective effect or concentration of harm.',
    'If internalization fails and harm concentrates on trapped populations, the victim set persists despite legal status shift, and externality regulation becomes performative (theater increases); the constraint reclassifies as snare from vulnerable-population seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_externality_internalization_adequacy, empirical, 'Are third-party externalities genuinely internalized or redistributed to vulnerable populations?').

omega_variable(
    reading_premise_stability_liberty_primacy,
    'Will the legalization reading''s foundational axiom — individual liberty primacy in substance use — withstand political and public-health pressure, or will it be overridden by harm-maximization concerns?',
    'Monitor policy drift: do new regulations begin to restrict use beyond externality prevention (e.g., mandatory abstinence programs, medicalization of use, restrictions on potency for non-externality reasons)? Does legislative intent shift toward prohibition-adjacent justifications?',
    'If liberty primacy is overridden, the reading''s axiom_status changes from holdable to overridden, and the legalization reading converges toward harm-reduction reading (health-maximization dominance). The structural divergence between readings collapses, making the sibling relationship foreclosure-adjacent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_premise_stability_liberty_primacy, conceptual, 'Will the legalization reading''s liberty-primacy axiom withstand political pressure or be overridden?').

omega_variable(
    committer_frame_reading_incompatibility,
    'Is the legalization reading''s normative frame (liberty + externality regulation) internally coherent, or does it collapse under the structural reality that third parties cannot consent to externality and exit options are bounded?',
    'Philosophical analysis of whether third-party protection via regulation can be justified under liberty-primacy axioms when exit is trapped; empirical study of whether regulatory design actually preserves third-party choice or simply shifts coercion from users to third parties.',
    'If the frame is incoherent, the reading becomes untenable, and substance control relocates entirely to harm-reduction and prohibition frames (legalization reading forecloses itself). If coherent, the reading persists but requires strong externality-protection enforcement to vindicate its premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_incompatibility, conceptual, 'Is the legalization reading''s normative frame internally coherent under its own liberty axiom?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(subs_tr_t16, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(subs_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(subs_be_t16, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(subs_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.14).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement_basis(subs_su_t16, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(subs_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, criminal_incarceration_externality).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, secondhand_exposure_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance-control kernel, instantiating the legalization frame (individual liberty, externality prevention). Sibling readings (prohibition_reading, harm_reduction_reading) have different ε values, different beneficiary/victim structures, and different claimed types because they answer the founding-problem question differently. The network links all readings; structural comparison requires reading all three stories and observing where ε, beneficiaries, and type diverge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
