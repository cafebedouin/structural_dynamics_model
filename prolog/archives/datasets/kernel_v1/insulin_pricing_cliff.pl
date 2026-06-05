% ============================================================================
% CONSTRAINT STORY: insulin_pricing_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insulin_pricing_cliff, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: insulin_pricing_cliff
 *   human_readable: Insulin Pricing Cliff: Market Capture and Extraction from Insulin-Dependent Patients
 *   domain: healthcare/pharmaceutical_pricing/metabolic_disease
 *
 * SUMMARY:
 *   The insulin pricing cliff represents a pure extraction mechanism
 *   targeting patients with Type 1 diabetes and insulin-dependent Type 2
 *   diabetes patients — populations with zero negotiating power due to
 *   absolute biological dependency. Insulin is metabolically non-negotiable:
 *   missing doses causes diabetic ketoacidosis, coma, and death. The
 *   constraint operates through a multi-layered enforcement apparatus:
 *   insulin manufacturers (Eli Lilly, Novo Nordisk, Sanofi) set list prices
 *   that have escalated 300% since 1996; pharmacy benefit managers layer
 *   rebate structures and formulary restrictions; insurance companies enforce
 *   prior authorization requirements; and the FDA maintains regulatory
 *   abstinence from price governance. The combination creates a snare:
 *   patients face list prices of $300–400/vial, even those with insurance
 *   face $35–100 copays per vial, and uninsured patients face near-total
 *   cost. Rationing and medical non-adherence are systematic outcomes. The
 *   constraint exhibits all six DR types from different perspectives: a snare
 *   from the patient perspective, pure coordination (rope) from manufacturer
 *   and PBM perspectives, mixed coordination-extraction (tangled rope) from
 *   organized patient advocacy and employer plan perspectives, a degraded
 *   regulatory ritual (piton) from the FDA perspective, identity-locked
 *   entrapment (snare with cognitive capture) from healthcare worker
 *   perspectives, and a false natural-law (mountain) from the
 *   innovation-incentive narrative that obscures policy contingency.
 *
 * KEY AGENTS:
 *   - Insulin-Dependent Patients (Type 1 and insulin-dependent Type 2): Primary victims (powerless/trapped) — bear full biological and financial extraction; zero exit options
 *   - Uninsured and Fixed-Income Populations: Secondary victims (powerless/constrained-to-trapped) — face maximum out-of-pocket costs and are forced into life-threatening rationing
 *   - Insulin Manufacturers (Eli Lilly, Novo Nordisk, Sanofi): Primary beneficiaries (institutional/arbitrage) — set list prices, receive rebates, capture monopoly rents from captive market
 *   - Pharmacy Benefit Managers (Express Scripts, CVS Caremark, Optum): Secondary beneficiaries (institutional/arbitrage) — layer rebate spreads, negotiate with manufacturers, extract spread from payers and patients
 *   - Insurance Underwriters and Employers: Tertiary actors (powerful/constrained) — negotiate with PBMs, bear costs of formulary restrictions and prior authorization overhead
 *   - Patient Advocacy Coalition (ADA, JDRF, patient unions): Organized victims (organized/constrained) — coordinate collective action, have achieved limited price concessions, constrained by resource barriers to litigation and sustained legislative campaigns
 *   - Healthcare Workers (Physicians, Nurses, Diabetes Educators): Identity-locked suppressors (moderate/identity_locked) — structurally mobile but professionally captured to function within rationing constraints; bear emotional cost of denying optimal treatment
 *   - FDA Regulatory Framework: Institutional observer (institutional/constrained) — maintains separation between drug approval and pricing, lacks statutory price authority, serves performative role in creating appearance of oversight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insulin_pricing_cliff, 0.68).
domain_priors:suppression_score(insulin_pricing_cliff, 0.82).
domain_priors:theater_ratio(insulin_pricing_cliff, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insulin_pricing_cliff, extractiveness, 0.68).
narrative_ontology:constraint_metric(insulin_pricing_cliff, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(insulin_pricing_cliff, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insulin_pricing_cliff, snare).
narrative_ontology:human_readable(insulin_pricing_cliff, "Insulin Pricing Cliff: Market Capture and Extraction from Insulin-Dependent Patients").
narrative_ontology:topic_domain(insulin_pricing_cliff, "healthcare/pharmaceutical_pricing/metabolic_disease").

domain_priors:requires_active_enforcement(insulin_pricing_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insulin_pricing_cliff, insulin_manufacturers).
narrative_ontology:constraint_beneficiary(insulin_pricing_cliff, pharmacy_benefit_managers).
narrative_ontology:constraint_beneficiary(insulin_pricing_cliff, insurance_underwriters).
narrative_ontology:constraint_victim(insulin_pricing_cliff, type_one_diabetics).
narrative_ontology:constraint_victim(insulin_pricing_cliff, uninsured_insulin_dependent_patients).
narrative_ontology:constraint_victim(insulin_pricing_cliff, fixed_income_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSULIN-DEPENDENT PATIENT (SNARE) — Zero biological negotiating power. Insulin is not discretionary; missing doses causes DKA, coma, death. The patient cannot refuse, cannot switch, cannot ration effectively below therapeutic levels. Faces list prices of $300–400/vial, even with insurance. No exit exists except death or medical crisis. Maximum suppression and experienced extraction. The constraint's core target.
constraint_indexing:constraint_classification(insulin_pricing_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT ADVOCACY COALITION (TANGLED ROPE) — Organized victims (American Diabetes Association, JDRF, patient unions) have coordinated to demand price caps and transparency, creating some collective pressure. Benefits from coalition membership (information sharing, political leverage) but faces severe cost barriers: legal action against manufacturers costs millions, legislative campaigns require sustained funding. Suppression is high but not total; coalition has achieved some price concessions (Novo Nordisk price cuts in 2024, though temporary). Classification: tangled_rope — coordination function exists but enforcement asymmetry is extreme.
constraint_indexing:constraint_classification(insulin_pricing_cliff, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSULIN MANUFACTURERS (ROPE) — Eli Lilly, Novo Nordisk, Sanofi collectively control >90% of global insulin supply. They experience the constraint as a coordination mechanism: setting list prices creates formulary placement incentives, encouraging pharmacy benefit managers to promote their products over competitors. The manufacturers benefit from list price escalation without bearing costs — they receive rebates that more than offset discounting. They have exit options (geographic arbitrage, product differentiation, market segmentation). The constraint is experienced as valuable coordination, not extraction. Classification: rope — pure coordination with asymmetric benefit.
constraint_indexing:constraint_classification(insulin_pricing_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACY BENEFIT MANAGERS (ROPE) — Express Scripts, CVS Caremark, Optum manage prescription benefits for employers and insurers. They layer rebate structures and formulary tiers on top of manufacturers' list prices, negotiating rebates that create opaque pricing. PBMs benefit from list price escalation because higher rebates generate larger absolute dollar savings that they pocket as spread. They have significant exit options and operate as arbitrageurs between payers and manufacturers. Classification: rope — pure coordination with substantial benefit.
constraint_indexing:constraint_classification(insulin_pricing_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE UNDERWRITERS / EMPLOYER PLANS (TANGLED ROPE) — Large employers self-insure health benefits and negotiate with PBMs. They have genuine coordination function: managing risk across a population requires pooling mechanisms. But they also face extraction: PBM rebate structures are opaque, and the pass-through to patient out-of-pocket costs is not fully transparent. Employers experience moderate extraction — they benefit from cost-containment mechanisms but don't capture the full value of rebates. Classification: tangled_rope — coordination with partial extraction.
constraint_indexing:constraint_classification(insulin_pricing_cliff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FDA REGULATORY FRAMEWORK (PITON) — FDA regulates insulin as an approved pharmaceutical but has no explicit price-regulation authority in the US market (unlike most developed nations). The regulatory framework persists through institutional inertia — maintaining separation between drug safety approval and pricing policy — despite the framework's failure to prevent market capture. Theater ratio is moderate-to-high: FDA publishes requirements and guidance that appear to govern access, but lack pricing teeth. The regulatory apparatus has atrophied toward pure performative function. Classification: piton — degraded coordination mechanism maintained through institutional path dependence.
constraint_indexing:constraint_classification(insulin_pricing_cliff, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, pharmaceutical innovation requires profit incentives; manufacturers argue that high prices fund R&D for better insulins and new diabetes treatments. The natural-law framing positions insulin pricing as an inevitable trade-off: high prices enable innovation, ergo the pricing cliff is an unchangeable feature of market-driven pharmaceutical development. However, this perspective conflicts with structural data: competitors in Canada and the EU pay $30–50/vial; identical insulin molecule carries different price tags by geography. The 'natural law' framing obscures policy choices. The engine will flag this as a false summit candidate — the constraint is contingent on regulatory policy, not immutable.
constraint_indexing:constraint_classification(insulin_pricing_cliff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: HEALTHCARE WORKERS / CLINICAL RATIONING ROLE (SNARE) — Doctors, nurses, and diabetes educators are structurally mobile (could advocate for regulatory change, could refuse to participate in rationing) but identity-locked into their clinical role: they are trained to work within budget constraints, to optimize dosing within formulary limits, and to accept insurance denials as the 'system.' Their professional identity requires functioning within the constraint rather than challenging it. They experience the constraint as a snare because they bear the emotional cost of rationing (denying patients optimal treatment) while unable to exit their role. Theater ratio is high: clinical note-writing about insurance denials is performative documentation without power. Classification: snare with identity_locked exit, revealing cognitive capture of medical professionals by the extractive system.
constraint_indexing:constraint_classification(insulin_pricing_cliff, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insulin_pricing_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insulin_pricing_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insulin_pricing_cliff, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insulin_pricing_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insulin_pricing_cliff, TR),
    TR >= 0.70.

:- end_tests(insulin_pricing_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and escalating. The constraint's core extraction mechanism is the gap between manufacturing cost (approximately $6–12 per vial globally) and list prices ($300–400). The manufacturers capture the full spread through rebate structures with PBMs. Patients experience extraction through out-of-pocket costs and access barriers. The extractiveness value reflects the net transfer from trapped patients to manufacturer-PBM-insurer coalition. The trajectory shows acceleration: baseline 0.42 (mid-2010s) → 0.68 (present), driven by list price escalation and opacity of rebate pass-through. Suppression (0.82): Very high and structural. Patients cannot refuse insulin (biological trap), cannot substitute (insulin is monopoly product with no functionally equivalent alternatives for Type 1), cannot negotiate price (individual bargaining power is zero), and cannot exit (medical exit is death). Prior authorization requirements and formulary restrictions create institutional suppression layered on top of biological necessity. The trajectory shows intensification: baseline 0.64 (mid-2010s) → 0.82 (present), driven by PBM formulary tightening and prior authorization expansion. Theater Ratio (0.45): Moderate. The constraint has a functional core (managing disease through medication) but increasing performative content (prior authorizations that delay access without reducing cost, rebate disclosures that appear to address extraction without affecting patient prices, FDA guidance that does not govern pricing). Theater has increased from 0.35 → 0.45 as regulatory agencies and PBMs add compliance rituals without functional impact. Classification as Snare: Extractiveness 0.68 > 0.66 threshold, suppression 0.82 > 0.60 threshold, χ (effective extraction) will be >= 0.66 when computed against powerless/trapped agents. Snare criteria are met fully.
 *
 * PERSPECTIVAL GAP:
 *   The insulin pricing cliff demonstrates the widest perspectival gap in the corpus: the same structural phenomenon is experienced as pure extraction (snare) by patients, pure coordination (rope) by manufacturers and PBMs, mixed coordination-extraction (tangled rope) by patient advocates and employers, a degraded ritual (piton) by the FDA, identity-locked entrapment (snare) by healthcare workers, and a natural-law trade-off (mountain candidate) by innovation advocates. The gap between powerless/trapped (snare) and institutional/arbitrage (rope) perspectives is near-maximal: the beneficial-to-beneficiary agent experiences the constraint as valuable coordination enabling price-based formulary negotiation; the trapped-to-victim agent experiences the constraint as life-threatening extraction with no exit. The piton classification of the FDA reveals that regulatory governance has atrophied — the FDA publishes guidelines and maintains approval authority but lacks price-control mechanism, making its institutional role performative. The mountain classification is a false summit candidate: the innovation-incentive argument naturalizes a policy choice (high US prices) as inevitable trade-off for pharmaceutical R&D, but identical insulin molecules sold for $30–50/vial in Canada and EU demonstrate that price escalation is policy-contingent, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Patients: d = 0.95 (full target due to biological trap + victim status) → f(d) = 1.42 → high experienced extraction (χ approaches 1.0 at local scope). Manufacturers: d = 0.05 (full beneficiary + arbitrage exit) → f(d) = -0.12 → negative experienced extraction (they experience the constraint as net benefit through pricing power). PBMs: d = 0.10 (beneficiary + arbitrage) → f(d) ≈ -0.05 → slight negative experience (rebate spreads exceed their operational costs). Patient advocates: d = 0.70 (victim + constrained exit from coalition membership costs) → f(d) ≈ 1.05 → moderate-high extraction despite organized power. Healthcare workers: d = 0.80 (victim + identity_locked exit; they bear emotional cost of rationing without ability to exit their clinical role) → f(d) ≈ 1.20 → high extraction despite moderate power atom. The directionality computations explain why beneficiaries see rope (coordination mechanism for negotiation) while victims see snare (inescapable extraction): they occupy opposite ends of the d spectrum, and the sigmoid amplifies the difference.
 *
 * MANDATROPHY ANALYSIS:
 *   The insulin pricing cliff resolves mandatrophy by revealing that the snare classification is stable across multiple victim perspectives (powerless/trapped, organized/constrained, moderate/identity_locked) and multiple beneficiary perspectives (institutional/arbitrage) produce the same rope classification. The tangled rope classification (patient advocates, employer plans) occupies a structural middle ground where both genuine coordination functions and asymmetric extraction are present. The piton and mountain perspectives are analytically interesting but do not undermine the core snare diagnosis: they are higher-scope and longer-horizon perspectives that reframe the constraint as institutional inertia or innovation trade-off, but do not change the immediate-biographical-local lived experience of the snare from the patient perspective. The mandatrophy is resolved by accepting all six classifications as structurally valid readings of different observational contexts, with the snare at the powerless/trapped/biographical/local context being the most ethically salient and the most structurally invariant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    list_price_vs_net_price_asymmetry,
    'What proportion of insulin ''extraction'' derives from patient out-of-pocket costs versus hidden manufacturer-PBM rebate asymmetries that don''t benefit patients?',
    'Full transparency of manufacturer rebate structures, PBM spread data, and employer plan pass-through rates. Longitudinal comparison of patient OOP cost trajectories against aggregate rebate accumulation.',
    'If patient OOP costs track list price escalation (low rebate pass-through): patients bear full extraction. If rebates exceed patient OOP savings (high PBM spread): patients experience extraction despite nominal rebate programs. Magnitude determines whether to classify unsured patients as additional victim group.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(list_price_vs_net_price_asymmetry, empirical, 'Allocation of extraction between patient costs and hidden rebate structures').

omega_variable(
    innovation_incentive_necessity,
    'Does the pharmaceutical industry''s claimed innovation incentive—that high insulin prices fund better treatments—reflect empirical R&D productivity, or rationalize extraction?',
    'Historical correlation analysis: compare R&D spending to new therapy launches across pre-price-escalation (1996–2008) vs post-escalation (2009–2026) periods. Map manufacturing profit margins to new molecule development timelines. Compare US innovation output to EU/Canadian markets with strict price regulation.',
    'If innovation productivity correlates with price escalation: natural-law framing has merit; classify mountain candidates as legitimate. If innovation productivity is uncorrelated or negative: the innovation rationale is theater; classify mountain candidates as false summits with clear malice. If EU strict regulation produces equivalent innovation: regulatory policy choice is evident; constraint is policy-contingent, not immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_incentive_necessity, empirical, 'Whether high insulin prices empirically drive innovation').

omega_variable(
    patient_demographic_targeting,
    'Is the pricing cliff''s suppression mechanism uniformly distributed across diabetes populations, or targeted toward those with lowest collective bargaining power (rural, uninsured, poor, elderly)?',
    'Geographic analysis of insulin access by insurance status, income, and population density. Temporal tracking of formulary restrictions imposed on uninsured populations vs insured populations. Comparative pricing analysis for identical insulin across insured/uninsured distribution channels.',
    'If suppression is uniform: constraint is systemic market failure. If suppression is targeted: constraint exhibits intentional discrimination, escalating from snare to engineered predation. Victim group taxonomy must expand to include specific demographics bearing maximum cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_demographic_targeting, empirical, 'Whether pricing suppression targets specific vulnerable demographics').

omega_variable(
    regulatory_capture_mechanism,
    'Does the FDA''s separation of price regulation from drug approval reflect principled policy design, or regulatory capture by manufacturers who benefit from price opacity?',
    'Historical analysis of FDA legislative authority vs statutory constraints imposed by pharmacy benefit manager exemptions and state-level regulatory preemption. Comparison of FDA transparency disclosures versus manufacturer price-setting authority. Tracking of manufacturer lobbying spend against regulatory restrictiveness.',
    'If principled design: regulatory framework is appropriately constrained and separate; classify FDA as piton (atrophied coordination mechanism). If regulatory capture: the piton is actually a snare from the FDA''s perspective — manufacturers have trapped regulators into non-enforcement. The FDA perspective may reclassify to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether FDA regulatory structure reflects capture by manufacturers').

omega_variable(
    identity_lock_persistence_after_exit,
    'When healthcare workers exit the clinical rationing role, does their identity-lock persist (continuing to internalize constraints) or dissolve (revealing the lock was institutional rather than intrinsic)?',
    'Qualitative interviews with retired physicians, nurses, and diabetes educators who left clinical practice. Tracking whether post-exit advocacy positions change relative to their clinical-period positions. Comparison of regulatory testimony by current vs retired healthcare professionals.',
    'If identity lock persists: the suppression mechanism is deeply internalized, and healthcare workers are not recoverable advocates for price reform. If lock dissolves: healthcare workers are constrained rather than identity-locked; they have latent advocacy capacity that exit reveals. Distinction affects strategy for mobilizing healthcare worker constituencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_after_exit, empirical, 'Whether healthcare worker identity lock persists after role exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insulin_pricing_cliff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insulin_tr_t0, insulin_pricing_cliff, theater_ratio, 0, 0.35).
narrative_ontology:measurement(insulin_tr_t5, insulin_pricing_cliff, theater_ratio, 5, 0.4).
narrative_ontology:measurement(insulin_tr_t10, insulin_pricing_cliff, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(insulin_be_t0, insulin_pricing_cliff, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(insulin_be_t5, insulin_pricing_cliff, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(insulin_be_t10, insulin_pricing_cliff, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(insulin_su_t0, insulin_pricing_cliff, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(insulin_su_t5, insulin_pricing_cliff, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(insulin_su_t10, insulin_pricing_cliff, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insulin_pricing_cliff, resource_allocation).
narrative_ontology:affects_constraint(insulin_pricing_cliff, prior_authorization_enforcement).
narrative_ontology:affects_constraint(insulin_pricing_cliff, pbm_rebate_opacity).
narrative_ontology:affects_constraint(insulin_pricing_cliff, insulin_access_rationing).

% DUAL FORMULATION NOTE:
% The insulin pricing cliff is a composite constraint family with separable mechanisms. The pricing cliff itself (manufacturer list price escalation) has ε=0.68. The prior authorization mechanism (insurance gatekeeping) has ε=0.55 (tangled rope—has access-control coordination function but extracts time and medical risk). The PBM rebate structure (opacity of spread extraction) has ε=0.62 (snare—pure opacity-based extraction). Each story gets its own ε and classification; they are linked by network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
