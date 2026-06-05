% ============================================================================
% CONSTRAINT STORY: oral_glp1_market_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oral_glp1_market_access, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: oral_glp1_market_access
 *   human_readable: Patent-Protected Market for Oral GLP-1 Agonists
 *   domain: economic/technological/healthcare
 *
 * SUMMARY:
 *   The patent-protected market for oral GLP-1 agonists (exemplified by oral
 *   semaglutide) represents a complex structural constraint combining genuine
 *   pharmaceutical innovation coordination with significant patient-level
 *   extraction. The underlying drug discovery (GLP-1 receptor agonism for
 *   weight loss) is real and valuable — decades of academic research plus
 *   substantial private R&D investment enabled the oral formulation. However,
 *   patent protection enables pricing that ranges from justified innovation
 *   incentive to rent extraction depending on the observer's structural
 *   position. This constraint exhibits the full range of DR types: uninsured
 *   patients experience pure snare (trapped, no access); insured patients
 *   experience tangled rope (coordination benefit offset by cost-sharing and
 *   access restrictions); the manufacturer experiences rope (legitimate
 *   innovation reward); generic manufacturers experience snare (legal
 *   foreclosure); healthcare payers experience tangled rope (negotiation
 *   leverage constrained by patent enforcement); organized generic/biosimilar
 *   pathways experience scaffold (sunset logic via patent expiration);
 *   physicians experience rope (effective therapeutic tool); the patent
 *   system itself exhibits piton characteristics (institutional inertia
 *   maintaining exclusivity beyond optimal innovation incentive). The theater
 *   ratio is low (0.42) — this constraint relies primarily on legal
 *   enforcement (patent law) rather than performative legitimacy,
 *   distinguishing it from constraints like peer review or traditional
 *   banking secrecy. Extractiveness has risen from 0.35 to 0.58 over 8 years
 *   as the drug gained market dominance and insurance coverage expanded,
 *   revealing that extraction intensity correlates with market penetration
 *   and public demand rather than production complexity.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — captures pricing power during patent term; coordinates drug development and distribution
 *   - Uninsured Patients: Primary victim (powerless/trapped) — face list prices of $1,000-1,500/month with no coverage or negotiation pathway
 *   - Insured Patients: Secondary victim (moderate/constrained) — benefit from treatment access but constrained by formulary restrictions and cost-sharing
 *   - Healthcare Payers: Organized beneficiary and victim (organized/constrained) — negotiate discounts but constrained by patent enforcement and public demand
 *   - Generic Manufacturers: Victim through foreclosure (moderate/trapped) — have technical capability to produce identical drug but are legally barred by patent protection
 *   - Prescribing Physicians: Beneficiary through therapeutic access (powerful/mobile) — coordinate treatment decisions with minimal extraction burden
 *   - Patent System: Institutional mediator (institutional/arbitrage) — enforces exclusivity; exhibits piton characteristics (maintenance through inertia)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oral_glp1_market_access, 0.58).
domain_priors:suppression_score(oral_glp1_market_access, 0.68).
domain_priors:theater_ratio(oral_glp1_market_access, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oral_glp1_market_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(oral_glp1_market_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(oral_glp1_market_access, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oral_glp1_market_access, tangled_rope).
narrative_ontology:human_readable(oral_glp1_market_access, "Patent-Protected Market for Oral GLP-1 Agonists").
narrative_ontology:topic_domain(oral_glp1_market_access, "economic/technological/healthcare").

domain_priors:requires_active_enforcement(oral_glp1_market_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, pharmaceutical_patent_holder).
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, prescribing_physicians).
narrative_ontology:constraint_beneficiary(oral_glp1_market_access, patients_with_coverage).
narrative_ontology:constraint_victim(oral_glp1_market_access, cost_bearing_patients).
narrative_ontology:constraint_victim(oral_glp1_market_access, healthcare_payers).
narrative_ontology:constraint_victim(oral_glp1_market_access, generic_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — Faces list prices of $1,000-1,500/month with no coverage pathway. Cannot access the effective treatment, cannot negotiate, cannot exit. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81. Pure extraction: trapped paying customer facing maximum extraction.
constraint_indexing:constraint_classification(oral_glp1_market_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED PATIENT (TANGLED ROPE) — Benefits from access to effective treatment; coordination function exists (insurance risk-pooling enables treatment). But constrained by formulary restrictions, prior authorization, high copays. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.48. Mixed: genuine coordination benefit offset by extraction via cost-sharing.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Patent protection enables price-setting, but also solves genuine coordination problem: funding R&D for rare/difficult diseases requires margin capture. Experiences constraint as coordination mechanism and legitimate property right. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary through arbitrage exit (can divest, license, sell to other manufacturers).
constraint_indexing:constraint_classification(oral_glp1_market_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE PAYERS (TANGLED ROPE) — Organized agents (Medicare, Medicaid, commercial insurers) benefit from formulary leverage negotiation but are constrained by patent enforcement and public demand for access. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.44. Hybrid: genuine negotiation coordination offset by extraction through brand premium pricing.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENERIC MANUFACTURERS (SNARE) — Patent exclusivity (typically 20 years from filing) prevents market entry despite capability to produce identical drug at 10-20% of brand price. Trapped by legal barrier; extraction mechanism is foreclosure. d≈0.87, f(d)≈1.25, σ=1.0 → χ≈0.73. Strong snare: economic capability exists but market access is prohibited.
constraint_indexing:constraint_classification(oral_glp1_market_access, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: GENERIC PATHWAY COALITION (SCAFFOLD) — Organized actors (FDA generic division, international generic manufacturers, health equity advocates) are building parallel pathways: biosimilar approvals, patent challenge mechanisms (Paragraph IV), international generic access in non-patent-enforcing jurisdictions. These pathways have sunset logic — they will erode patent value after exclusivity expires or via legal challenge. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.12. Low effective extraction; coalition has clear agency and structural exit path.
constraint_indexing:constraint_classification(oral_glp1_market_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PRESCRIBING PHYSICIANS (ROPE) — Benefit from effective therapeutic tool and manufacturer-funded education; coordinate treatment access. Powerful + mobile: can recommend alternative treatments, can discuss costs, can prescribe generics when available. d≈0.25, f(d)≈0.18, σ=1.0 → χ≈0.10. Low extraction: genuine coordination benefit with real exit options.
constraint_indexing:constraint_classification(oral_glp1_market_access, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: PATENT SYSTEM (PITON) — Patent protection persists through institutional inertia despite degraded primary function (incentivizing innovation is real; excluding generics after peak development cost recovery is rent extraction, not innovation incentive). theater_ratio=0.55 (moderate). The patent exclusivity ritual is maintained but increasingly challenged by price controls, parallel imports, and compulsory licensing in developing countries. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.03. Piton because theatrical component (term length) exceeds functional component.
constraint_indexing:constraint_classification(oral_glp1_market_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view reveals genuine tension: patent protection does fund pharmaceutical R&D (coordination function), but pricing power extracts from patients (asymmetric extraction). ε=0.58 reflects this hybrid nature. Neither pure coordination (rope) nor pure extraction (snare) captures the structural duality. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.75. Analytical classification confirms tangled_rope as the primary type.
constraint_indexing:constraint_classification(oral_glp1_market_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oral_glp1_market_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oral_glp1_market_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oral_glp1_market_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(oral_glp1_market_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(oral_glp1_market_access, TR),
    TR >= 0.70.

:- end_tests(oral_glp1_market_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, rising over interval. The baseline extraction (0.35) reflects legitimate R&D cost recovery — oral formulation development required substantial investment. The rise to 0.58 reflects that pricing power now exceeds what innovation incentive justifies: the drug has passed peak development cost recovery (typically 5-7 years post-launch), yet prices remain elevated due to market dominance and insurance coverage expansion. The extractiveness is not as high as a pure rent-extraction constraint (which would be 0.70+) because some margin still funds legitimate R&D for follow-on indications and next-generation formulations. Suppression (0.68): Moderately high. Barriers to alternative treatment access include patent protection (legal suppression), high cost (economic suppression), insurance formulary restrictions (bureaucratic suppression), and physician prescribing inertia. However, suppression is not maximal because generic pathway alternatives exist (Paragraph IV challenges, biosimilar approvals, patent expiration in 8-12 years). Theater ratio (0.42): Low. Patent enforcement is primarily legal/structural, not performative. The constraint does not require theatrical legitimacy — it operates through property law, not through cultural narrative or ritual. The modest theater component reflects marketing/education activities by the manufacturer but these are secondary to the patent enforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces the full spectrum of types because the structural extraction is asymmetric across power levels. The powerless uninsured patient sees snare: trapped by cost and legal barriers with no exit. The moderate insured patient sees tangled rope: genuine coordination benefit (insurance risk-pooling enables access) offset by extraction (cost-sharing, access restrictions). The institutional manufacturer sees rope: legitimate property right and innovation reward; coordination function is real (patent incentivizes drug development). The organized healthcare payers see tangled rope: negotiation leverage constrained by patent enforcement; some patients benefit, others are excluded. Generic manufacturers see snare: pure legal foreclosure despite technical capability. The analytical observer sees tangled rope: genuine innovation coordination tension with patient access extraction. The perspectival gap is driven by power differences (institutional actors can exit or arbitrage; powerless actors cannot) and structural position (beneficiary vs victim of patent enforcement). No perspective produces rope + rope or snare + snare — the structural asymmetry forces distributed classification across the type spectrum.
 *
 * DIRECTIONALITY LOGIC:
 *   Uninsured patient: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Insured patient: Victim + constrained → d≈0.62, f(d)≈0.82. Significant extraction but not trapped (can switch to generics, alternative therapies if available). Manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can divest, license, or face price controls without collapse. Healthcare payers: Victim + constrained + beneficiary (dual role) → d≈0.58, f(d)≈0.75. Can negotiate discounts but constrained by patent enforcement and public demand. Generic manufacturers: Victim + trapped → d≈0.87, f(d)≈1.25. Legal barrier is more durable than economic barrier. Physicians: Beneficiary + mobile → d≈0.25, f(d)≈0.18. Real therapeutic benefit; can recommend alternatives, discuss costs. Patent system: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification (degraded function) rather than snare because the institutional structure has agency (Congress could reform patent terms; courts could restrict pharmaceutical patents).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by separating innovation coordination (rope function) from patient access extraction (snare function). The manufacturer's rope perspective is legitimate: patent protection does fund R&D, and the innovation-incentive mechanism is real. The uninsured patient's snare perspective is also legitimate: pricing power at current levels exceeds what innovation justifies, and cost barriers prevent access despite technical feasibility of supply. The constraint is NOT mislabeled as pure extraction (which would erase the innovation coordination function), nor is it pure coordination (which would erase the patient-level extraction). The tangled_rope classification at the analytical level captures both: the constraint has genuine coordination function (pharmaceutical innovation) AND asymmetric extraction (patient access barriers). The mandatrophy is resolved by recognizing that both are structurally true: the same mechanism (patent protection) enables both innovation coordination AND patient extraction. The task for policy is not to choose which is 'real' but to calibrate the mechanism to maximize the coordination function while minimizing the extraction — e.g., via shorter patent terms for secondary indications, price controls in certain jurisdictions, or mandatory licensing triggers. The current level (ε=0.58) suggests the extraction is growing beyond the innovation-incentive justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_threshold,
    'What patent term length and margin level is necessary to fund breakthrough pharmaceutical R&D vs what level constitutes rent extraction unrelated to innovation incentives?',
    'Comparative analysis of R&D pipeline economics; correlation between patent term/margin and R&D spend by therapeutic area; case studies of drugs developed under different incentive regimes',
    'If necessary margin is <30% above generic production cost: current pricing is rent extraction (snare from all perspectives). If necessary margin is >50%: current pricing may be justified (rope from analyst perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_threshold, empirical, 'R&D funding threshold for innovation incentives').

omega_variable(
    global_price_arbitrage_sustainability,
    'Can international price discrimination (low-income countries pay 10-15% of US price) be sustained indefinitely or does it erode patent value through re-importation and gray markets?',
    'Market analysis of parallel import flows; tracking of generic penetration in high-price vs low-price regions; assessment of manufacturer enforcement costs',
    'If unsustainable: patent protection is structurally weakening (scaffold sunset logic confirmed). If sustainable: price discrimination mitigates extraction harm globally (snare perspective weakened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_price_arbitrage_sustainability, empirical, 'Sustainability of global price arbitrage under patent protection').

omega_variable(
    biosimilar_pathway_efficacy,
    'Will FDA/EMA biosimilar approval pathways for GLP-1 agonists create genuine price competition before patent expiration or do regulatory barriers maintain effective monopoly?',
    'Tracking biosimilar approvals; price measurement post-approval; market share analysis; manufacturing capacity assessment',
    'If biosimilars achieve >20% market share pre-expiration: scaffold pathway is functioning, generic manufacturer snare is partially escaped. If <5% market share: regulatory barriers maintain snare structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biosimilar_pathway_efficacy, empirical, 'Efficacy of biosimilar pathway in creating price competition').

omega_variable(
    public_manufacturing_feasibility,
    'Could government-operated or nonprofit manufacturing facilities produce oral GLP-1 agonists at 20-30% of current prices while maintaining sufficient quality and supply?',
    'Cost analysis of manufacturing facilities; production capacity modeling; quality control benchmarking; case studies of existing public/nonprofit drug manufacturing',
    'If feasible: structural alternative exists (institutional-level snare escape pathway). If not feasible: extraction mechanism is tied to genuine production complexity (snare is more defensible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_manufacturing_feasibility, empirical, 'Feasibility of public pharmaceutical manufacturing as snare escape').

omega_variable(
    patent_challenge_success_rates,
    'What is the empirical success rate of Paragraph IV patent challenges for pharmaceutical patents, and how much do successful challenges accelerate generic entry?',
    'FDA/PTAB case law analysis; timeline from challenge filing to generic approval; market share post-generic-entry; cost of litigation',
    'If success rate >40% and generic entry <3 years post-challenge: scaffold pathway is real and accessible. If <20% success rate or >5 years to entry: pathway is theoretical, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_challenge_success_rates, empirical, 'Patent challenge pathway efficacy and timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oral_glp1_market_access, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oral_tr_t0, oral_glp1_market_access, theater_ratio, 0, 0.38).
narrative_ontology:measurement(oral_tr_t4, oral_glp1_market_access, theater_ratio, 4, 0.4).
narrative_ontology:measurement(oral_tr_t8, oral_glp1_market_access, theater_ratio, 8, 0.42).

% Extraction over time
narrative_ontology:measurement(oral_be_t0, oral_glp1_market_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oral_be_t4, oral_glp1_market_access, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(oral_be_t8, oral_glp1_market_access, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oral_glp1_market_access, resource_allocation).
narrative_ontology:affects_constraint(oral_glp1_market_access, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(oral_glp1_market_access, generic_drug_market_entry).
narrative_ontology:affects_constraint(oral_glp1_market_access, healthcare_insurance_formulary_control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(oral_glp1_market_access, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
