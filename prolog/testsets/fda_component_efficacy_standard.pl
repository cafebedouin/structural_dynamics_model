% ============================================================================
% CONSTRAINT STORY: fda_component_efficacy_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_component_efficacy_standard, []).

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
 *   constraint_id: fda_component_efficacy_standard
 *   human_readable: FDA's Component-Level Efficacy Standard for Combination Vaccines
 *   domain: regulatory/pharmaceutical
 *
 * SUMMARY:
 *   The FDA's requirement that combination vaccines demonstrate
 *   component-level efficacy creates a structural tension between regulatory
 *   safety assurance and innovation incentives. The standard emerged in the
 *   1980s when combination vaccines were additive (e.g., DPT: diphtheria,
 *   pertussis, tetanus as independent threats). As vaccine technology evolved
 *   toward synergistic combinations (immune-enhancing adjuvants,
 *   cross-reactive epitopes, temporal sequencing effects), the
 *   component-level requirement became increasingly disconnected from how
 *   combination vaccines actually work. This constraint exhibits the core
 *   mandatrophy: it simultaneously solves a genuine coordination problem
 *   (ensuring component safety through rigorous testing) and extracts from
 *   potential innovators (by raising regulatory cost of novel combinations).
 *   The extractiveness has grown over 20 years as the gap between regulatory
 *   testing model and biological reality widened, reflected in rising
 *   theater_ratio as developers spend more effort demonstrating independent
 *   component performance in formats that do not reflect real-world use.
 *
 * KEY AGENTS:
 *   - Novel Vaccine Developers: Primary victims (powerless/trapped) — biotech startups and academic labs pursuing innovative combinations face prohibitive regulatory costs and cannot exit the FDA approval pathway
 *   - Incumbent Manufacturers: Primary beneficiaries (institutional/arbitrage) — Merck, Pfizer, GSK benefit from barrier to entry; have pre-existing component data and distributed compliance infrastructure
 *   - FDA's Regulatory Division: Secondary beneficiary (institutional/arbitrage) — maintains standard as institutional practice; benefits from clear approval criteria and reduced decision variance
 *   - Public Health Agencies (CDC, State Health): Secondary victims (organized/constrained) — benefit from predictable efficacy profiles but constrained in ability to innovate toward emerging threats or request rapid novel combinations
 *   - International Development Organizations (WHO, GAVI): Constrained but with exit path (organized/constrained) — subject to FDA standard de facto but building alternative approval pathways (WHO EUL, regional prequalification) with sunset logic
 *   - FDA's Own Evaluation Process: Institutional maintainer (institutional/arbitrage) — perpetuates component-level standard through inertia despite recognition that synergy is the operative mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_component_efficacy_standard, 0.38).
domain_priors:suppression_score(fda_component_efficacy_standard, 0.52).
domain_priors:theater_ratio(fda_component_efficacy_standard, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_component_efficacy_standard, extractiveness, 0.38).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fda_component_efficacy_standard, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_component_efficacy_standard, tangled_rope).
narrative_ontology:human_readable(fda_component_efficacy_standard, "FDA's Component-Level Efficacy Standard for Combination Vaccines").
narrative_ontology:topic_domain(fda_component_efficacy_standard, "regulatory/pharmaceutical").

domain_priors:requires_active_enforcement(fda_component_efficacy_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_component_efficacy_standard, incumbent_vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(fda_component_efficacy_standard, fda_regulatory_capacity).
narrative_ontology:constraint_victim(fda_component_efficacy_standard, vaccine_innovation_pipeline).
narrative_ontology:constraint_victim(fda_component_efficacy_standard, public_health_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL VACCINE DEVELOPERS (SNARE) — Small biotech firms and academic developers pursuing innovative combination vaccines face prohibitive regulatory costs and approval timelines. Each component must demonstrate independent efficacy even when the clinical benefit derives from synergy. Cannot exit: FDA approval is mandatory for market access. Bears full cost of the standard without benefit of incumbency or distributed compliance infrastructure.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT MANUFACTURERS (ROPE) — Large pharmaceutical firms with established combination vaccines benefit from the standard as a barrier to entry. They have pre-existing efficacy data for individual components, distributed compliance infrastructure, and regulatory relationships. The standard functions as coordination: it clarifies approval pathways and validates their existing portfolio. Effective arbitrage through regulatory precedent.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AGENCIES (TANGLED ROPE) — CDC and state health departments benefit from predictable vaccine efficacy profiles (coordination function) but are constrained by inability to approve vaccines faster than FDA allows or to pressure innovation toward novel combinations addressing emerging threats. The standard both enables disease surveillance consistency and prevents rapid response to outbreak-specific needs. Mixed: genuine coordination function with asymmetric extraction.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL DEVELOPMENT ORGS (SCAFFOLD) — WHO, GAVI, and development agencies face the FDA standard as a temporary constraint with a sunset. Alternative pathways (WHO Emergency Use Listing, prequalification procedures) are maturing as independent verification systems. The standard's extraction power is declining as decentralized efficacy evaluation capabilities build in middle-income countries. Constrained exit in the short term; genuine exit emerging over 10-15 years.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FDA EVALUATION PROCESS (PITON) — The component-level standard is substantially performative: it requires generating efficacy data for components that may never be tested independently in clinical practice. The agency maintains this ritual through institutional inertia (it is how efficacy has always been demonstrated) despite growing recognition that combination efficacy depends on synergy, not isolated component performance. Theater ratio 0.65 reflects that much of the approval discussion concerns regulatory format compliance rather than clinical benefit assessment.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilization-scale view, the standard solves a genuine coordination problem (ensuring vaccine safety through rigorous component testing) while extracting from innovation (by raising the regulatory cost of novel combinations). The constraint is neither pure coordination nor pure extraction, but a hybrid. The beneficiaries (incumbent firms, FDA capacity) and victims (novel developers, public health flexibility) are clearly distinguished by structural position.
constraint_indexing:constraint_classification(fda_component_efficacy_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_component_efficacy_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_component_efficacy_standard, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fda_component_efficacy_standard, TR),
    TR >= 0.70.

:- end_tests(fda_component_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The standard creates measurable barriers to market entry (increased clinical trial costs, extended timelines) but does not prevent novel combinations entirely — developers with sufficient capital can meet requirements. The extractiveness is higher than pure coordination (0.15) because the burden falls disproportionately on smaller firms and academic researchers, but lower than Snare territory (0.46+) because large incumbents have pathways through existing component data. Suppression (0.52): Moderate. Significant barriers to exit include: (1) FDA approval is mandatory for US market access, (2) no alternative US regulatory pathway for vaccine combinations, (3) clinical trial costs prohibitive for non-capital-backed researchers, (4) publication bias against negative efficacy trials. However, suppression is not total — international pathways exist (WHO EUL, European approval), allowing partial exit. Theater ratio (0.65): High and increasing. Over 20 years, the theater ratio rose from 0.45 to 0.65 as complexity of combination vaccines grew beyond the regulatory testing model. Developers increasingly conduct component-level trials that do not reflect real-world use, generating performative efficacy data required for approval but not informative for clinical practice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a clean beneficiary/victim split aligned with firm incumbency. Incumbent firms see coordination (Rope); novel developers see extraction (Snare). The gap arises from asymmetric access to compliance infrastructure, not from disagreement about the standard's function. The standard's function is genuinely to coordinate safety testing (true coordination benefit). But the distribution of compliance cost is asymmetric: those with pre-existing component data and regulatory relationships experience low cost; those without face prohibitive barriers. This is the core mandatrophy pattern: a legitimate coordination mechanism layered with extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values: Incumbent manufacturers (beneficiary + arbitrage) derive low d (~0.15) → negative f(d) → negative contribution to χ (they experience the constraint as enabling). Novel developers (victim + trapped) derive high d (~0.95) → high f(d) (~1.42) → maximum contribution to χ (they experience maximum extractiveness). Public Health Agencies (victim + constrained) derive intermediate d (~0.60-0.75) → moderate f(d) (~0.85-1.10) → moderate χ. The analytical perspective (d ~0.72, f(d) ~1.15) sees the full structure: coordination function for safety testing + asymmetric distribution of compliance costs. The directionality derivation makes manifest what narrative alone obscures: the standard extracts from those outside the incumbent coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is correctly classified as Tangled Rope because it satisfies both gates: (1) genuine coordination function — the standard ensures component safety through rigorous testing, reducing epistemic uncertainty about combination vaccine safety profiles; (2) asymmetric extraction — the compliance cost is borne disproportionately by novel developers while benefits (clear regulatory pathway, competitive advantage through incumbency) flow to large manufacturers. The beneficiary/victim declarations are structurally precise: 'incumbent_vaccine_manufacturers' benefit from barrier to entry; 'vaccine_innovation_pipeline' and 'public_health_flexibility' bear costs. The theater ratio (0.65) is high but not dominant (piton threshold is 0.70), reflecting that while developers conduct performative component trials, the underlying regulatory process does involve genuine efficacy assessment. The constraint avoids false classification as pure Snare (which would require suppression ≥0.60 and victims with no beneficiary element) and as pure Rope (which would require low suppression and symmetric burden distribution). Tangled Rope captures the hybrid: coordination function (safety) + extraction mechanism (barrier to entry).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synergy_versus_component_equivalence,
    'Does vaccine efficacy depend primarily on individual component performance or on synergistic interactions between components?',
    'Comparative immunogenicity studies: vaccines with identical components in different configurations; mechanistic studies of epitope mapping and immune response sequencing; post-market surveillance correlation between predicted and observed efficacy',
    'If component-dominant: FDA standard is justified and extractiveness is overstated (~0.25). If synergy-dominant: the standard is substantially theater and extractiveness is understated (~0.55).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synergy_versus_component_equivalence, empirical, 'Whether vaccine efficacy depends on component performance or synergy').

omega_variable(
    regulatory_cost_barrier_magnitude,
    'How much additional development cost and timeline delay does the component-level standard impose on novel combination vaccines compared to single-component vaccines or to WHO EUL pathway?',
    'Historical cost analysis of approved vaccines; developer surveys; comparative timeline analysis (FDA vs WHO EUL); phase timing analysis for component-level approval vs integrated efficacy trials',
    'If marginal cost <$5M and <1 year: suppression is overstated. If cost >$50M and >3 years for novel developers: suppression coefficient and extractiveness confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_cost_barrier_magnitude, empirical, 'Cost and timeline barrier imposed by component-level standard').

omega_variable(
    innovation_pipeline_deflection,
    'Are novel vaccine combinations being abandoned or redirected to non-US markets due to FDA component-level requirements?',
    'Patent analysis: novel combination vaccines filed but abandoned in US jurisdiction; developer interviews; market launch geography analysis (which vaccines were launched in EU/WHO approval first); development pipeline stage-out analysis',
    'If significant deflection (>20% of novel combinations): victim group ''vaccine innovation pipeline'' is verified as structurally constrained. If minimal deflection: the standard is less extractive than assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_pipeline_deflection, empirical, 'Whether novel vaccine combinations are being abandoned due to FDA requirements').

omega_variable(
    public_health_response_cost,
    'What is the measurable cost to outbreak response capacity when FDA approval timeline prevents rapid deployment of novel combination vaccines matched to emerging pathogen profiles?',
    'Retrospective analysis of outbreak response scenarios; modeling of counterfactual vaccine availability; case studies (COVID-era variant boosters, mpox response timing); expert panel assessment of scenario costs',
    'If costs are substantial and measurable: public health agencies'' victim status is confirmed. If costs are negligible or outweighed by safety benefits: the standard''s extraction from public health is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_health_response_cost, empirical, 'Cost to public health response from FDA approval timeline constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_component_efficacy_standard, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fda_vax_tr_t0, fda_component_efficacy_standard, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fda_vax_tr_t10, fda_component_efficacy_standard, theater_ratio, 10, 0.58).
narrative_ontology:measurement(fda_vax_tr_t20, fda_component_efficacy_standard, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(fda_vax_be_t0, fda_component_efficacy_standard, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fda_vax_be_t10, fda_component_efficacy_standard, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(fda_vax_be_t20, fda_component_efficacy_standard, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_component_efficacy_standard, enforcement_mechanism).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, combination_vaccine_innovation_incentive).
narrative_ontology:affects_constraint(fda_component_efficacy_standard, pandemic_rapid_response_capability).

% DUAL FORMULATION NOTE:
% The component-level efficacy standard is structurally upstream of constraints on rapid pandemic response and vaccine innovation pipeline. The standard's extractiveness directly constrains downstream actors' ability to innovate or respond rapidly. Each downstream constraint has its own extractiveness value reflecting its specific structural position; this constraint provides the regulatory enforcement mechanism for both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
