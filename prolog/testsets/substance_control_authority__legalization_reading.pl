% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint models the legalization reading of the substance control
 *   authority kernel: the state establishes a regulatory framework to permit
 *   drug production and consumption as legal commerce, subject to
 *   quality/dosage/potency/distribution controls. Users exit both criminal
 *   and unregulated-supply victim sets; illegal suppliers are structurally
 *   excluded; third parties are protected through market regulation rather
 *   than criminalization. The reading is distinct from prohibition (which
 *   criminalizes use) and harm reduction (which accepts use while minimizing
 *   harms without legalization). This story instantiates ONLY the
 *   legalization reading's structure and costs—it does not describe the
 *   siblings, does not average across readings, and does not claim to model
 *   all three readings at once. The kernel contest sits in omega variables
 *   and cs_structure.
 *
 * KEY AGENTS:
 *   - users_exiting_criminal_markets: primary beneficiary — access regulated substance without criminal penalty
 *   - regulated_producers: agenda-setter — licensed legal manufacture and distribution under state control
 *   - regulatory_apparatus: institutional agenda-setter and payer — administers licensing, product standards, enforcement
 *   - illegal_market_suppliers: excluded — structurally barred from legitimate market by regulatory barriers
 *   - public_health_authorities: beneficiary — data, funding, intervention capacity
 *   - law_enforcement: payer (reallocation) — shift from use-focused to compliance-focused enforcement
 *   - treatment_systems: beneficiary — tax revenue and data access, but capacity pressure from potential use increase
 *   - potential_new_users: excluded — not represented in legalization design, health risk externalized
 *   - adjacent_communities: beneficiary — reduced street-level trade, but visibility of regulated retail
 *   - prohibition-maintaining jurisdictions: observer — see spillover effects, pressure on their own policies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.28).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '1825aef8-94f5-41a1-8826-c5f1e8089311').
narrative_ontology:cs_kernel_codification('1825aef8-94f5-41a1-8826-c5f1e8089311', formalized).
narrative_ontology:cs_authority_grounding('1825aef8-94f5-41a1-8826-c5f1e8089311', lineage).
narrative_ontology:cs_interpretation_layer_present('1825aef8-94f5-41a1-8826-c5f1e8089311').
narrative_ontology:cs_reading_relation('1825aef8-94f5-41a1-8826-c5f1e8089311', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('1825aef8-94f5-41a1-8826-c5f1e8089311', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('1825aef8-94f5-41a1-8826-c5f1e8089311', foundational, drug_use_accepted_and_regulated).
narrative_ontology:cs_axiom_status(drug_use_accepted_and_regulated, holdable).
narrative_ontology:cs_axiom_grounding('1825aef8-94f5-41a1-8826-c5f1e8089311', drug_use_accepted_and_regulated, deontological).
narrative_ontology:cs_axiom('1825aef8-94f5-41a1-8826-c5f1e8089311', foundational, market_regulation_achieves_safety_better_than_criminalization).
narrative_ontology:cs_axiom_status(market_regulation_achieves_safety_better_than_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('1825aef8-94f5-41a1-8826-c5f1e8089311', market_regulation_achieves_safety_better_than_criminalization, empirically_contingent).
narrative_ontology:cs_reference_frame('1825aef8-94f5-41a1-8826-c5f1e8089311', state_regulation_of_commerce_for_public_health).
narrative_ontology:cs_drift_state('1825aef8-94f5-41a1-8826-c5f1e8089311', contemporary_legalization_trials_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1825aef8-94f5-41a1-8826-c5f1e8089311', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, users_exiting_criminal_markets).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, taxation_revenue_collectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adjacent_communities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, treatment_systems).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, regulatory_apparatus).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, law_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access regulated, quality-controlled substances at transparent prices without criminal penalties or exposure to violence. Exit from illegal supply chain reduces health risks from contamination and overdose. Subject to taxation and regulatory restrictions on quantity/potency/advertising; legal access is conditional on state-defined boundaries.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, users_exiting_criminal_markets, beneficiary,
    powerless, biographical, mobile, national).

% Produce and distribute controlled substances under regulatory licensing. Collect legal profits with market certainty and property rights protection. Subject to strict quality, labeling, potency, and advertising controls; cannot serve all demand (regulatory caps). Exit means loss of market access and licensing authority.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_producers, agenda_setter,
    organized, generational, constrained, national).

% Sets and enforces product standards, dosage limits, potency caps, advertising restrictions, retail location controls, age-verification protocols, and track-and-trace systems. Bears cost of inspection, testing, enforcement, and treatment infrastructure. Adjudicates disputes between producers and public health objectives.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, regulatory_apparatus, payer).

% Lose market share and revenue as legal supply captures demand. Cannot compete on price or availability once regulation establishes legal distribution. Structurally excluded from the legitimate market by regulatory barriers; remaining markets are non-compliant or unregulated segments.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, illegal_market_suppliers, excluded,
    powerful, biographical, trapped, national).

% Gain oversight of consumption patterns, dosing, contamination risks, and adverse effects. Can intervene via product standards, potency caps, and integrated treatment systems. Funding secured through taxation revenue and population-health data enables evidence-based interventions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_authorities, beneficiary,
    institutional, generational, analytical, national).

% Shift from enforcement against users/small suppliers to enforcement against unregulated producers, contamination, and age-restriction violations. Resource reallocation required; some enforcement burden reduces, but regulatory compliance monitoring increases. Institutional capacity for arrest/prosecution of users shrinks; capacity for producer audits and supply-chain verification grows.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, law_enforcement, payer,
    organized, biographical, constrained, national).

% Reduced street-level drug trade, associated violence, and property crime. Retail locations subject to distance/zoning rules to minimize neighborhood concentration. Risk of increased visibility of use in some settings; public order concerns shift from criminalized presence to regulated commerce visibility.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adjacent_communities, beneficiary,
    moderate, biographical, constrained, local).

% Receive funding from tax revenue on legal sales; access to better consumption and health data to target interventions; reduced stigma enabling earlier intervention. Also face demand surge if use volume increases post-legalization; capacity constraints.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, treatment_systems, beneficiary,
    moderate, generational, constrained, national).

% May increase consumption due to reduced legal/social barriers and easier supply access. No seat in the regulatory design process; their interests (future health status) are externalized from the immediate cost-benefit accounting of legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, potential_new_users, excluded,
    powerless, biographical, constrained, national).

% Face cross-border supply flows, arbitrage, and legitimacy challenges from neighboring legalized jurisdictions. Can observe outcomes and adjust their own policies; their prohibition stance is pressured but not logically foreclosed by this reading's implementation elsewhere.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, jurisdictions_maintaining_prohibition, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, regulated_producers).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of unsafe, uncontrolled supply: replaces fragmented illegal producers with standardized quality, dosage, and safety oversight; establishes single verified distribution chain; integrates public health data collection and intervention points.
% TRANSFER_FUNCTION: Transfers revenue from consumers of controlled substances to the state (via taxation), regulated producers (via legal profit), and public health/treatment infrastructure (via earmarked tax revenue). Transfers enforcement burden from criminal justice to regulatory/health systems. Transfers market power from illegal suppliers to state-licensed producers.
% ABSENT_VOICES: Potential future users whose use volume may increase are not represented in the initial legalization decision; their health externality is forecasted but not voted on. Illegal suppliers are structurally excluded. Jurisdictions enforcing prohibition neighbor the legalized state and experience spillover effects but have no formal seat in regulation design.
% DISAPPEARANCE_RATIONALE: If legalization and its regulatory apparatus vanished, illegal markets would rapidly re-expand to fill the supply gap within weeks; users would revert to contaminated/unverified supply; treatment systems would lose tax revenue; public health surveillance infrastructure would collapse; law enforcement would return to use-focused enforcement. The entire economic organization of the substance supply chain would reorganize around the now-unregulated market.
% FOUNDING_PROBLEM: Criminalization of drug use created a black market where suppliers have no incentive to control quality, dosage, or contamination; users face violence, overdose risk, and legal penalties; treatment is stigmatized; public health data is unavailable; law enforcement is overwhelmed with low-level enforcement; incarceration diverts resources from other public safety.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, treatment providers, and some law enforcement officials in legalized jurisdictions attest the founding problem is substantially addressed by legalization with regulatory controls. Prohibition advocates attest the founding problem persists as a false framing of drug use as a public health rather than public order issue. Evidence from Oregon, Portugal, Canada, and Uruguay is cited by legalization advocates; prevention-focused researchers cite increased use volume and new harms in those jurisdictions. No unanimous external corroboration; the dispute is live across expert communities.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the legalization model solves a real coordination problem (safe supply, public health oversight, eliminated black market) but imposes regulatory costs: taxation transfers revenue to the state, producers capture legal profits under licensing restrictions, and potential use volume increase creates externalized health costs for new users. Suppression is low (0.28) because the primary mechanism is incentive alignment (legal access, regulated safety, profit certainty for producers) rather than coercive exclusion; however, illegal suppliers face structural exclusion via regulatory barriers, and advertising/potency/retail-location controls are suppressive toward producers. Theater is low (0.18): the regulatory function is genuine (quality control, dosage limits, track-and-trace), though an omega notes the possibility that regulation becomes performative if use-prevention is the unstated goal disguised as safety. Accessibility collapse is moderate (0.65): once legalization is established, alternatives (black market, prohibition) are collapsed for users, but potential new users retain the option of non-use (the founding problem of 'how do we make substances safer for users who will use them anyway' does not compel non-users to use). Resistance is high (0.71): legalization faces active opposition from prohibition advocates, prevention-focused public health actors, and constituencies viewing increased use as a failure; law enforcement resistance is moderate but real. The measurement series run on a shared time grid across all three metrics. All baseline values are projected (basis='projected') because this reading's empirical history is limited to recent legalization trials; older prohibition and harm-reduction readings have longer evidentiary records.
 *
 * PERSPECTIVAL GAP:
 *   Regulated producers experience this as beneficial coordination and market opportunity (they are agenda-setters with constrained but secure exits). Users experience it as liberation from criminalization and access to safe supply (beneficiary status, mobile exit). The regulatory apparatus experiences it as successful public health intervention AND ongoing burden (agenda-setter status carries both enforcement cost and authority). Illegal suppliers experience it as market destruction (excluded status, trapped with no legitimate options). Prohibition advocates experience it as state-enabled harms and failed prevention (observer status, opposing framing of the founding problem itself). The engine computes divergent per-seat classifications from this structural asymmetry: users and public health see coordination; producers see legalized monopoly; enforcement sees reallocation; prohibition advocates see failure. None of these is 'wrong'—they are different seats' truths about the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Users (powerless, mobile exit) are near the beneficiary end (d near 0.0–0.3): the constraint subsidizes their safety by eliminating black-market price premiums and substituting legal access. Regulated producers (organized, constrained exit) are near symmetric or slightly beneficiary (d near 0.4–0.5): they gain legal profit and market certainty but are tightly regulated on potency, advertising, and location. The regulatory apparatus (institutional, analytical exit) is symmetric-to-payer (d near 0.5–0.7): it bears enforcement and oversight costs while capturing some legitimacy and funding. Illegal suppliers (excluded, trapped) face pure extraction in the form of market destruction (high d, near 1.0 in an indirect sense, though they are not formal stakeholders in the legalized system—they are excluded). Law enforcement (organized, constrained exit) is a payer (d elevated by enforcement reallocation costs). Potential new users (excluded) have no seat in this story's directionality; their future health is an externality not yet computed. No directionality overrides are needed; the structural derivation from beneficiary/victim declarations and exit options is sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading does not face classical mandatrophy: it is not a former coordination mechanism decayed into performance. Rather, it is a LIVE contested reading of the substance control kernel, where the mandate (state authority over drug markets) is itself disputed. The mandate here is 'regulate markets to ensure safety and eliminate black markets.' If use volume increases sharply and treatment capacity fails to keep pace, a secondary mandatrophy could emerge: the regulatory apparatus persists as theater (potency caps enforced, but use harms increase anyway), while the founding problem (unsafe supply) is replaced by a new one (excess use and inadequate treatment). This is marked in omega variables. The constraint avoids mandatrophy in early phases because the coordination function is fresh and the illegal market elimination is rapid; mandatrophy risk rises if: (a) use increases beyond forecast and public health capacity lags, (b) regulatory capture allows producers to circumvent safety standards, or (c) the state transitions to using legalization primarily for tax revenue rather than public health goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_volume_elasticity,
    'Does legalizing supply with reduced legal barriers substantially increase consumption volume, particularly among previously-abstinent populations?',
    '5–10 year longitudinal data from legalized jurisdictions (Oregon, Canada, Portugal, Uruguay) tracking consumption by age cohort, frequency, and substance type; comparison to counterfactual prohibition-enforced jurisdictions via proxy methods.',
    'High elasticity (use volume increases 30%+) shifts the founding-problem evaluation: if prevention is the unstated goal, legalization fails. Legalization advocates reframe the goal as ''safe supply for those who use anyway'' (shifting the founding problem), while prohibition advocates claim legalization caused preventable harms. This is not a measurement of the constraint''s type but a resolution of the contested founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_elasticity, empirical, 'Whether demand elasticity to legal supply is high or low.').

omega_variable(
    regulatory_capture_risk,
    'Do regulated producers successfully capture the regulatory apparatus, circumventing potency/advertising/dosage controls in service of profit maximization?',
    'Audit of regulatory enforcement: product testing vs. label claims, advertising scrutiny vs. actual practices, potency cap compliance in legalized jurisdictions.',
    'If capture is substantial, the constraint shifts from rope (genuine coordination) toward snare (regulation as cover for producer extraction). The public health beneficiary function becomes theater. This would force reclassification by the engine or trigger a mandatrophy transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether regulatory capture erodes the public health protection function.').

omega_variable(
    reading_coexistence_logics,
    'Can legalization and prohibition coexist in the same jurisdiction? Can a single jurisdiction hold both readings at once, or must one reading foreclose the other?',
    'Institutional analysis: do legalized jurisdictions maintain criminal penalties for non-compliance (e.g., unlicensed production, underage possession), effectively executing BOTH readings simultaneously? Or does legalization logically rule out criminalization of user-side activity?',
    'If legalization and prohibition can coexist (some substances legal, some criminal; some actors subject to regulation, some to criminalization), then the readings INFLUENCE but do not FORECLOSE each other, changing the cs_structure.reading_relations classification. If legalization logically rules out user-criminalization, the relation is FORECLOSES.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_logics, conceptual, 'Logical structure of the reading coexistence: do the readings foreclose or coexist?').

omega_variable(
    externality_concentration,
    'Are the health harms of increased use volume concentrated in specific populations (e.g., economically marginalized, youth cohorts), or distributed across all users?',
    'Public health surveillance data from legalized jurisdictions broken down by age, income, employment, health-insurance status, and geographic location. Comparison of health-outcome disparities pre- and post-legalization.',
    'If harms concentrate in powerless populations while benefits (tax revenue, regulated access, treatment funding) distribute to institutional actors, the constraint exhibits the asymmetric extraction signature of a snare, despite claiming rope status. This would flag a false-rope diagnosis. If harms and benefits distribute proportionally, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_concentration, empirical, 'Whether harms and benefits are distributed or concentrated.').

omega_variable(
    treatment_capacity_lag,
    'Does the state allocate tax revenue from legalization to treatment infrastructure at a pace sufficient to meet demand growth from increased use volume?',
    'Audit of tax revenue allocation: percentage devoted to treatment vs. enforcement vs. general revenue. Waiting lists for treatment programs; treatment-capacity utilization rates; outcome data on treatment efficacy.',
    'If revenue is allocated primarily to general state budgets rather than treatment, the founding problem (addressing harms) is not solved despite the legalization mandate. The regulatory apparatus becomes a tax-collection mechanism, and treatment-system capacity fails, producing a secondary mandatrophy: regulation persists but coordination function atrophies. This would trigger the T17 abductive trigger (mountain_extraction_accumulation) if extraction metrics rise post-legalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_capacity_lag, empirical, 'Whether legalization revenue funds treatment adequate to use growth.').

omega_variable(
    kernel_reading_empirical_grounds,
    'Is the choice between legalization, prohibition, and harm reduction reading grounded in empirical evidence about what works, or grounded in different moral framings of the same empirical landscape?',
    'Meta-analysis of legalization trials: do they show empirical superiority on defined health/safety outcomes, or do they show trade-offs (use up, harms down in some dimensions, up in others) that require normative judgment to weigh?',
    'If empirical evidence decisively favors one reading (e.g., legalization demonstrably reduces overdose mortality, non-consensual crime, and incarceration without increasing net harms), that reading''s authority is strengthened and competing readings lose grounding. If evidence shows trade-offs requiring normative judgment, all readings coexist as empirically defensible but morally contested—the kernel contest is not resolvable by evidence alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_empirical_grounds, conceptual, 'Whether the kernel contest is empirically resolvable or fundamentally normative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t3, substance_control_authority__legalization_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement_basis(subs_tr_t3, projected).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(subs_tr_t6, projected).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(subs_tr_t12, projected).
narrative_ontology:measurement(subs_tr_t18, substance_control_authority__legalization_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(subs_tr_t18, projected).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__legalization_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(subs_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t3, substance_control_authority__legalization_reading, base_extractiveness, 3, 0.36).
narrative_ontology:measurement_basis(subs_be_t3, projected).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.39).
narrative_ontology:measurement_basis(subs_be_t6, projected).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(subs_be_t12, projected).
narrative_ontology:measurement(subs_be_t18, substance_control_authority__legalization_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(subs_be_t18, projected).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__legalization_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(subs_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t3, substance_control_authority__legalization_reading, suppression_requirement, 3, 0.24).
narrative_ontology:measurement_basis(subs_su_t3, projected).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.26).
narrative_ontology:measurement_basis(subs_su_t6, projected).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.27).
narrative_ontology:measurement_basis(subs_su_t12, projected).
narrative_ontology:measurement(subs_su_t18, substance_control_authority__legalization_reading, suppression_requirement, 18, 0.28).
narrative_ontology:measurement_basis(subs_su_t18, projected).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__legalization_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(subs_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a three-reading constraint family modeling the substance control authority kernel. The legalization reading (this file) represents the state's authority to regulate drug markets as legal commerce. The prohibition reading models the state's authority to criminalize use/possession. The harm reduction reading models the state's authority to accept use while minimizing harms via public health. These are not descriptions of the same constraint from different perspectives; they are structurally distinct constraints with different beneficiary/victim structures, different enforcement mechanisms, and different founding problems. They compete for institutional instantiation within any jurisdiction. All three coexist globally. Each reading's ε-value is distinct and cannot be averaged; measurement of one reading's empirical outcome does not adjudicate the others. The network edges represent causal/institutional influence: legalization reading affects both siblings by displacing their institutional footprint in jurisdictions where it is adopted; harm reduction reading influences legalization by providing public health infrastructure and surveillance data; prohibition reading competes with legalization by maintaining criminal enforcement apparatus and alternative moral framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
