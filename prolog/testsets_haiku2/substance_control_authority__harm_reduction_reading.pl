% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm-Reduction Authority Over Drug Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-reduction reading of the contested
 *   kernel 'substance control authority': the state's power to regulate drug
 *   use and supply. This reading accepts that drug use will continue despite
 *   prohibition and shifts authority from criminal law to public health
 *   apparatus. Users decriminalized for possession/use but remain subject to
 *   health surveillance, treatment mandates, and conditional service access.
 *   Supply-side criminalization persists; the constraint maintains
 *   prohibition of distribution while decriminalizing consumption. Third
 *   parties bear disease-transmission risk externalities. The reading differs
 *   from prohibition (which criminalizes both users and suppliers) and from
 *   legalization (which regulates supply as legal commerce). The claim/metric
 *   gap is intentional: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination problem solved via health services + asymmetric extraction
 *   as users remain partially victimized) while metrics are authored for
 *   honest descriptive truth, not to match the claim.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: primary target; decriminalized from possession but subject to health surveillance and treatment conditionality
 *   - public_health_authorities: agenda-setter; design and operate service infrastructure
 *   - criminalized_drug_markets: sustained enforcement target; supply remains prohibited despite user-side decriminalization
 *   - law_enforcement_agencies: agenda-setter + payer; shift from user arrests to supply interdiction, lose enforcement revenue
 *   - treatment_providers: beneficiary + agenda-setter; expand service provision and institutional authority
 *   - third_parties_disease_risk: beneficiary; externality reduction through needle exchange and infection control
 *   - prohibition_advocates: excluded; their objection (use itself is harmful and requires criminalization) is not admitted
 *   - legalization_advocates: excluded; their objection (criminalized supply perpetuates market harms) is not admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.31).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm-Reduction Authority Over Drug Use").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '75206fa6-6b66-49a8-9780-2952a992bcac').
narrative_ontology:cs_kernel_codification('75206fa6-6b66-49a8-9780-2952a992bcac', formalized).
narrative_ontology:cs_authority_grounding('75206fa6-6b66-49a8-9780-2952a992bcac', extraction).
narrative_ontology:cs_interpretation_layer_present('75206fa6-6b66-49a8-9780-2952a992bcac').
narrative_ontology:cs_reading_relation('75206fa6-6b66-49a8-9780-2952a992bcac', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('75206fa6-6b66-49a8-9780-2952a992bcac', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('75206fa6-6b66-49a8-9780-2952a992bcac', foundational, drug_use_inevitable_under_prohibition).
narrative_ontology:cs_axiom_status(drug_use_inevitable_under_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('75206fa6-6b66-49a8-9780-2952a992bcac', drug_use_inevitable_under_prohibition, empirically_contingent).
narrative_ontology:cs_axiom('75206fa6-6b66-49a8-9780-2952a992bcac', foundational, health_authority_superior_to_criminal_authority).
narrative_ontology:cs_axiom_status(health_authority_superior_to_criminal_authority, holdable).
narrative_ontology:cs_axiom_grounding('75206fa6-6b66-49a8-9780-2952a992bcac', health_authority_superior_to_criminal_authority, instrumental).
narrative_ontology:cs_axiom('75206fa6-6b66-49a8-9780-2952a992bcac', secondary, supply_side_criminalization_necessary_for_state_control).
narrative_ontology:cs_axiom_status(supply_side_criminalization_necessary_for_state_control, holdable).
narrative_ontology:cs_axiom_grounding('75206fa6-6b66-49a8-9780-2952a992bcac', supply_side_criminalization_necessary_for_state_control, instrumental).
narrative_ontology:cs_reference_frame('75206fa6-6b66-49a8-9780-2952a992bcac', health_authority_over_drug_response).
narrative_ontology:cs_drift_state('75206fa6-6b66-49a8-9780-2952a992bcac', contemporary_post_opioid_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75206fa6-6b66-49a8-9780-2952a992bcac', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, third_parties_disease_risk).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, criminalized_drug_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighboring_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain partial victim status under health harms (overdose, infection, organ damage) but exit criminal penalties for possession/use. Access harm-reduction services (needle exchange, medication-assisted treatment, supervised consumption sites, counseling) that reduce acute health risks. Subject to treatment mandates, drug testing, monitoring, and conditional access to services. Cannot exit the system through legal withdrawal (decriminalization is not optional) but can strategically limit engagement with services or migrate jurisdictions. Identity as 'drug user' becomes formalized within health system rather than criminal system — a different but still institutionalizing classification.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary).

% Set and enforce harm-reduction framework: define services, eligibility, data collection, monitoring protocols, medication formularies, and enforcement conditions. Justify arrangement as reducing overdose mortality, communicable disease, and incarceration when criminalization alone fails. Control institutional expansion (clinic networks, surveillance infrastructure, training programs) and budget allocation. Can exit by shifting back to criminalization or forward to legalization, but have strong incentive to maintain status quo given institutional investments.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive disease-transmission risk reduction through needle-exchange and infection-control services: reduced HIV, hepatitis C, and other bloodborne pathogen transmission in general population. Do not run services or design framework but benefit from health externality. Carry diffuse cost if public health resources flow to drug-user services rather than to other prevention priorities. Cannot exit constraint's benefit stream without exiting public health system itself; constrained by geography and reliance on state health infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_parties_disease_risk, beneficiary,
    organized, generational, constrained, national).

% Face sustained supply-side prohibition, interdiction, incarceration, and asset seizure despite user-side decriminalization. Decriminalization of users does not legalize supply or reduce market interdiction. Markets remain underground, subject to price volatility, quality uncertainties, and violence. Cannot exit through legalization without policy shift; cannot exit through migration without supply-chain disruption. The constraint maintains the prohibition that makes their operation criminal while reducing police attention to lower levels of distribution.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, criminalized_drug_markets, payer,
    powerful, biographical, trapped, national).

% Shift enforcement from user-level arrests (low-yield resource consumption) to supply-chain disruption, trafficking interdiction, and organized crime prosecution. Retain authority to enforce supply-side prohibition; lose user-level enforcement revenue (asset seizure, fines from possession arrests, incarceration bed-use). Face institutional resistance: law enforcement culture built on user-level enforcement; retraining and organizational reorientation required. Can exit through reverting to criminalization or advancing to legalization, but institutional path-dependence favors maintaining current enforcement allocation.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, payer).

% Bear cross-border effects: users and distribution networks may relocate or shift hubs to boundary zones if one jurisdiction decriminalizes users but criminalizes supply while neighbors maintain full criminalization. They absorb externalized crime, public health costs, and enforcement burden without controlling the policy generating the displacement. Cannot easily exit through unilateral policy escalation; negotiated regional frameworks or policy harmonization required but politically difficult.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, neighboring_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Expand service provision, client volume, institutional authority, and funding under harm-reduction framework. Design and administer medication-assisted treatment (methadone, buprenorphine), needle-exchange programs, counseling, medical monitoring, and supervised consumption sites. Benefit from public funding, insurance coverage expansion, and institutional legitimacy as health authority. Face pressure to show cost-effectiveness and health outcomes; manage complex patients with multiple morbidities. Can exit if funding disappears, but depend on constraint's political durability for revenue.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, treatment_providers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, treatment_providers, agenda_setter).

% Would argue criminalization of possession is necessary to deter use, protect third parties from disorder, and maintain social norms against drug use. They contest harm-reduction's acceptance of use as a policy baseline, asserting use itself is a primary harm that decriminalization amplifies. Excluded from this reading's authority structure: their objection (use must be criminalized) is not admitted as a valid perspective; the reading's starting premise (accepting use as persistent) forecloses their alternative. Can seek to displace the constraint through criminalization revival but face entrenched treatment-provider institutions and competing policy narratives.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Would argue maintaining supply-side criminalization perpetuates organized crime, price inflation, quality unpredictability, and violence that harm users and third parties more than regulated legal markets would. They contest harm-reduction's compromise as incoherent: accepting use (users know what they buy and take it where they want) while criminalizing supply (underground markets persist). Excluded from this reading: their objection (criminalized supply is the core failure) challenges the reading's framework itself. Can seek to displace through legalization but face political resistance and institutional consolidation of harm-reduction infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the failure of criminalization-only approaches to reduce use: channels widespread drug use toward health-surveillance and intervention infrastructure (treatment, disease prevention, supervised consumption) rather than through criminal penalties that do not prevent use and consume enforcement resources without public-safety gain. Coordinates response when criminalization empirically fails.
% TRANSFER_FUNCTION: Moves authority over drug-use response from criminal law to public health apparatus. Transfers users from criminal-penalty victim set to health-surveillance and treatment-mandate victim set. Redirects enforcement resources from user-level arrests to supply-chain disruption. Expands treatment-provider institutional capacity and public health surveillance authority. Moves disease-reduction externality to third parties while keeping drug markets criminalized.
% ABSENT_VOICES: Prohibition advocates (who argue criminalization is necessary to maintain social norms and deter use, and that acceptance of drug use increases overall harm) are structurally excluded from the reading's framework. Legalization advocates (who argue that criminalized supply perpetuates market harms more destructive than regulation would be, and that the constraint's hybrid approach is incoherent) are also excluded. Both groups would say the founding problem and the constraint's solution are misaligned; their objections are not admitted within the harm-reduction reading's epistemic structure.
% DISAPPEARANCE_RATIONALE: If the harm-reduction constraint vanished, the jurisdiction would face pressure to either revert to full criminalization (with user-level enforcement costs, reduced treatment access, mass incarceration) or advance to legalization (with regulated supply, different enforcement priorities, removal of supply-side criminalization). The treatment-provider infrastructure, law enforcement resource allocation, public health surveillance apparatus, and service-access mechanisms all depend on this reading remaining institutionally stable. Users would lose decriminalization status. Organized crime would face different enforcement pressures. Third parties would lose disease-reduction benefits. The world materially rearranges.
% FOUNDING_PROBLEM: Criminalization of drug use produces demonstrable failures: mass incarceration of users without addiction treatment access, user-level enforcement consuming police resources with minimal public-safety yield, underground markets enabling overdose and communicable disease, exclusion of users from legitimate health care due to legal stigma, and perverse incentives that increase drug purity and potency (users dose higher when quality is uncertain). Criminalization alone does not prevent use.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, epidemiologists, law enforcement agencies in multiple jurisdictions, and addiction medicine specialists attest the founding problem persists: overdose mortality, hepatitis C and HIV transmission, incarceration rates, and enforcement resource costs remain high under criminalization-only regimes. Prohibition advocates dispute the adequate characterization of the problem (they argue the problem is use itself, not criminalization's handling of use) and legalization advocates dispute the adequacy of the constraint's solution (criminalized supply perpetuates market failures). Independent research from RAND Corporation, European drug policy institutes, and public health agencies not financially tied to harm-reduction expansion confirms the empirical failure of criminalization-only approaches on overdose and transmission metrics.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint solves a real coordination problem (managing widespread use when criminalization fails) but does so by shifting authority to public health without removing coercion — users gain decriminalization but lose bodily autonomy over treatment mandates and surveillance. Suppression is lower (0.31) because the constraint relies on voluntary service uptake more than on criminal threat; users can refuse treatment within negotiated limits. Theater rises gradually (0.08 to 0.22) as the service infrastructure matures: early focus on disease reduction (functional) shifts to institutional legitimacy-maintenance as treatment-provider sectors stabilize (increasingly performative). The measurement trajectory shows stable medium-term extractiveness (plateau after t=15) because the constraint has reached equilibrium — initial gains in user compliance and disease reduction do not increase further with time; enforcement effort stabilizes. The shared time grid applies metrics at every examined point: extractiveness, theater, and suppression all authored at t=0, 5, 10, 15, 20, 25.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (people who use drugs, criminalized markets, law enforcement) and the agenda-setter seats (public health authorities, treatment providers) should compute different constraint types: from the payer side, the arrangement looks extractive (users lose bodily autonomy, markets face interdiction, enforcement loses revenue); from the agenda-setter side, it is genuine coordination solving a failed-criminalization problem. The engine computes this divergence from structural data (beneficiary/victim, power, exit options, spatial scope) without author tuning.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs sit near the target end (d approaching 0.8+): they are decriminalized (directionality boost toward beneficiary) but remain subject to health surveillance, treatment mandates, and conditional service access (directionality pull back toward target). Their identity_locked exit (addiction, self-concept fused with drug-use identity, social isolation) means they cannot easily abandon the system even if it becomes extractive. Public health authorities sit at the beneficiary end (d near 0.0): they design and enforce the rules, set resource allocation, and justify outcomes by their own framework. Law enforcement agencies sit asymmetrically (d near 0.5): they lose enforcement revenue and user-level arrests (payer direction) but gain authority over supply-focused interdiction (beneficiary direction); the two effects partially cancel. Criminalized drug markets (traffickers) sit at the target end (d approaching 1.0): they face sustained supply-side prohibition, incarceration, and interdiction regardless of user decriminalization. Third parties (disease-risk groups) are beneficiaries (d near 0.0): they receive disease reduction as an externality without bearing the surveillance or mandate costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap by maintaining active functional coupling between the health problem (drug-related harms) and the intervention mechanism (health services, decriminalization, surveillance). The risk emerges if the founding problem (criminalization-driven incarceration and underground markets) is solved but the service apparatus persists for institutional reasons — e.g., treatment providers lobbying to maintain user bases, health authorities expanding surveillance authority beyond its original mandate. The theater trajectory (rising from 0.08 to 0.22) captures this drift: as disease-reduction outcomes plateau, a growing share of service activity becomes maintenance-of-institutional-legitimacy rather than health protection. The constraint remains tangled_rope at this measurement point, but omega variables flag the drift toward piton (atrophied health mandate, institutionalized surveillance theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decriminalization_gateway_to_normalization,
    'Does decriminalization of possession functionally operate as a gateway to pressures for full legalization, or can the harm-reduction reading sustain indefinitely in the hybrid state (decriminalized users + criminalized supply)?',
    'Longitudinal policy analysis across 20+ jurisdictions that adopted harm-reduction models: track whether decriminalization remains stable or predictably transitions to legalization within 15-25 years. Distinguish stable harm-reduction regimes from transient equilibria.',
    'If decriminalization structurally enables legalization pressure, the harm-reduction reading is an unstable intermediate state; the constraint''s type may shift from tangled_rope (sustainable hybrid) toward rope (coordination mechanism that actually transitions to another form). If stable, the reading supports its own institutional continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decriminalization_gateway_to_normalization, empirical, 'Whether harm-reduction is a stable endpoint or a transient stage on the path to legalization.').

omega_variable(
    treatment_mandate_vs_bodily_autonomy,
    'Is the constraint''s health-surveillance and treatment-mandate apparatus extractive from users in a way that differs structurally from criminal law (shifting authority venue but not reducing coercion), or does it materially reduce the net coercive burden despite its mandates?',
    'Comparative analysis of user-side coercion before and after harm-reduction adoption: measure incarceration, health outcomes, access to exit pathways, bodily autonomy over treatment, and subjective autonomy reports. Compare jurisdictions with harm-reduction to prohibition-only and legalization regimes.',
    'If mandates transfer coercion without reducing it (substituting surveillance for incarceration), extractiveness increases and the constraint should reclassify toward snare. If health-surveillance mandates materially reduce net coercion relative to criminalization, extractiveness is genuinely lower and the tangled_rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_mandate_vs_bodily_autonomy, empirical, 'Whether harm-reduction reduces net coercion or merely relocates it from criminal law to health apparatus.').

omega_variable(
    supply_criminalization_vs_market_externalities,
    'Does maintaining supply-side criminalization within the harm-reduction reading prevent organized crime and market consolidation, or does it perpetuate the very market distortions (price inflation, quality unpredictability, violence) that legalization advocates cite as the founding problem of harm-reduction''s own logic?',
    'Comparative market analysis: measure drug prices, purity variation, supply-chain violence, and organized crime revenue in harm-reduction vs. prohibition vs. legalization jurisdictions. Assess whether criminalized supply under harm-reduction enables the same market failures legalization would address.',
    'If criminalized supply perpetuates significant market harms that undermine health outcomes, the constraint becomes internally contradictory (accepts use but maintains supply conditions that harm users); this would suggest the founding problem and the constraint''s mechanism are misaligned, strengthening legalization-reading claims. If market harms are measurably lower than under pure prohibition, the hybrid reading sustains its coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_criminalization_vs_market_externalities, conceptual, 'Whether harm-reduction''s acceptance of use is coherent with its maintenance of criminalized supply.').

omega_variable(
    third_party_harm_distribution,
    'Do third parties actually benefit from disease-transmission reduction under harm-reduction, or are the disease-reduction gains concentrated among people who use drugs while new third-party harms (drug-facilitated crime, overdose rescue costs, treatment-infrastructure gentrification) offset them?',
    'Public health surveillance data: track communicable disease prevalence, crime victimization, emergency response utilization, and health-care system strain before and after harm-reduction adoption. Disaggregate by disease outcome vs. other third-party impacts.',
    'If third parties experience net benefit, the constraint''s beneficiary set is accurate and tangled_rope classification holds. If third-party harms offset disease gains, the beneficiary set should narrow and extraction from users appears higher (less genuine coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_harm_distribution, empirical, 'Whether third parties materially benefit from harm-reduction disease reduction.').

omega_variable(
    kernel_reading_coherence,
    'Is the harm-reduction reading defensible as a coherent alternative to prohibition and legalization, or is it unstable under sustained critique from both directions?',
    'Philosophical/policy analysis of the foundational premises: Does the reading coherently justify accepting drug use while criminalizing supply? Do prohibition and legalization readings logically foreclose this reading, or can all three coexist as live defended positions?',
    'If harm-reduction is logically stable, reading_relations should be coexists_with for both siblings. If prohibition logically forecloses harm-reduction (by asserting use cannot be accepted), or legalization forecloses it (by asserting supply cannot remain criminalized), the relation should shift to forecloses and the reading''s axioms would require reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether harm-reduction is a structurally coherent reading of the contested kernel or a logically unstable compromise.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression in this reading (0.31) structural (enforcement machinery, legal barriers, service gatekeeping) or internalized (users accept monitoring/mandates as legitimate health intervention and comply without external force)?',
    'Post-exit analysis: if users exit the constraint (migrate to legalization jurisdictions, abandon treatment, cease reporting), does suppression persist (internalized self-monitoring) or dissolve (structural suppression only)? Qualitative interviews on compliance motivation.',
    'If suppression is primarily structural, the measured 0.31 represents enforcement intensity and is the right metric. If internalized, the constraint carries hidden higher-order suppression (users police themselves even absent enforcement) not captured by the scalar, raising the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized user compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__harm_reduction_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__harm_reduction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__harm_reduction_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__harm_reduction_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__harm_reduction_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__harm_reduction_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__harm_reduction_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__harm_reduction_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__harm_reduction_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement_basis(subs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% The kernel substance_control_authority instantiates three structurally distinct constraint readings: harm_reduction_reading (this file), prohibition_reading, and legalization_reading. Each reading has its own epsilon, beneficiary/victim structure, enforcement mechanism, and institutional authority. The readings are not alternative framings of one constraint but three separate constraints unified by shared kernel and contested legitimacy claim. Each reading should be authored as an independent story with its own omegas documenting the alternatives. Network edges link siblings: harm_reduction affects both prohibition and legalization by changing the legitimacy landscape of state drug authority; each reading also affects the others. Prohibition and legalization readings should similarly declare this constraint in their affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
