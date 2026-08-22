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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: State Harm Reduction Authority (Decriminalization + Services)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates a harm reduction reading of contested state
 *   drug policy authority. The state authorizes continued use
 *   (decriminalization) while deploying public health infrastructure
 *   (supervised consumption sites, medication-assisted treatment, disease
 *   screening) to minimize health harms and disease transmission. This
 *   reading is structurally distinct from prohibition (which criminalizes use
 *   to deter behavior) and legalization (which permits regulated commercial
 *   supply). The harm reduction reading accepts use as a persistent fact and
 *   optimizes around disease and death minimization rather than enforcement
 *   or commercial control. Users exit criminal victim status but remain
 *   subject to health harms the services mitigate. Neighboring communities
 *   bear neighborhood-level harms (open-air markets, visible use, property
 *   crime). Third parties face disease transmission risk that services
 *   directly manage. The arrangement requires active enforcement — to exclude
 *   dealers, prevent open-air commercial competition with user-sourced
 *   supply, and maintain service-site boundaries.
 *
 * KEY AGENTS:
 *   - state_public_health_authority: Institutional agenda-setter; frames drug use as disease/epidemiology problem; authoritizes and funds services; derives institutional autonomy and professional legitimacy from disease-reduction framing
 *   - users_accessing_services: Powerless beneficiary/payer; exit criminal victim status; remain subject to health harms; access non-punitive care; choice set bounded by service availability and political contingency
 *   - harm_reduction_practitioners: Organized beneficiary; professional employment and legitimacy depend on state continuation; constrained by performance metrics (disease reduction, non-overdose outcomes)
 *   - neighboring_communities: Moderate-power payer; bear neighborhood harms (open-air markets, visible use, crime); exit options constrained by housing tenure and relocation cost
 *   - enforcement_transition_workers: Moderate-power payer; experience loss of traditional enforcement-volume-dependent legitimacy and advancement; can professionalize (police social work partnerships) or resist
 *   - prohibition_advocates: Powerful excluded party; would reframe as enabling addiction and failing to address root cause; jurisdiction-dependent power (strong in prohibition-aligned regions, weak where harm reduction has institutional support)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.22).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Authority (Decriminalization + Services)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '5954ce1b-7810-4691-ad01-2c850dad77c3').
narrative_ontology:cs_kernel_codification('5954ce1b-7810-4691-ad01-2c850dad77c3', formalized).
narrative_ontology:cs_authority_grounding('5954ce1b-7810-4691-ad01-2c850dad77c3', extraction).
narrative_ontology:cs_interpretation_layer_present('5954ce1b-7810-4691-ad01-2c850dad77c3').
narrative_ontology:cs_reading_relation('5954ce1b-7810-4691-ad01-2c850dad77c3', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5954ce1b-7810-4691-ad01-2c850dad77c3', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('5954ce1b-7810-4691-ad01-2c850dad77c3', foundational, continued_use_is_structural_fact).
narrative_ontology:cs_axiom_status(continued_use_is_structural_fact, holdable).
narrative_ontology:cs_axiom_grounding('5954ce1b-7810-4691-ad01-2c850dad77c3', continued_use_is_structural_fact, empirically_contingent).
narrative_ontology:cs_axiom('5954ce1b-7810-4691-ad01-2c850dad77c3', foundational, services_are_primary_control_mechanism).
narrative_ontology:cs_axiom_status(services_are_primary_control_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5954ce1b-7810-4691-ad01-2c850dad77c3', services_are_primary_control_mechanism, instrumental).
narrative_ontology:cs_reference_frame('5954ce1b-7810-4691-ad01-2c850dad77c3', decriminalized_use_with_public_health_infrastructure).
narrative_ontology:cs_drift_state('5954ce1b-7810-4691-ad01-2c850dad77c3', contemporary_legalization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5954ce1b-7810-4691-ad01-2c850dad77c3', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_practitioners).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, users_accessing_services).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighboring_communities).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, disease_transmission_vectors).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, enforcement_transition_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, treatment_provider_sector).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, users_accessing_services).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, treatment_provider_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers harm reduction policy: authorizes decriminalization, funds and licenses consumption sites, medication-assisted treatment, needle exchange, disease screening. Operates under public health mandate rather than criminal enforcement. Derives institutional autonomy, budget authority, and professional legitimacy from reframing drug use as an epidemiology problem. Can point to declining disease transmission and overdose death rates as evidence of efficacy. Political sustainability depends on health outcomes and political coalition support.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Exit criminal victim status through decriminalization; access non-judgmental services without penalty for possession or use. Receive sterile injection supplies, overdose reversal medication, medical monitoring, addiction treatment, housing support. Remain subject to health harms (overdose, infection, organ damage) and social harms (housing instability, family disruption) that services mitigate but do not eliminate. Choice set is bounded by service location, hours, and state funding contingency. Can discontinue services or continue use at personal risk, but cannot change the underlying drug policy framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, users_accessing_services, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, users_accessing_services, payer).

% Operate consumption sites, needle exchange, medication-assisted treatment, and coordination programs under state authorization. Professional employment, legitimacy, and advancement depend on state continuation of the model and political support. Acquire status as public health professionals rather than law enforcement auxiliaries. Constrained by performance metrics (disease reduction, non-overdose rates) and by political vulnerability — a shift to prohibition or legalization would displace the professional domain.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_practitioners, beneficiary,
    organized, biographical, constrained, national).

% Claim epidemiological territory over drug-use management: shift budget authority from criminal enforcement to public health services, shift performance metrics from arrest rates to morbidity/mortality reduction, shift institutional autonomy to disease-focused decision-making. Can shift framing between treatments and enforcement depending on political climate (escalate or relax services based on political pressure). Political legitimacy rides on measurable health outcomes and on managing neighborhood externalities.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Bear neighborhood-level harms: open-air drug markets often persist or relocate near consumption sites; visible injection in parks and public spaces; used needles in outdoor areas; increased property crime (theft to fund continued use); noise and social disorder. Services reduce these harms relative to prohibition (no arrest-and-release cycling, some site supervision and sanitation) but do not eliminate them. Exit options are constrained by housing tenure (renters and homeowners cannot easily relocate) and by economic resources (relocating is costly). Can organize politically to restrict site placement or demand community mitigation investments.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, neighboring_communities, payer,
    moderate, biographical, constrained, local).

% Police and correction officers whose enforcement volume and institutional legitimacy traditionally depended on drug arrests and prosecutions. Harm reduction reduces this dependency — lower arrest volumes, fewer prosecutions, smaller jail/prison populations for drug offenses. Experience this as loss of professional legitimacy, career advancement opportunities, and union negotiating leverage. Can professionalize into social work partnerships (police social work teams, deflection programs) or resist and advocate for enforcement escalation. Constrained by union contracts and by institutional path-dependency.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, enforcement_transition_workers, payer,
    moderate, biographical, constrained, local).

% Political and moral constituencies (conservative lawmakers, law-and-order advocates, some community organizations) who view drug use as a behavior that should be deterred through criminal penalty, not accommodated through services. Excluded from policy authority in harm reduction jurisdictions. Would reframe the constraint as enabling addiction, undermining personal responsibility, failing to address the root problem (user behavior change), and shifting costs to neighborhoods and taxpayers. Their objections are heard in legislative testimony but do not block implementation where harm reduction coalitions have institutional support and health data.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    powerful, generational, constrained, national).

% Addiction treatment providers (residential, outpatient, medication-assisted treatment programs, recovery housing) receive referrals and co-managed cases from harm reduction sites. Benefit from expanded user engagement through low-barrier pathway and continuous relationship. Face tension with the harm reduction model: harm reduction's low-barrier philosophy (accepting ongoing use, not requiring abstinence as precondition) can conflict with abstinence-based treatment ideology and program models. Constrained by funding model — insurance reimbursement and government contracts may penalize overlap with harm reduction or may require abstinence-focused outcomes that harm reduction sites cannot guarantee.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, treatment_provider_sector, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, treatment_provider_sector, payer).

% The epidemiological reality of bloodborne pathogen transmission (HCV, HIV, HBV) and respiratory pathogen spread in crowded spaces. Not an agent but a class of harms the constraint is engineered to manage through needle provision, vaccination, isolation protocols, and ventilation. Authoring this entity as a 'payer' names the primary harm the constraint is designed to address: disease transmission is what the services directly target.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, disease_transmission_vectors, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_authority__harm_reduction_reading, disease_transmission_vectors).

% Political constituencies and economists who argue that full legalization and regulated commercial supply would achieve harm reduction goals better than state-managed services: eliminating black markets, ensuring product quality, enabling taxation, and removing the state's gatekeeping power over supply. Excluded from policy authority in harm reduction jurisdictions. Would argue that the constraint is a half-measure maintaining state control while appearing progressive, and that it fails to address the root cause (criminalization of supply). Their objections are heard but do not displace the constraint unless political support shifts.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    powerful, generational, constrained, national).

% UN Drug Control conventions and signatory enforcement systems (International Narcotics Control Board) that define permissible drug policy. Harm reduction's acceptance of continued use can be read as inconsistent with treaty language on 'eradication' and 'elimination of non-medical use.' Some treaty systems allow interpretation of harm reduction as consistent with 'adequate safeguards'; others treat it as non-compliant. Jurisdictions adopting harm reduction either reinterpret treaties (Canada, Switzerland, Netherlands) or formally reserve. Structurally trapped — cannot unilaterally change international regime, but face diplomatic pressure and compliance questions.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, international_drug_enforcement_regimes, excluded,
    powerful, generational, trapped, global).

% The epidemiological risk of bloodborne and respiratory pathogen circulation in the general population through disease transmission vectors. Not an agent but a class of harms the constraint manages at the population level through disease prevention infrastructure. The services directly target this risk.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_party_infectious_disease_risk, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(substance_control_authority__harm_reduction_reading, third_party_infectious_disease_risk).

% Takes the full constraint structure as the analytical object: state authorization + service infrastructure + disease management mechanisms + community externalities + political sustainability conditions. Tracks how the harm reduction reading distinguishes itself structurally from prohibition (decriminalization vs. enforcement escalation) and legalization (state-managed vs. market-regulated supply), and how these three readings compete for institutional dominance.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of infectious disease prevention and overdose mortality management among a dispersed user population that prohibition-enforcement alone cannot reach: provides vector control (needle provision, vaccination, medical monitoring) and keeps users in contact with health systems rather than driven underground.
% TRANSFER_FUNCTION: Moves public health budget and institutional authority from criminal enforcement to public health services; moves legitimacy of drug use from criminal frame to disease frame; moves enforcement labor from police/courts to social workers/nurses; moves disease burden (and disease reduction) from communities to public health accounting.
% ABSENT_VOICES: Prohibition advocates and law-and-order constituencies are structurally excluded — they would argue for enforcement escalation and reject the premise that continued use can be 'managed' rather than eliminated. International drug control regimes are diplomatically sidelined. Some sections of affected communities are isolated from policy input — marginalized neighborhoods bearing neighborhood-level harms have less institutional voice than public health authority and harm reduction practitioners.
% DISAPPEARANCE_RATIONALE: If decriminalization and service infrastructure vanished, users would return to criminal victim status (arrest, incarceration, criminal record); disease transmission would accelerate (no sterile supplies, no vaccination access); overdose death rates would rise sharply; police enforcement would intensify; the epidemiological knowledge of disease circulation would revert to criminal-system silence. The world would materially rearrange toward prohibition.
% FOUNDING_PROBLEM: High rates of infectious disease (HCV, HIV transmission) and overdose mortality among people who use drugs, driven by injection without sterile supplies and lack of overdose reversal capacity; enforcement-led prohibition paradoxically increased harms by driving use underground and away from health systems.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities, independent epidemiologists, and harm reduction researchers from outside benefiting-party seats attest that disease transmission and overdose mortality remain live problems and that service-based mitigation is evidence-supported. Prohibition advocates attest the founding problem differently (they frame the 'real problem' as addiction itself, not as disease transmission), but the specific health harms this reading addresses are documented by health systems and verified by follow-up studies in sites like Switzerland, Canada, and Australia.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.58 reflects an asymmetric arrangement: public health authority and harm reduction practitioners benefit institutionally and professionally; users benefit from decriminalization and services but remain subject to substantial health harms; neighboring communities bear diffuse costs (disorder, crime, disease risk) while disease transmission control is treated as a public health good (not costed to communities). The arrangement is NOT pure extraction because genuine coordination is present — the disease prevention problem is real and services solve it. It is NOT pure coordination because the framing as 'disease management' rather than 'behavior control' privileges one institutional reading and excludes prohibition/legalization framings. Suppression is moderate-low (0.22) — the constraint's enforcement is primarily boundary maintenance (keeping drug commerce off the street, maintaining site perimeters) rather than behavior suppression; users can decline services or continue use, and are not forced into treatment. Theater is low (0.18): the services have documented real function (disease reduction, overdose prevention), though some performance metrics are performatively emphasized (recovery rates, abstinence benchmarks that are not structurally necessary to the harm reduction goal). The temporal series shows extractiveness rising from 0.48 to peak at 0.59 (time 15) then stabilizing at 0.58, driven by political consolidation of the model — initial skepticism (prohibition advocates' resistance) decreases over time as health data accumulates and the institutional model hardens. Suppression falls monotonically as the constraint normalizes and becomes less politically contested.
 *
 * PERSPECTIVAL GAP:
 *   The state_public_health_authority and harm_reduction_practitioners will compute as beneficiaries with low directionality (d near 0.2) — they derive institutional benefit, set the agenda, face no exit pressure. Users will compute as targets with moderate-to-high directionality (d around 0.6–0.75) — they exit criminal jeopardy but remain subject to health harms and service dependency. Neighboring communities will compute with directionality around 0.65 — they bear diffuse costs, have constrained exit (housing/relocation), and do not set policy. From the beneficiary seats the arrangement is genuine coordination with some regrettable externalities (neighborhood harms). From the user and community seats, the same structure operates as a transfer of institutional authority and budget from enforcement to public health while neighborhood-level costs persist. The engine computes this divergence from power, exit, and spatial scope — users' local spatial scope and identity-locked or constrained exit produce higher d; authority's institutional power and analytical spatial scope produce lower d.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: state_public_health_authority (institutional power, analytical spatial scope, arbitrage exit → d ≈ 0.15–0.25); harm_reduction_practitioners (organized power, national scope, constrained exit → d ≈ 0.25–0.35); users_accessing_services (powerless, local scope, constrained exit, but exit is political contingency on funding rather than structural lock → d ≈ 0.55–0.65, moderate target). Victims (payers): neighboring_communities (moderate power, local scope, constrained exit → d ≈ 0.60–0.70); enforcement_transition_workers (moderate power, local scope, constrained exit → d ≈ 0.50–0.60). The disease transmission vectors and third-party risk are non-agents but the beneficiary/victim structure reflects the directionality: public health authority collects the institutional benefit of disease control; users collect decriminalization (a real benefit, worth substantial d reduction); neighborhoods pay the externality. No directionality override is needed; the derivation chain (beneficiary status + exit options + power + scope) captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (high disease transmission and overdose mortality in unsupervised populations) is live — the epidemiological facts are attested by independent health researchers and death registries in multiple jurisdictions. The constraint is not mandatrophic because the founding problem persists and the arrangement has not atrophied into performance. The threat is political rather than functional: if prohibition advocates or legalization advocates gain jurisdiction, the constraint could be replaced (prohibition would shift to criminalization; legalization would shift to commercial regulation). The contest between these three readings is live in legislatures and courts, not yet resolved into theater. The harm reduction reading sustains itself through measurable health outcomes (declining HCV transmission, stable or rising overdose-reversal rates), which feed political legitimacy. If political support shifts to prohibition or legalization, the mandatrophy clock would reset — the constraint would become a vestigial institutional form maintained by inertia rather than by either policy effectiveness or political mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_transmission_causal_attribution,
    'What proportion of the measured disease reduction (HCV/HIV transmission decline, overdose death reduction) is caused by needle provision and medication-assisted treatment specifically, versus caused by other confounders (aging of the user cohort, medical advances in treatment, macroeconomic factors reducing new initiation)?',
    'Randomized controlled trials or instrumental variable analysis isolating the causal effect of service availability from secular trends. Comparison of jurisdictions with identical secular conditions but different service deployment.',
    'If services are causally central to disease reduction, the constraint''s efficacy claim is strengthened and prohibition arguments weaken. If disease reduction is primarily driven by confounders, the constraint''s functional justification erodes and political support depends more on decriminalization ideology than on health outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_transmission_causal_attribution, empirical, 'Causal attribution of measured disease reduction to the harm reduction services versus other factors.').

omega_variable(
    neighborhood_externality_acceptability_boundary,
    'What level of neighborhood harm (visible use, open-air markets, property crime, disease risk in local environment) is politically acceptable as a trade-off for user decriminalization and health improvement? Where is the acceptability threshold, and who decides?',
    'Public opinion surveys and political economy analysis tracking where harm reduction support collapses; observational study of neighborhood composition and political opposition to sites; historical analysis of site placement decisions and neighborhood consent.',
    'If neighborhood harms are bounded below a political threshold, the constraint is sustainable. If harms are perceived as excessive, prohibition advocates can mobilize on neighborhood protection grounds and displace the constraint. The reading may bifurcate: harm reduction continues in low-density areas but is replaced by prohibition in dense urban communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neighborhood_externality_acceptability_boundary, preference, 'Political acceptability boundary for neighborhood externalities from harm reduction sites.').

omega_variable(
    state_funding_contingency_and_political_reversibility,
    'If political coalitions shift and funding for harm reduction is withdrawn, how quickly does the constraint collapse and what is the transition path back to prohibition?',
    'Historical cases of policy reversal (e.g., Canada''s Safe Supply program reversals); modeling of institutional lock-in and budget dependency; interviews with practitioners about continuity requirements.',
    'If the constraint has high institutional lock-in and practitioner employment dependence, reversal would face organizational resistance and would be costly. If funding is contingent and institutional investment is shallow, reversal could be rapid and users would have minimal protection. This is partly distinct from mandatrophy — the constraint may functionally persist but be politically fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_funding_contingency_and_political_reversibility, empirical, 'Political reversibility of the harm reduction reading due to funding contingency.').

omega_variable(
    legalization_foreclosure_or_coexistence,
    'Is the harm reduction reading structurally foreclosed by legalization, or can they coexist in a hybrid model (legal supply + harm reduction services for continued harms)?',
    'Examination of jurisdictions that have legalized (cannabis legalization in Canada, heroin-assisted treatment in Switzerland alongside decriminalization) — do harm reduction services persist, expand, or decline after legalization? What is the institutional relationship?',
    'If legalization forecloses harm reduction (commercial supply replaces state services), the constraint''s sustainability depends on preventing legalization, which is a vulnerability to political shift. If they coexist, harm reduction has a longer structural shelf life. The reading_relations classification (forecloses vs. coexists_with) may be empirically resolvable through natural experiments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legalization_foreclosure_or_coexistence, empirical, 'Whether legalization reading forecloses or coexists with harm reduction.').

omega_variable(
    framing_as_disease_versus_behavior_control,
    'Is the characterization of drug use as a ''disease/epidemiology problem'' rather than a ''behavior control problem'' a genuine epistemic reframing or a rhetorical reframing that obscures continued state control? Does the disease frame make different structural choices than a behavior-control frame, or is it functionally equivalent with different labeling?',
    'Comparative analysis of prohibition-frame decision-making (enforcement targets, arrest criteria, punishment structure) versus disease-frame decision-making (service eligibility, clinical protocols, outcomes metrics). If the decision structures differ (disease frame triggers different institutional rules), the framing is structurally consequential; if decision-making is observably identical, the framing is rhetorical.',
    'If the disease frame is structurally consequential, users genuinely experience a different constraint. If it is rhetorical, the constraint is a relabeling of control that deepens extraction (users believe they are being treated, but are actually being managed under a new authority). This is a conceptual omega about whether the reading is a genuine alternative or a renewed form of the same control structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_as_disease_versus_behavior_control, conceptual, 'Whether the disease framing is structurally distinct from a behavior-control frame or rhetorical relabeling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t3, substance_control_authority__harm_reduction_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__harm_reduction_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__harm_reduction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__harm_reduction_reading, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(subs_be_t3, substance_control_authority__harm_reduction_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__harm_reduction_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__harm_reduction_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__harm_reduction_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(subs_su_t3, substance_control_authority__harm_reduction_reading, suppression_requirement, 3, 0.26).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__harm_reduction_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__harm_reduction_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__harm_reduction_reading, suppression_requirement, 25, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'substance_control_authority'. The prohibition_reading and legalization_reading are sibling constraints instantiating different structural arrangements from the same kernel commitment. All three readings share the referent (state authority over drug use) but instantiate different ε values, victim sets, and mechanisms. The harm reduction reading is influenced by both siblings: prohibition creates a political background condition (the threat of enforcement escalation); legalization creates institutional competition (if commercial supply emerges, state-managed services may be displaced). Neither completely forecloses harm reduction, but legalization reduces its institutional scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
