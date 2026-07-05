% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Harm Reduction Reading of Substance Control Authority
 *   domain: public health / criminal justice / political economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-reduction reading of the contested
 *   substance_control_authority kernel: the state retains authority over drug
 *   use but exercises it through public health services and decriminalization
 *   rather than criminal punishment (the prohibition_reading) or regulated
 *   legal commerce (the legalization_reading). Users exit the criminal victim
 *   set but remain partially exposed to health harms because the underlying
 *   supply chain stays illicit and unregulated — this is the structural delta
 *   that makes this reading a distinct constraint from its siblings rather
 *   than a different measurement of the same one. Declining extraction and
 *   suppression over the interval reflect the trajectory of jurisdictions
 *   (Portugal, British Columbia) as harm reduction infrastructure matures and
 *   enforcement of possession recedes, while theater ratio rises modestly as
 *   some jurisdictions layer symbolic compliance requirements (mandatory
 *   counseling referrals, administrative panels) onto services without
 *   materially changing outcomes.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: primary beneficiary of decriminalization, partial victim of persistent health harm (powerless/trapped) — exits criminal victim set but not health victim set
 *   - public_health_agencies: agenda_setter administering the framework, institutionally invested in the harm-reduction middle position specifically
 *   - harm_reduction_service_providers: organized beneficiary/agenda_setter whose survival depends on this reading persisting rather than resolving toward either sibling
 *   - unregulated_supply_dependent_users: bears the specific harm this reading does not resolve — supply-side risk — distinguishing it sharply from legalization_reading
 *   - residents_near_service_sites: payer bearing concentrated local disorder costs
 *   - researchers_and_epidemiologists: analytical observer providing corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.38).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Harm Reduction Reading of Substance Control Authority").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public health / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '0cd6e11c-e76d-4d84-ab2b-bf93a723f92a').
narrative_ontology:cs_kernel_codification('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', distributed).
narrative_ontology:cs_authority_grounding('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', distributed).
narrative_ontology:cs_reading_relation('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', foundational, drug_use_is_health_condition_not_crime).
narrative_ontology:cs_axiom_status(drug_use_is_health_condition_not_crime, holdable).
narrative_ontology:cs_axiom_grounding('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', drug_use_is_health_condition_not_crime, empirically_contingent).
narrative_ontology:cs_axiom('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', foundational, decriminalization_sufficient_without_market_legalization).
narrative_ontology:cs_axiom_status(decriminalization_sufficient_without_market_legalization, holdable).
narrative_ontology:cs_axiom_grounding('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', decriminalization_sufficient_without_market_legalization, instrumental).
narrative_ontology:cs_reference_frame('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', prohibition_era_criminal_enforcement_baseline).
narrative_ontology:cs_drift_state('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', post_overdose_crisis_policy_reform, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0cd6e11c-e76d-4d84-ab2b-bf93a723f92a', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, surrounding_communities).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, residents_near_service_sites).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, unregulated_supply_dependent_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, harm_reduction_reduces_mortality).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, decriminalization_reduces_criminal_justice_contact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit the criminal victim set — no longer arrested or incarcerated for possession or use — and gain access to needle exchanges, supervised consumption sites, naloxone, and drug-checking services without a criminal record attached. But they remain exposed to the health harms of substance use itself: overdose risk from an unregulated, often adulterated supply persists because the drug market itself is not legalized. They cannot fully exit dependency, and access to services is geographically and administratively uneven.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Administer harm reduction infrastructure: needle exchanges, supervised consumption sites, overdose prevention, drug-checking. They set eligibility, siting, and funding priorities, and defend the framework against political pressure to revert to prohibition or accelerate to legalization. Their authority and budgets are legitimated by falling overdose deaths and disease transmission rates, which creates institutional stake in the harm-reduction framing specifically rather than either sibling reading.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, regional).

% Operate the clinics, outreach programs, and consumption sites that constitute the primary mechanism of this reading. They receive public and philanthropic funding tied to the decriminalization-plus-services model, and their organizational survival depends on the framework persisting in its current, partial form rather than resolving toward either full prohibition or full legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, agenda_setter).

% Benefit from reduced disease transmission (HIV, hepatitis C), fewer improperly discarded needles, and lower overdose-death burdens on emergency services when harm reduction infrastructure is well-sited and funded. This is a genuine coordination gain shared broadly across the population, not captured by a single actor.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, surrounding_communities, beneficiary,
    organized, generational, constrained, regional).

% Bear concentrated, localized costs — visible public drug use, discarded paraphernalia, and perceived disorder near supervised consumption sites and outreach hubs — that the decriminalized-but-not-legalized framework does not fully resolve, because use remains illegal-adjacent and unregulated even though possession is not prosecuted. Their objections are frequently overridden by siting decisions made at the agency level.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, residents_near_service_sites, payer,
    moderate, biographical, constrained, local).

% Because the reading decriminalizes use and possession without legalizing and regulating the supply chain, this subgroup continues to source drugs from an illicit, unpredictable market — fentanyl contamination, inconsistent potency, and no quality control. Harm reduction services (drug checking, naloxone) mitigate but do not eliminate this exposure; the underlying health harm this reading is named for persists structurally.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, unregulated_supply_dependent_users, payer,
    powerless, immediate, trapped, local).

% Lose the possession-arrest enforcement lever they held under prohibition but retain responsibility for trafficking, public disorder, and crime adjacent to drug markets. They occupy an ambiguous position: partially relieved of low-level enforcement burden, but tasked with managing disorder externalities the harm-reduction framework does not directly address.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, observer).

% Argue the framework tolerates ongoing criminal-market activity and normalizes use without addressing supply. They are politically present in legislative debate but structurally excluded from administering the harm-reduction apparatus once decriminalization is enacted; their preferred framework (prohibition_reading) is a rival constraint, not a component of this one.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, biographical, analytical, national).

% Argue that decriminalization without regulated legal markets leaves users dependent on an unsafe illicit supply and forfeits tax and quality-control benefits. They are excluded from shaping supply-side policy under this reading, which deliberately stops short of legalizing commerce.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, biographical, analytical, national).

% Study overdose mortality, disease transmission, and criminal-justice-contact outcomes across jurisdictions that have adopted this reading versus its siblings, providing the empirical basis (or counter-evidence) other seats invoke to defend or attack the framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, researchers_and_epidemiologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose deaths and disease transmission by removing criminal penalties for use/possession and substituting public health services (needle exchange, supervised consumption, drug checking, naloxone distribution) as the state's primary point of contact with people who use drugs.
% TRANSFER_FUNCTION: Moves resources from general public health and social-service budgets toward harm reduction infrastructure; moves risk of arrest and incarceration away from users; does NOT move supply-chain risk away from users, since the drug market remains illicit and unregulated. Diffuse disorder costs move toward residents near service sites.
% ABSENT_VOICES: Prohibition advocates and legalization advocates are both structurally outside the administering apparatus once this reading is enacted — the framework occupies a middle position that satisfies neither sibling reading's constituency fully. Residents near service sites are formally consulted in siting processes but frequently have their objections overridden by agency-level public health priorities.
% DISAPPEARANCE_RATIONALE: If decriminalization and the associated service infrastructure vanished overnight, people who use drugs would re-enter the criminal justice system for possession, service providers would lose their operating mandate and funding, overdose deaths and disease transmission would rise measurably (per pre-decriminalization baselines), and law enforcement would resume possession-level arrests. The arrangement is load-bearing for a specific, measurable population of service users and providers.
% FOUNDING_PROBLEM: Overdose deaths, HIV/HCV transmission from needle sharing, and mass incarceration for low-level possession under prohibition were producing high mortality and social costs without reducing drug use; the founding problem was to reduce these harms without waiting for or requiring full market legalization.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological research (outside both the service-provider and law-enforcement beneficiary sets) in jurisdictions with supervised consumption sites and decriminalization (e.g., Portugal, British Columbia, Vancouver's Insite) corroborates reduced overdose mortality and disease transmission, attested by peer-reviewed public health literature and government audit bodies not directly funded by the harm-reduction service providers themselves. Prohibition advocates dispute that the founding problem is adequately addressed, citing persistent public disorder and unregulated-supply deaths.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).
:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, declining) is moderate-low: the framework's coordination function (reducing mortality and disease transmission) is real and substantial, and the extraction that remains is concentrated in supply-side health harm that the reading structurally does not address rather than in rent extraction by an administering party. Suppression (0.42, declining) reflects reduced but not eliminated coercive machinery — decriminalization removes criminal penalties for possession but police and municipal authorities retain enforcement tools against public use, loitering, and trafficking adjacent to the decriminalized zone. Theater ratio (0.28, rising slightly) captures growing administrative overlay (mandatory service-linkage requirements, compliance panels) that does not always track health outcomes. Accessibility collapse is moderate (0.35) — real alternatives (full legalization, return to prohibition) remain politically live and contested, unlike a mountain where alternatives are foreclosed. Resistance is substantial (0.55) — both prohibition and legalization advocates actively contest this middle position from opposite directions.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-agency and service-provider seats, this reading looks like successful, functioning coordination — mortality down, disease transmission down, incarceration down. From the unregulated-supply-dependent user seat, the same structure looks like partial relief riding on top of an unresolved core harm: the illicit supply chain that produces overdose risk in the first place. The engine's per-seat computation should reflect this divergence rather than resolve it toward one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs occupy a genuinely split directionality: as beneficiaries of decriminalization they move toward the subsidized end, but as bearers of unregulated-supply health risk they remain exposed toward the target end — this is why they appear in both beneficiaries and victims arrays, reflecting the reading's core structural delta from its siblings. Public health agencies and service providers sit at low d (structural beneficiaries administering and legitimated by the framework). Residents near service sites and unregulated-supply-dependent users sit at higher d, bearing costs the framework does not fully resolve. This asymmetry — genuine coordination benefit for the broad population, concentrated unresolved harm for a specific subgroup — is what makes tangled_rope the structurally accurate claim rather than pure rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass incarceration and preventable death under prohibition) remains substantially live per epidemiological corroboration, which argues against mandatrophy — this is not a hollowed-out mandate persisting past its function. However, the specific mechanism (decriminalization without legalization) is defended by service providers and agencies partly because it preserves their administrative and funding role, distinct from the underlying public health goal, which is the seam where reversion toward tangled_rope from a purer rope framing is structurally justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_side_harm_irreducibility,
    'Is the residual health harm borne by unregulated-supply-dependent users an irreducible cost of stopping short of legalization, or a policy failure specific to under-resourced harm-reduction implementation that could be closed without moving to full legalization?',
    'Comparative analysis of overdose mortality trends in jurisdictions with well-funded drug-checking and safe-supply pilot programs (which partially substitute for legalization) versus jurisdictions with decriminalization alone.',
    'If irreducible, this reading has a structural ceiling on harm reduction that only the legalization_reading can break through, strengthening the case that these are genuinely distinct constraints rather than points on a spectrum. If reducible, the gap between this reading and legalization_reading narrows and some of its extraction could be treated as remediable rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_harm_irreducibility, empirical, 'Whether residual supply-side health harm is structural to this reading or a fixable implementation gap.').

omega_variable(
    administrative_capture_of_harm_reduction_apparatus,
    'Do service-provider and agency incentives to preserve program funding and institutional relevance create drift toward maintaining decriminalization-without-legalization even after evidence might favor full legalization?',
    'Track whether public health agencies and funded service providers publicly support or oppose legalization ballot initiatives or legislation in jurisdictions where they operate, and whether that position correlates with funding structure.',
    'If agencies/providers systematically oppose legalization for funding-preservation reasons independent of the evidence, this reading has a self-perpetuating administrative core that pushes it toward tangled_rope more strongly than the underlying health evidence would justify on its own.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_capture_of_harm_reduction_apparatus, conceptual, 'Whether institutional self-preservation, not evidence, sustains this reading''s middle position.').

omega_variable(
    kernel_framing_disagreement_location,
    'Where exactly does the disagreement between this reading and the prohibition_reading sit — is it about the state''s underlying authority claim (does the state have the right to criminalize) or purely about the empirical means-end question (does criminalization reduce harm)?',
    'Analyze whether prohibition advocates would accept decriminalization if shown clear evidence of net harm reduction, or whether their objection is independent of empirical outcomes (i.e., a deontological claim about drug use itself).',
    'If the disagreement is purely empirical, the readings could in principle converge on evidence; if it is a foundational deontological disagreement about state authority and moral status of drug use, the readings are permanently coexisting rather than potentially convergent, which affects how the reading_relations should be interpreted going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_disagreement_location, conceptual, 'Whether the prohibition/harm-reduction split is empirical or foundational-normative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__harm_reduction_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__harm_reduction_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__harm_reduction_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the substance_control_authority kernel (harm_reduction_reading, prohibition_reading, legalization_reading). Each reading has a distinct beneficiary/victim structure and ε profile: prohibition_reading criminalizes users and treats third-party protection as the coordination function; legalization_reading fully regulates the market and removes both criminal and much health-harm exposure by enabling quality control; this harm_reduction_reading occupies the middle position, removing criminal exposure while leaving supply-side health harm partially unresolved. The three are not the same constraint measured differently — they have different victim sets and different enforcement mechanisms — and are linked here per the ε-invariance decomposition principle rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
