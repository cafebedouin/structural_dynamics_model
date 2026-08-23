% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Authority to Criminalize Drug Use/Possession (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control authority asserts that
 *   criminalizing drug possession/use protects third parties by deterring
 *   drug-related crime and disorder. In operation, it functions as a tangled
 *   rope: a genuine coordination claim (deterrence of third-party harm) fused
 *   with massive asymmetric extraction (incarceration, racialized
 *   enforcement, resource transfer to carceral institutions). The constraint
 *   requires active enforcement at every level — police, prosecutors, courts,
 *   prisons — and its persistence depends on suppressing alternatives (harm
 *   reduction, regulated supply, decriminalization). The claimed coordination
 *   function is undermined by evidence: deterrence effects are minimal,
 *   racial disparities are extreme, and the constraint's own enforcement
 *   generates the violence and disorder it claims to prevent. The engine will
 *   compute per-seat types from the structural data: from the drug user seat
 *   this is a snare; from the law enforcement seat a rope; from the
 *   analytical seat a tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.87).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Authority to Criminalize Drug Use/Possession (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '8701c511-c528-48c0-9885-a8ff4282b6f6').
narrative_ontology:cs_kernel_codification('8701c511-c528-48c0-9885-a8ff4282b6f6', formalized).
narrative_ontology:cs_authority_grounding('8701c511-c528-48c0-9885-a8ff4282b6f6', extraction).
narrative_ontology:cs_interpretation_layer_present('8701c511-c528-48c0-9885-a8ff4282b6f6').
narrative_ontology:cs_reading_relation('8701c511-c528-48c0-9885-a8ff4282b6f6', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8701c511-c528-48c0-9885-a8ff4282b6f6', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('8701c511-c528-48c0-9885-a8ff4282b6f6', foundational, criminalization_necessary_for_public_order).
narrative_ontology:cs_axiom_status(criminalization_necessary_for_public_order, holdable).
narrative_ontology:cs_axiom_grounding('8701c511-c528-48c0-9885-a8ff4282b6f6', criminalization_necessary_for_public_order, instrumental).
narrative_ontology:cs_axiom('8701c511-c528-48c0-9885-a8ff4282b6f6', secondary, incarceration_justified_as_deterrence).
narrative_ontology:cs_axiom_status(incarceration_justified_as_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('8701c511-c528-48c0-9885-a8ff4282b6f6', incarceration_justified_as_deterrence, instrumental).
narrative_ontology:cs_reference_frame('8701c511-c528-48c0-9885-a8ff4282b6f6', mid_century_prohibition_consensus).
narrative_ontology:cs_drift_state('8701c511-c528-48c0-9885-a8ff4282b6f6', contemporary_mass_incarceration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8701c511-c528-48c0-9885-a8ff4282b6f6', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_parties_protected_from_drug_crime).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_institutions).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, prison_industrial_complex).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racially_marginalized_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, incarcerated_population).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, deterrence_theory).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, public_order_doctrine).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, supply_side_control_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalized for possession and use; face arrest, incarceration, loss of housing/employment/benefits, and lifelong collateral consequences. Identity-locked because criminal record and stigma fuse with self-concept; exit requires systemic decriminalization, not individual choice. Racial disparities mean Black and Latino users bear drastically higher enforcement intensity for similar usage rates.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, biographical, identity_locked, national).

% Experience concentrated enforcement (stops, searches, arrests) at rates far exceeding usage prevalence. Community trust eroded; families destabilized by incarceration; economic extraction via fines, fees, and lost labor. Trapped because geographic and economic segregation limits mobility, and political disenfranchisement limits democratic exit.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racially_marginalized_communities, payer,
    powerless, generational, trapped, regional).

% Directly confined in prisons/jails for drug offenses; subjected to forced labor, inadequate healthcare, and violence. Exit is structurally blocked by sentence length, parole barriers, and reentry collateral consequences. Overwhelmingly drawn from the two groups above.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, incarcerated_population, payer,
    powerless, biographical, trapped, national).

% General public purportedly protected from drug-related property crime, violence, and public disorder by deterrence. Benefit is diffuse and contested — evidence on deterrence efficacy is mixed, and many third parties (e.g., families of users) experience net harm. Mobile because they can relocate to lower-crime areas; benefit claim does not bind them to the constraint.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_parties_protected_from_drug_crime, beneficiary,
    organized, biographical, mobile, national).

% Set enforcement priorities, control discretionary resources (asset forfeiture, grant funding, overtime), and maintain institutional missions built around drug prohibition. Benefit from budgetary flows, mission expansion, and political capital. Arbitrage-grade exit: can pivot enforcement to other domains (terrorism, immigration, cyber) if drug war winds down.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_institutions, beneficiary).

% Private prison operators, correctional unions, commissary/telecom contractors, and allied legislators capture recurring revenue from drug-war incarceration. Lobby for mandatory minimums, truth-in-sentencing, and immigration detention expansion. Arbitrage exit: diversified across carceral and surveillance markets; drug prohibition is one revenue stream among many.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, prison_industrial_complex, beneficiary,
    powerful, generational, arbitrage, national).

% Advocate for harm reduction, treatment-on-demand, and decriminalization. Systematically excluded from drug policy decision-making tables (scheduling decisions, grant allocations, legislative hearings) where law enforcement dominates. Constrained exit: can build parallel services but cannot change the criminal legal framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    moderate, biographical, constrained, national).

% Sees the full structure: a constraint that claims coordination (deterrence) while extracting liberty, labor, and life-years from a racially defined underclass to sustain institutional budgets and political narratives. No material stake in the constraint's persistence or removal.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deter drug use and trafficking to reduce third-party harms: property crime by users seeking money for drugs, violence from illicit markets, public disorder from visible use, and overdose externalities on emergency services.
% TRANSFER_FUNCTION: Moves liberty (incarceration), labor (prison work, foregone earnings), public resources (enforcement, courts, corrections budgets), and political power (disenfranchisement) from drug users and racially marginalized communities to law enforcement institutions, the prison industrial complex, and the general public via the deterrence claim.
% ABSENT_VOICES: Drug users themselves (criminalized, stigmatized, silenced), harm reduction practitioners (marginalized in policy venues), families of incarcerated people (disenfranchised, geographically dispersed), and communities most impacted by enforcement (politically disempowered). Their absence is structural: the constraint's enforcement machinery depends on their exclusion.
% DISAPPEARANCE_RATIONALE: If criminalization vanished overnight: illicit markets would shift toward regulated supply (as with alcohol post-Prohibition); 400,000+ drug-war prisoners would need release/re-sentencing; $50B+ annual enforcement spending would be reallocated; police/community relations would transform; racial disparities in incarceration would collapse. The carceral state's current architecture would lose its primary fuel.
% FOUNDING_PROBLEM: Mid-20th century rise in heroin/cocaine use linked to urban crime spikes and social disorder; political consensus (bipartisan) that criminal prohibition was necessary to protect public order and health, codified in the 1970 Controlled Substances Act and escalated by the 1980s 'War on Drugs'.
% FOUNDING_PROBLEM_CORROBORATION: The founding consensus is attested by legislative histories (CSA 1970, ADAA 1986, Crime Bill 1994) and contemporary executive rhetoric. But the Kerner Commission (1968), the National Academy of Sciences (2001, 2019), the U.S. Sentencing Commission (multiple reports), and every major public health association (APHA, AMA, NAM) attest that the founding problem has been superseded: drug use persists, deterrence fails, racial disparities are documented, and public health approaches achieve better outcomes. No independent body corroborates that prohibition still solves its original problem.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the severity of liberty deprivation and collateral consequences for millions. Suppression (0.87) captures the active machinery: scheduling system, mandatory minimums, asset forfeiture, police militarization, and the systematic exclusion of health-based alternatives. Theater ratio (0.38) acknowledges that enforcement performs 'protecting communities' while actually generating harm — but the deterrence claim retains enough plausibility among beneficiaries to sustain the theater. Accessibility collapse (0.75) is high because the scheduling regime legally forecloses medical/research access and policy alternatives; resistance (0.68) is significant but fragmented across reform, abolition, and legalization movements. The measurement series (1970-2020) shows extractiveness and suppression rising through the drug war peak, then plateauing as reform pressure mounts; theater rises as the deterrence rationale frays.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (users, marginalized communities, incarcerated) experience this as a snare: pure extraction with no coordination benefit. The agenda-setter/beneficiary seats (law enforcement, prison complex) experience it as a rope: they coordinate enforcement and collect the gains. The third-party beneficiary seat experiences a claimed rope (they get protection) but the evidence suggests the protection is illusory — the engine's per-seat computation will reveal this divergence. The constraint is a tangled rope precisely because these seats are locked into the same structure with opposing directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Drug users and racially marginalized communities are full targets (d ≈ 0.9-1.0): they bear the extraction with trapped/identity-locked exit. Incarcerated population is the extreme target (d ≈ 1.0). Third parties protected are beneficiaries (d ≈ 0.1-0.2): they receive the claimed deterrence benefit with mobile exit. Law enforcement institutions are dual-positioned: agenda-setters who also benefit (budget, mission) — derived d ≈ 0.15 but structurally they shape the constraint, so override to d ≈ 0.25 captures their partial capture. Prison industrial complex are clear beneficiaries (d ≈ 0.1) with arbitrage exit. Public health advocates are excluded (no directionality). The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mid-century drug-crime surge) is dead/contested: drug use rates fluctuate independently of enforcement; crime dropped in the 1990s while incarceration rose; Portugal's decriminalization (2001) and U.S. state cannabis legalization show alternatives work. Yet the constraint persists because the institutions it built (DEA, state police drug units, prison systems, asset forfeiture) have become self-justifying — mandatrophy resolved into institutional self-preservation. The classification prevents mislabeling: calling this a pure snare would miss the genuine (if failed) coordination claim that sustains political legitimacy; calling it a rope would miss the racialized extraction that is its operational core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (prohibition_reading) of the contested kernel substance_control_authority. What structural elements do the sibling readings (harm_reduction_reading, legalization_reading) change?',
    'Compare the three readings'' beneficiary/victim sets, enforcement mechanisms, and epistemic warrants. The kernel is the state''s authority over substances; the readings instantiate different constraints with different ε, different victims, different types.',
    'If the kernel framing is accepted, the three readings are a constraint family linked by network.affects_constraints. If rejected, each reading stands alone. The engine''s cross-reading contamination analysis depends on this link.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committers structure: kernel identity and sibling reading deltas').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.87) primarily structural (legal barriers, police power, carceral infrastructure) or partially internalized (users/communities believe they deserve punishment, have internalized stigma, self-police)?',
    'Post-decriminalization suppression trajectory: in jurisdictions that decriminalized (Portugal, Oregon Measure 110, various cannabis states), measure whether community-level suppression persists via stigma, healthcare discrimination, and child-welfare involvement after legal barriers fall.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the target carries the constraint internally after formal exit. This would increase effective extraction for identity-locked seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in drug prohibition').

omega_variable(
    deterrence_efficacy_contested,
    'Does criminalization actually deter third-party harm (property crime, violence, disorder), or is the deterrence claim empirically false — making the coordination function a cover story?',
    'Natural experiments: compare jurisdictions with divergent enforcement intensity (e.g., cannabis legalization states vs. prohibition states; Portugal pre/post 2001; Netherlands coffee shop policy). Measure third-party harm outcomes (property crime, violent crime, public disorder calls) controlling for confounders.',
    'If deterrence is empirically near-zero, the coordination function is fictive and the constraint is a snare, not a tangled rope. If deterrence is real but small, the tangled rope classification holds but the coordination/extraction ratio shifts. This is the central empirical question for the constraint''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_efficacy_contested, empirical, 'Whether the claimed coordination function (deterrence) has empirical support').

omega_variable(
    racial_disparity_as_design_or_drift,
    'Are the extreme racial disparities in enforcement (Black Americans 3-4x arrest rate for similar usage) a designed feature of the constraint (intentional targeting) or an emergent property of ''neutral'' enforcement in a segregated society?',
    'Historical analysis of legislative intent (Congressional records, Nixon/Ehrlichman quotes, CSA scheduling criteria) vs. structural analysis of how ''neutral'' policing in segregated geography produces disparate impact. Both may be true at different levels.',
    'If designed, the constraint is a snare with racial extraction as purpose. If emergent, it is a tangled rope where the coordination function generates racialized extraction as a byproduct. The classification''s moral weight differs, but the engine''s structural classification may not — both produce high extraction on powerless seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_as_design_or_drift, conceptual, 'Origin of racial disparities in drug enforcement: design vs. structural emergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_prohibition_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(substance_control_prohibition_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(substance_control_prohibition_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(substance_control_prohibition_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(substance_control_prohibition_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(substance_control_prohibition_tr_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(substance_control_prohibition_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(substance_control_prohibition_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(substance_control_prohibition_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(substance_control_prohibition_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(substance_control_prohibition_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(substance_control_prohibition_be_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_prohibition_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(substance_control_prohibition_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(substance_control_prohibition_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(substance_control_prohibition_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(substance_control_prohibition_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(substance_control_prohibition_su_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This is the prohibition_reading of the substance_control_authority kernel. The three readings form a constraint family: they share the kernel (state authority over substances) but instantiate different constraints with different ε, different victim/beneficiary sets, and different types. The prohibition reading (this story) has high ε (0.78) and claims tangled_rope. The harm_reduction_reading would have lower ε (health interventions are less extractive) and likely claim rope or scaffold. The legalization_reading would have ε near market-regulation levels and likely claim rope. All three are linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
