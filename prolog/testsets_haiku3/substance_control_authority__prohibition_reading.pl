% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminal Prohibition of Drug Use and Possession (Deterrence Reading)
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the PROHIBITION READING of the contested
 *   kernel 'substance_control_authority': state authority to criminalize drug
 *   use and possession justified by the need to protect third parties from
 *   drug-related crime and social disorder through deterrence. The reading's
 *   core premise is that criminalization reduces drug use via fear of
 *   punishment, thereby reducing harms to non-users and communities. Drug
 *   users are positioned as vectors of harm whose behavior must be deterred;
 *   the constraint treats addiction as a moral/criminal failure rather than a
 *   public health condition. The beneficiary group (non-user third parties
 *   seeking protection) is constructed as discrete and passive; the victim
 *   group (criminalized users) is constructed as criminal actors requiring
 *   incapacitation. The enforcement machinery is the state police and
 *   correctional apparatus. This reading coexists with the
 *   harm_reduction_reading (accepts use, minimizes health harms) and the
 *   legalization_reading (regulates markets as commerce) — all three remain
 *   live policy positions in different jurisdictions. The ε referent is the
 *   standing arrangement under the prohibition reading's own lights: how
 *   extractive is the criminalization system as assessed from within the
 *   deterrence/criminal-justice framing? The reading's endorsed alternative
 *   (harm reduction or legalization) is NOT the referent — ε describes the
 *   criminalization arrangement itself.
 *
 * KEY AGENTS:
 *   - State criminal authority (institutional): sets and enforces criminalization statutes; beneficiary via criminal justice budgets and authority
 *   - Drug users criminalized (powerless, identity_locked): bear criminal liability, incarceration, permanent record; structured victims
 *   - Low-income communities (organized, constrained): receive disparate enforcement burden while nominally protected; asymmetric extraction/benefit
 *   - Non-user third parties (powerful, mobile): benefit from claimed deterrent effect; concentrated beneficiary set
 *   - Law enforcement agencies (institutional, arbitrage exit): administer enforcement; benefit from budgets and mandate continuity
 *   - Medical/public health professionals (powerful, excluded): marginalized from policy-setting despite expertise in addiction
 *   - Racial minorities (moderate power, constrained exit): arrested at 2–4x rates despite similar use; racialized extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.68).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.79).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminal Prohibition of Drug Use and Possession (Deterrence Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236').
narrative_ontology:cs_kernel_codification('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', formalized).
narrative_ontology:cs_authority_grounding('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', lineage).
narrative_ontology:cs_interpretation_layer_present('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236').
narrative_ontology:cs_reading_relation('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', foundational, drug_use_deterrable_via_criminal_threat).
narrative_ontology:cs_axiom_status(drug_use_deterrable_via_criminal_threat, holdable).
narrative_ontology:cs_axiom_grounding('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', drug_use_deterrable_via_criminal_threat, empirically_contingent).
narrative_ontology:cs_axiom('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', foundational, state_duty_to_protect_non_users_via_criminalization).
narrative_ontology:cs_axiom_status(state_duty_to_protect_non_users_via_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', state_duty_to_protect_non_users_via_criminalization, deontological).
narrative_ontology:cs_reference_frame('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', deterrence_through_criminal_threat).
narrative_ontology:cs_drift_state('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', contemporary_harm_reduction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddac04a9-4d8b-4fdb-b73f-6dd5d31a5236', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, non_users_protected_third_parties).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users_criminalized).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, low_income_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, low_income_communities).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, incarcerated_population).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racial_minorities_especially_african_americans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces criminal statutes against drug possession and use. Justifies criminalization as necessary deterrence to protect neighborhoods, workplaces, and families from the social disorder and crime associated with drug markets and addiction. Administers enforcement through police, prosecutors, and correctional systems. Receives political support from constituencies that prioritize order and abstinence norms.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_criminal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Face criminal liability, arrest, incarceration, and permanent record consequences for drug possession and use. The constraint treats addiction itself as a criminal matter rather than a public health condition. Exit from the constraint requires either cessation (extremely high cost for those with physiological addiction) or geographic flight to non-enforcing jurisdictions (not practically available). Identity as drug-dependent becomes legally inscribed, creating a permanent barrier to employment, housing, and social reintegration.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users_criminalized, payer,
    powerless, biographical, identity_locked, national).

% Experience both extraction and coordination benefit: they bear disproportionate enforcement burden (racial disparities in arrest and incarceration rates documented across enforcement agencies), and they are nominally protected from drug-market crime and disorder. The protective coordination is unequally distributed; enforcement costs concentrate on residents of heavily policed neighborhoods while drug markets in affluent areas face lighter enforcement.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, low_income_communities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, low_income_communities, beneficiary).

% Benefit from reduced incidence of drug-related street crime, property crime, and visible drug markets in their neighborhoods and workplaces. The constraint's deterrent effect is claimed to reduce their risk of victimization and exposure to addiction and overdose among family members. Their benefit depends on the constraint's maintenance and enforcement. They can exit by relocating to neighborhoods with different enforcement profiles or by switching political allegiance to support alternative readings.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, non_users_protected_third_parties, beneficiary,
    powerful, biographical, mobile, national).

% Enforce criminalization through arrest and prosecution. Benefit from drug prohibition via criminal justice budgets, staffing justification, and asset seizure authority (which generates revenue). The constraint's continuation provides ongoing operational mandate and resource allocation. Their organizational survival depends on the persistence of drug criminalization as a policy objective.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary).

% Would argue for treating drug dependence as a chronic health condition rather than a criminal matter, and would advocate for evidence-based treatment access. They are structurally excluded from setting drug policy; criminalization constrains their ability to provide treatment (legal liability for providers, criminalizing access to medication-assisted treatment in some jurisdictions). Their expertise in addiction medicine is marginalized by the prohibition reading.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, medical_and_public_health_communities, excluded,
    powerful, biographical, constrained, national).

% Bears the highest extraction cost: physical liberty is removed, social ties are severed, employment opportunities are foreclosed by criminal record, family structures are destabilized. Many are locked into the constraint through repeat cycling via addiction and re-criminalization. Their exit from the constraint is structurally blocked by the severity of consequences and the addictive properties of the substances at issue.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, incarcerated_population, payer,
    powerless, biographical, trapped, national).

% Are arrested and incarcerated for drug offenses at rates 2–4x higher than white populations despite similar usage rates. This racialized enforcement pattern is a documented structural feature of drug prohibition, not incidental to it. Their communities bear a disproportionate share of criminalization costs while claiming to receive the same protective benefits as more affluent, less-policed communities — the distribution of extraction and protection is asymmetric by race.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racial_minorities_especially_african_americans, payer,
    moderate, generational, constrained, national).

% Would advocate for needle exchange, overdose prevention sites, and medication-assisted treatment as primary interventions to reduce the health harms of drug use. They are excluded from policy-setting because their acceptance of ongoing drug use conflicts with the prohibition reading's core premise of abstinence-through-deterrence. Some interventions they advocate for are actively criminalized under drug laws.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters drug use through criminal penalties; coordinates societal response to drug-related harm by establishing a unified legal prohibition backed by enforcement machinery; reduces visible drug markets and street-level drug dealing through police surveillance and incapacitation of dealers via incarceration.
% TRANSFER_FUNCTION: Transfers individual liberty, employment opportunity, and social standing from drug users and marginalized communities (especially African American and Latino communities subject to disparate enforcement) to the state (through control and incapacitation), to law enforcement (through resource allocation and authority), and to non-user populations (through reduced exposure to visible drug markets and associated crime).
% ABSENT_VOICES: Drug users themselves have extremely limited voice in the legislative process setting criminalization policy; harm reduction professionals are excluded from policy-setting; medical experts treating addiction are marginalized; racial justice advocates and communities experiencing disparate enforcement are excluded from designing enforcement systems.
% DISAPPEARANCE_RATIONALE: If criminal prohibition disappeared overnight, drug markets would shift from criminalized to regulated or tolerated status; incarceration rates would fall sharply, releasing hundreds of thousands; treatment systems would reorganize around public health models rather than criminal justice; racial disparities in enforcement would disappear; law enforcement budgets and authority would contract. The world would rearrange substantially.
% FOUNDING_PROBLEM: Drug-related street crime, visible drug markets, and disorder in neighborhoods; drug-driven overdose deaths and family destabilization; concern that unrestricted access to addictive substances harms the vulnerable and destabilizes communities.
% FOUNDING_PROBLEM_CORROBORATION: The state criminal justice system and law enforcement attest the founding problem remains live. Public health researchers, harm reduction advocates, and independent analysts attest the founding problem is partially addressed by prohibition but that criminalization has created additional harms (mass incarceration, racial disparities, barriers to treatment access) that exceed the gains. Multiple independent jurisdictional trials of decriminalization and harm reduction models report reduced overdose deaths and social disorder relative to criminalization, providing corroboration from outside the prohibition-supporting institutions.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the interval (0.48→0.68) because criminalization is maintained and expanded even as evidence of deterrent efficacy weakens; the constraint persists not because its founding problem demands it, but because institutional stakeholders (law enforcement, correctional agencies) benefit from its continuation. Suppression is high (0.79) and rising because maintaining the prohibition requires sustained police authority, incarceration, and active exclusion of alternative approaches (harm reduction sites, treatment access). Theater rises (0.22→0.41) because an increasing share of enforcement activity defends the prohibition itself against an accumulating empirical challenge rather than responding to organic drug-related disorder. Accessibility_collapse is moderate (0.62) because alternatives (legalization, harm reduction models) remain discursively available in some jurisdictions and in public discourse, even though individual drug users face near-total alternative collapse (identity_locked). Resistance is high (0.71) because harm reduction advocates, public health experts, racial justice movements, and affected communities actively resist the prohibition reading, generating sustained political conflict. The measurement grid reflects the constraint's trajectory from weaker coherence (lower extraction when the deterrence case was empirically plausible) to stronger extraction (as the deterrence case erodes but institutional stakeholders protect the arrangement). On a shared time grid: every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the state criminal authority seat: the constraint is coordination (deters crime, protects neighborhoods, organizes collective response to drug harms). From the drug-using seat: the constraint is pure extraction (criminalization for possession of a substance one uses; jail time; permanent record; no offsetting benefit). From the low-income community seat: the constraint is tangled—they are coordinated for protection AND extracted for enforcement, asymmetrically. From the non-user third-party seat: the constraint is rope (they get protection, pay nothing directly, coordinate via shared deterrent effect). The engine should surface these divergences through per-seat classification from the authored structural data. The perspectival gap is not uncertainty or disagreement about the facts; it is structural: the same rule produces different d values for different seats, leading to different type classifications. The claim does not resolve this; the engine measures it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim framing chains to the core premise of the prohibition reading: that deterrence protects third parties (hence they are beneficiaries), while drug users impose costs requiring criminal control (hence they are victims). The reading's epistemic foundation is that criminalization produces a net protective effect. But the empirical data from harm reduction jurisdictions, public health research, and criminological studies increasingly challenges that foundation: deterrence does not reduce drug use proportionally to incarceration rates; criminalization delays treatment access; incarceration increases overdose risk upon release. If the reading's foundational claim (deterrence works) is empirically falsified, the beneficiary/victim positioning collapses: third parties were not actually protected at the cost claimed, so they are not net beneficiaries. This frames the omegas below.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was drug-related crime and disorder. That problem remains contestably live (public health researchers say it is partly solved by other means, partly unsolvable via criminalization; criminal justice institutions say it is live). But the means by which prohibition addresses the problem have atrophied: criminalization does not produce the deterrent effect it was theoretically supposed to produce. Evidence from Portugal, certain U.S. jurisdictions, and Canada shows that decriminalization with harm reduction does not increase drug use and reduces overdose deaths. The constraint persists not because the founding problem demands it but because institutional stakeholders (state criminal authority, law enforcement) benefit from its maintenance and have the power to exclude alternatives from policy consideration. Mandatrophy is resolved: the constraint exhibits the classic signs of a zombie arrangement maintained by beneficiaries against the decay of its functional justification. The theater_ratio rise (0.22→0.41) models this atrophy: an increasing share of enforcement activity is devoted to defending the prohibition itself (excluding harm reduction sites, arresting people for treatment-access violations) rather than addressing the original problem. The six_questions mismatch corroborates this: founding_problem_status=contested + disappearance_verdict=world_rearranges together indicate institutional capture of a constraint whose function has eroded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_contested,
    'Does criminalization actually deter drug use at the population level, or does it merely incapacitate users through incarceration while not preventing use-initiation?',
    'Longitudinal comparison of drug-use prevalence rates in criminalization vs. decriminalization jurisdictions controlling for treatment availability, socioeconomic conditions, and drug supply factors. Randomized or quasi-experimental policy change studies measuring use rates before/after decriminalization.',
    'If deterrence is shown to be ineffective at preventing use, the beneficiary/victim framing collapses: third parties were not actually protected at the cost claimed, so the claimed beneficiary set is not empirically benefiting. This would shift the constraint from tangled_rope (coordination+extraction) toward snare (pure extraction with suppression). If deterrence is confirmed effective, the tangled_rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_contested, empirical, 'Whether the prohibition reading''s core claim (criminalization deters drug use) holds under empirical scrutiny.').

omega_variable(
    racial_disparities_structural_or_incidental,
    'Are the documented racial disparities in drug arrest and incarceration rates (African Americans arrested 2–4x higher than white populations despite similar use rates) a necessary feature of criminalization or an incidental/correctable artifact of enforcement practice?',
    'Analysis of enforcement patterns controlling for jurisdiction, officer demographics, and reported use rates; comparison of disparities across different policing models within criminalization; examination of whether race-blind enforcement algorithms or reforms reduce disparities.',
    'If disparities are structural to the criminalization approach (because it targets street-level markets where enforcement is visible, and these markets are racialized), then the victim set must be understood as racially constituted — low-income African American communities are not symmetrically protected and burdened, they are disproportionately extracted from. This shifts the classification toward snare (racially targeted extraction). If disparities are correctable through enforcement reform, the tangled_rope classification can be preserved by arguing reform would improve the distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparities_structural_or_incidental, empirical, 'Whether racial disparities in drug criminalization are incidental to or structurally required by the prohibition model.').

omega_variable(
    suppression_internalized_or_external,
    'Is the measured suppression (0.79) primarily external (police authority, legal barriers, risk of incarceration) or substantially internalized (drug users have internalized the shame, criminality, and social judgment such that suppression persists even when external enforcement is removed)?',
    'Post-decriminalization trajectory studies: if suppression drops sharply after external enforcement is removed, suppression is primarily external; if suppression persists (users continue to hide, avoid treatment, report shame even after decriminalization), suppression is partially internalized. Qualitative research with people who have exited the constraint via geographic migration or jurisdictional decriminalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the barrier to exit is more durable even if formal criminalization is removed. This would shift the exit_options designation for drug users from identity_locked toward constrained (harder to escape than legal categories alone suggest). The constraint''s persistence would depend less on active enforcement and more on psychological/social infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_or_external, empirical, 'Whether suppression operates structurally (external barriers) or psychologically (internalized shame and judgment).').

omega_variable(
    alternative_readings_foreclosure_or_coexistence,
    'Are the harm_reduction_reading and legalization_reading logically foreclosed by the core premises of the prohibition_reading, or do all three readings remain simultaneously holdable within different policy frameworks?',
    'Analysis of the foundational axioms: if the prohibition reading''s core claim is ''drug use must be criminally deterred,'' and the harm_reduction reading''s core claim is ''drug use should be accepted and harms minimized,'' these are logically incompatible within a single framework. But in a meta-framework allowing pluralistic jurisdictional experimentation, all three coexist. The question is whether any single polity must choose one reading or whether federation/subsidiarity permits coexistence.',
    'If readings forecast each other, the constraint''s classification is contingent on which reading''s framework is adopted: criminalization is rope under prohibition, snare under harm reduction, incoherent under legalization. If readings coexist, each constraint story (each reading) is valid per se, and the meta-question is how jurisdictions navigate the choice. This affects whether the kernel_context should position readings as forecloses vs. coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure_or_coexistence, conceptual, 'Whether sibling readings of substance_control_authority logically foreclose or coexist with the prohibition_reading.').

omega_variable(
    third_party_protection_efficacy,
    'Do non-user third parties actually receive proportional protection from drug-related crime and disorder via criminalization, or is their protection contingent on other factors (neighborhood investment, treatment availability, social services) that correlate with but are not caused by criminalization?',
    'Quasi-experimental analysis of neighborhoods that experienced decriminalization or harm-reduction policy changes, measuring crime rates, visible drug markets, and order outcomes. Causal pathway analysis isolating criminalization''s contribution from confounded factors.',
    'If third-party protection is primarily caused by criminalization, the beneficiary classification is supported. If protection correlates with socioeconomic investment and treatment availability more strongly than with criminalization, then the stated beneficiary group may not actually be benefiting from the constraint as claimed — it is benefiting from other arrangements that the prohibition reading obscures. This would shift the constraint toward snare (obscured extraction) or clarify that the constraint is extractive primarily from users while non-users derive benefits from orthogonal policies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_protection_efficacy, empirical, 'Whether non-user third parties derive measurable protection from criminalization independent of other community factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__prohibition_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__prohibition_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(subs_tr_t16, observed).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(subs_tr_t24, observed).
narrative_ontology:measurement(subs_tr_t32, substance_control_authority__prohibition_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(subs_tr_t32, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__prohibition_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__prohibition_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(subs_be_t16, observed).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(subs_be_t24, observed).
narrative_ontology:measurement(subs_be_t32, substance_control_authority__prohibition_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(subs_be_t32, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__prohibition_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__prohibition_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement_basis(subs_su_t16, observed).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement_basis(subs_su_t24, observed).
narrative_ontology:measurement(subs_su_t32, substance_control_authority__prohibition_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement_basis(subs_su_t32, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(subs_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel substance_control_authority. The harm_reduction_reading and legalization_reading are sibling constraints (separate JSON files) drawing from the same kernel but instantiating different authority structures and beneficiary/victim positions. All three are live policy positions in contemporary jurisdictions. They are linked via the network.affects_constraints array and via the cs_structure.reading_relations declarations (coexists_with for each sibling). Each reading has its own ε, derived from its own logic of what the standing arrangement IS; the readings do not compete on a single ε axis but rather on whether the arrangement's purpose (deterrence, harm reduction, regulation) is legitimate. Decomposition follows the ε-invariance principle: measuring the same arrangement (criminalization) from three reading perspectives yields three different ε values because the three readings assess the arrangement against different normative and empirical referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__prohibition_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
