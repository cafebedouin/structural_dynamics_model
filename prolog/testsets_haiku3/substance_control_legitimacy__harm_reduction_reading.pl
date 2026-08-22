% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Medicalized Substance Control via Harm Reduction Authority
 *   domain: public_health_policy
 *
 * SUMMARY:
 *   The harm-reduction reading of substance control establishes
 *   medical/public health authority over substance use policy, shifting the
 *   frame from criminalization ('drug users are criminals') to medicalization
 *   ('people with substance use disorder are patients'). This reading
 *   instantiates ONE constraint within a contested kernel: the same
 *   institutional space is read differently by prohibition advocates (who see
 *   medical harm-reduction as enabling addiction), legalization advocates
 *   (who see it as paternalistic medicalization), and harm-reduction
 *   practitioners (who see it as minimizing overdose death and disease
 *   without criminalization). This story generates the harm-reduction
 *   constraint only—the sibling readings are separate constraint stories,
 *   linked via network.affects_constraints. The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled_rope (coordination
 *   function + asymmetric extraction) while metrics show moderate
 *   extractiveness accumulating over time and increasing theatrical activity
 *   (treatment compliance, recovery narrative management) relative to core
 *   harm-reduction function (overdose prevention, disease reduction). Do not
 *   reconcile the claim to the metrics; that divergence is the measurement
 *   the corpus takes.
 *
 * KEY AGENTS:
 *   - public_health_authority: Sets the medical frame and administers treatment mandates; derives legitimacy from duty to minimize harm.
 *   - people_with_substance_use_disorder: Gain medical access and reduced criminalization; simultaneously bear mandatory treatment participation, medical surveillance, and identity lock into the 'patient' role.
 *   - informal_drug_market_participants: Persist despite the medical framing; encounter enforcement as 'untreated' or 'noncompliant' rather than 'criminal.'
 *   - legalization_advocates and prohibition_advocates: Structurally excluded from harm-reduction policy design; their voices would fragment the medical consensus.
 *   - medical_profession: Benefits from expanded authority, research funding, and prestige grounded in the medical frame.
 *   - analytical observer: Measures whether the constraint actually reduces criminalization or merely layers medical authority atop partial criminalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Medicalized Substance Control via Harm Reduction Authority").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'c026e1bf-5e83-469a-9d99-bf9c1115fc5e').
narrative_ontology:cs_kernel_codification('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', distributed).
narrative_ontology:cs_authority_grounding('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', extraction).
narrative_ontology:cs_interpretation_layer_present('c026e1bf-5e83-469a-9d99-bf9c1115fc5e').
narrative_ontology:cs_reading_relation('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', foundational, substance_use_medical_condition).
narrative_ontology:cs_axiom_status(substance_use_medical_condition, holdable).
narrative_ontology:cs_axiom_grounding('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', substance_use_medical_condition, empirically_contingent).
narrative_ontology:cs_axiom('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', foundational, medical_authority_necessary_harm_reduction).
narrative_ontology:cs_axiom_status(medical_authority_necessary_harm_reduction, holdable).
narrative_ontology:cs_axiom_grounding('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', medical_authority_necessary_harm_reduction, empirically_contingent).
narrative_ontology:cs_axiom('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', secondary, decriminalization_prerequisite_health_access).
narrative_ontology:cs_axiom_status(decriminalization_prerequisite_health_access, holdable).
narrative_ontology:cs_axiom_grounding('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', decriminalization_prerequisite_health_access, empirically_contingent).
narrative_ontology:cs_reference_frame('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', public_health_harm_minimization).
narrative_ontology:cs_drift_state('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', contemporary_policy_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c026e1bf-5e83-469a-9d99-bf9c1115fc5e', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_treatment_infrastructure).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, medical_profession).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, informal_drug_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_research_community).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the medical frame for substance use policy. Defines treatment standards, funding priorities, and compliance measures. Derives institutional legitimacy and budget authority from the frame. Can shift to a different reading (prohibition or legalization) only through political processes outside the authority's control; from its internal perspective, it has no exit.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Gains expanded authority over substance use diagnosis, treatment, and management. Expands medical practice scope and prestige. Can continue practicing under alternative readings (prohibition, legalization) but loses this authority expansion and associated funding. Has higher exit options than the public health authority because medical practice has intrinsic value independent of the policy frame.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, medical_profession, beneficiary,
    institutional, generational, mobile, national).

% Gain reduced criminal prosecution risk and access to harm-reduction services (overdose prevention, medication-assisted treatment, disease screening). Simultaneously bear mandatory or incentivized treatment participation, medical surveillance through treatment records, loss of choice about which treatment modality to pursue, and internalized identity as a 'patient in recovery.' Exiting the system requires abandoning the patient identity and the (genuine) harm-reduction benefits that identity provides—making exit extremely costly. This is the canonical identity-locked exit case: the exit is theoretically available but practically requires dismantling the self-concept the system has constructed.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder, beneficiary).

% Supply substances outside the formal treatment system. Encounter enforcement for supply-side crimes (distribution, trafficking, possession for sale) and for failing to use or comply with formal treatment. Are classified as 'untreated' rather than 'criminal' in the harm-reduction frame, but enforcement against them continues—undercover operations, asset seizure, imprisonment for supply crimes. Cannot exit by complying with the treatment mandate because their role is supply, not demand; they have no 'noncompliance' pathway to legitimacy within the system.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, informal_drug_market_participants, payer,
    powerless, immediate, trapped, local).

% Do not accept the medical framing or treatment mandate. May face coercive treatment under civil commitment or mental health law, or may remain in the informal market and criminal justice system. Their voice—that autonomy over substance use should not be conditional on medical compliance—is systematically excluded from harm-reduction policy conversations, even though their situation determines whether the policy is actually medicalizing or just adding a medical layer atop criminal control.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_refusing_treatment, excluded,
    powerless, immediate, trapped, local).

% Hold that substance use is inherently harmful and state authority derives from moral duty to prevent use through criminal deterrence and incapacitation. Their view is excluded from harm-reduction policy design (which presupposes the medical frame is legitimate), though they retain electoral and legislative influence in many jurisdictions and can advocate for prohibition-side policy shifts. Their exit from the harm-reduction frame is constrained by the political cost of defending criminalization in the face of documented harms (overdose death, mass incarceration).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    moderate, generational, constrained, national).

% Hold that competent adults have autonomy over substance use and state authority should be limited to preventing third-party harm through market regulation. Their view is excluded from harm-reduction policy design (which presupposes medical authority is necessary), though they have growing organized influence through advocacy groups, research networks, and some policy experiments (Portugal, Switzerland). Their exit from the harm-reduction frame is constrained by the political cost of defending full legalization against medical and prohibition framing.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, global).

% Benefits from research funding, career advancement, publication opportunities, and institutional prestige grounded in the harm-reduction frame. Defines what counts as 'evidence' (overdose death reduction, disease transmission reduction, treatment engagement) and what is not measured (autonomy loss, treatment coercion, black-market persistence). Their research findings feed back into policy legitimation and funding decisions. Can practice research under alternative frames (prohibition outcome measurement, legalization policy analysis) but loses this research paradigm and associated funding.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_research_community, beneficiary,
    organized, generational, mobile, global).

% Retains enforcement authority for supply-side crimes and violation of treatment conditions. Bears the cost of partial decriminalization (reduced drug convictions, fewer incarcerated people for possession for personal use) while continuing to enforce supply-side prohibitions. Is positioned as an observer of the medical system's success in diverting people from criminal justice to treatment, but also witnesses intensification of enforcement against informal market participants who do not access or comply with treatment. Its exit from the system is constrained by constitutional and statutory obligations to enforce law.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, observer).

% Measures the constraint's actual operation: whether it medicalized substance use (reduced criminalization, increased treatment access, improved health outcomes) or layered medical authority atop partial criminalization, creating a dual system that extracts from both treatment participants and informal market actors. Examines whether the founding problem (overdose death, criminalization harm) is actually being solved or merely being reframed. Remains outside the policy system and all the readings; can adopt any reading or none.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, public_health_authority).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes substance use management under a public health mandate: overdose prevention, disease transmission reduction, and treatment infrastructure are coordinated through a medical authority rather than fragmented across individual providers, criminal justice, and informal mutual aid.
% TRANSFER_FUNCTION: Moves people with substance use disorder from criminal justice system into medical/treatment system, shifting agency and authority from law enforcement to medical professionals. Simultaneously transfers social authority and institutional legitimacy to the medical frame, and research funding to the institutions that validate the frame.
% ABSENT_VOICES: People who refuse the medical identity ('I use substances on my own terms'); prohibition advocates who would frame substance use as moral failure requiring criminal deterrence; legalization advocates who frame it as autonomy requiring market regulation. Their exclusion is structural to the harm-reduction reading—it presupposes the medical framing is the legitimate frame.
% DISAPPEARANCE_RATIONALE: If harm-reduction authority and medical gatekeeping vanished, substance use policy would reorganize around one of the sibling readings—either expanded criminalization, legalization/regulation, or fragmented informal management. The current coordination structure (unified medical authority over access, treatment, and harm reduction) would collapse; treatment infrastructure would be repurposed or abandoned; autonomy claims and market-regulation arguments would resurface in policy.
% FOUNDING_PROBLEM: Criminalization of substance use produces overdose death, disease (HIV, hepatitis C), incarceration of people with addiction, and persistent black markets. Medical authority can reduce these harms without the collateral damage of criminal incarceration.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and harm-reduction advocates attest the founding problem (overdose death, disease, incarceration) is live and harm reduction has reduced it. Legalization advocates contest whether the founding problem justifies medical authority over autonomy. Prohibition advocates contest whether harm reduction actually solves the problem (pointing to persistent drug use, black markets, and treatment non-compliance). External corroboration comes from epidemiological data on overdose and disease trends (supporting the harm-reduction reading on mortality metrics) but NOT from research on autonomy loss or treatment coercion (which is systematically under-measured in harm-reduction literature).
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.62 over the interval because harm-reduction mandates—treatment participation, medication requirements, surveillance through medical records, coercive treatment of 'noncompliant' individuals—accumulate even as the frame emphasizes 'helping' rather than 'punishing.' The constraint extracts autonomy and identity flexibility from people in treatment, while generating institutional rents for the medical system and research infrastructure. Suppression plateaus at 0.58 because the medical frame reduces overt coercion (no criminal incarceration for use) but retains underlying force (civil commitment, treatment mandates, loss of medical privacy). Theater ratio rises from 0.28 to 0.41, then plateaus: 'recovery narrative' management, 'treatment compliance' metrics, and 'successful integration' stories absorb an increasing share of effort relative to the core overdose-prevention function. The constraint persists because the medical authority benefits from it (career, legitimacy, research funding) and because the alternative frames (legalization, prohibition) are more politically costly to defend or implement. Resistance is high (0.72) because people with substance use disorder, informal market participants, prohibition advocates, and legalization advocates all have reasons to resist—though their resistance operates through different mechanisms (non-compliance, black-market persistence, electoral pressure, patient advocacy).
 *
 * PERSPECTIVAL GAP:
 *   Authority-seat (public_health_authority) versus target-seat (people_with_substance_use_disorder): the authority reads the constraint as liberation (decriminalization, medical access); the target reads it as conditional liberty (freedom from prison requires medical compliance). Exclusion-seat (legalization_advocates, prohibition_advocates) versus consensus-seat (harm-reduction coalition): the excluded seats read the medical frame as illegitimate paternalism or insufficient protection; the consensus seats read it as evidence-based policy. The engine's per-seat computation should surface these gaps from the structural directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   The public_health_authority is the clear agenda-setter and derives authority from the medical frame (d near 0.0, beneficiary-facing). People_with_substance_use_disorder are identity-locked into the 'patient' role—they can exit the system only by abandoning the treatment identity and the (genuine) harm-reduction benefits it carries, making them high-d targets (near 1.0) despite nominal 'beneficiary' language. Informal market participants are fully captured by enforcement (d approaching 1.0, full target). Legalization and prohibition advocates are excluded, so their directionality is asymmetric (they have views about the constraint but no structural position in it). The medical profession has organized power, mobile exit options (can practice under any policy frame), and benefits from the medical authority—d near 0.0, secondary beneficiary. This structure produces the tangled_rope signature: genuine coordination function (centralized harm reduction, disease prevention) paired with asymmetric extraction (enforcement from people_with_substance_use_disorder and informal market participants, institutional rents to medical system and researchers).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure snare classification because its founding problem (overdose death from criminalization) is genuinely live and unresolved—harm reduction has demonstrably reduced overdose death and disease transmission compared to pure criminalization. However, the theater-ratio rise (from 0.28 to 0.41) and the identity-lock mechanism (people_with_substance_use_disorder cannot exit treatment without abandoning their social identity and institutional supports) suggest mandatrophy risk: the constraint began as genuine coordination (solve overdose death through medical access) but increasingly operates as identity-capture-and-management (people are locked into the 'patient' identity and 'recovery' narrative, whether or not the actual medical function is advancing). The measurement series should trigger the mandatrophy detection system: if theater_ratio continues to rise while core outcomes (overdose death, disease transmission) flatten or worsen, the constraint has crossed into piton territory (performance without function). Right now it sits at tangled_rope: real coordination + real extraction, both live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_authority_legitimacy,
    'Is the medical framing a legitimate expression of state duty to minimize harm, or is it paternalistic capture of individual autonomy by medical institutions?',
    'Post-intervention autonomy measures: if people exiting the medical system report that their autonomous capacity and self-determination were enhanced by the transition from criminal to medical status, the medical framing is legitimate. If they report autonomy loss (treatment coercion, identity lock, surveillance) as offsetting the harm-reduction benefit, the framing is paternalistic. Also: whether legalization-style regulatory systems (where adults manage substance use outside medical authority) achieve comparable or better harm-reduction outcomes.',
    'If medical authority is paternalistic, the constraint reclassifies from tangled_rope (legitimate coordination + extraction) toward snare (illegitimate extraction wearing a medical mask). If medical authority is legitimate, it remains tangled_rope with justifiable extraction as the cost of medical coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_authority_legitimacy, conceptual, 'Whether state medical authority over substance use is legitimately minimizing harm or illegitimately imposing medical paternalism.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.58) structural (external constraints on exit, treatment mandates, enforcement against noncompliance) or internalized (people have internalized the medical identity and police themselves)?',
    'Post-exit trajectory: if people who leave the treatment system and abandon the medical identity report that the suppression persists (they continue to police themselves, report shame about non-compliance, reconstruct the patient identity), suppression is partially internalized. If suppression drops after exit, it is purely structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—it persists in the target''s behavior even after the external constraint is removed, indicating deeper identity lock. If purely structural, the suppression is confined to the active enforcement period and does not travel with the target after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the measured suppression (treatment mandates, surveillance, enforcement) is structural or internalized in the target''s self-policing.').

omega_variable(
    black_market_persistence_mechanism,
    'Does the black market persist because criminalization is partial (supply-side crimes still carry prison risk), because demand is inelastic (people continue using despite treatment availability), because treatment is coercive (people prefer informal access to autonomous access), or because of all three?',
    'Comparative analysis across jurisdictions with different policy mixes: full legalization (Canada, Portugal), harm reduction without supply decriminalization (UK), and prohibition (most of the US). If black markets shrink only with supply legalization, the driver is criminalization of supply. If they shrink only with demand decriminalization + treatment access, the driver is demand elasticity. If they persist under all conditions, the driver is autonomous preference or treatment coercion.',
    'If supply criminalization is the driver, the constraint''s extraction from informal market participants is avoidable without abandoning harm reduction—policy change without type change. If demand inelasticity is the driver, the extraction is structurally necessary (some people will not use formal treatment). If treatment coercion is the driver, the extraction is an artifact of the medical authority''s enforcement choices—policy change without constraint change. This affects whether informal market participants should be reclassified from victims to beneficiaries (if they actively prefer informal access) or remain victims (if they are trapped by criminalization or coercive treatment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence_mechanism, empirical, 'What drives the persistence of informal drug markets despite harm-reduction policies: supply criminalization, demand inelasticity, or treatment coercion.').

omega_variable(
    theater_ratio_rise_interpretation,
    'Does the rise in theater_ratio (from 0.28 to 0.41) reflect increasing performative activity around treatment compliance and recovery narratives (piton drift), or increasing need for legitimacy maintenance as the constraint''s extractive character becomes more visible?',
    'Content analysis of policy documents, training materials, and institutional narratives over the interval: if the proportion of effort devoted to ''recovery story'' management, ''treatment success metrics,'' and ''patient compliance'' is rising while core outcomes (overdose death, disease transmission) are flat or declining, the rise is performative (piton drift). If the rise correlates with increasing political contestation (legalization and prohibition advocates gaining visibility), the rise is legitimacy maintenance (structural response to challenge).',
    'If performative, the constraint is drifting toward piton (same extraction, less genuine function). If legitimacy-maintenance, the constraint remains tangled_rope but under increasing political pressure—one of the sibling readings may be displacing it. This affects whether the measurement series should trigger mandatrophy warnings (performative rise) or policy-contestation warnings (legitimacy-maintenance rise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_rise_interpretation, empirical, 'Whether the rising theater_ratio reflects piton drift (increasing performativity without function) or structural response to increasing political contestation.').

omega_variable(
    reading_foreclosure_empirical_challenge,
    'If empirical evidence accumulates that legalization-style regulatory systems achieve equivalent or superior harm reduction outcomes without medical paternalism, does the harm-reduction reading''s empirical grounding collapse, forcing a reconsideration of the reading''s legitimacy?',
    'Long-term comparative outcome data from jurisdictions experimenting with legalization (Switzerland, Canada, Portugal) versus harm-reduction medicalization (UK, many EU countries, some US states). If legalization achieves comparable overdose prevention, disease reduction, and superior autonomy outcomes, the empirical grounds for harm-reduction reading''s medical authority axiom are undermined.',
    'If the empirical challenge is substantial and acknowledged, the harm-reduction reading''s foundational axiom (medical authority is necessary for harm reduction) becomes overridden in the reading''s own tradition. The reading would need to reformulate: either abandon medical authority while retaining harm reduction (converging toward legalization), or abandon harm reduction as the goal (converging toward prohibition). This would be a cs_structure.axiom status change from holdable to overridden, computed via axiom_overriding drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical_challenge, empirical, 'Whether the empirical claim grounding the harm-reduction reading''s medical authority axiom (that medical authority is necessary for harm reduction) will be challenged by legalization-outcome data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(subs_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(subs_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(subs_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three structurally distinct constraints, each instantiating a different reading of how state authority should be grounded and exercised over substance use. This reading (harm_reduction_reading) is distinguished by its claim that medical/public-health authority derives from duty to minimize harm without criminalization. The prohibition_reading grounds authority in criminal deterrence of use. The legalization_reading grounds authority in individual autonomy and market regulation. Each reading has its own ε (harm reduction shows moderate extractiveness via treatment mandates; prohibition shows high extractiveness via enforcement and imprisonment; legalization shows low extractiveness via market regulation with limited state role). They coexist as live positions in contemporary policy discourse—none logically forecloses the others, though each influences the structural conditions under which the others operate. All three stories are linked by network.affects_constraints edges forming a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
