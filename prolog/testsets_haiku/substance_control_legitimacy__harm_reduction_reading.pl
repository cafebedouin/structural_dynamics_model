% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Harm Reduction Authority: Medicalization of Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading locates substance control authority in public
 *   health expertise and mandates medicalization of substance use: users are
 *   offered treatment, harm reduction services, and decriminalization in
 *   exchange for accepting surveillance, compliance monitoring, and the
 *   patient identity. This reading claims to minimize harm without
 *   criminalization. The constraint's actual operation combines genuine
 *   coordination (treatment access improved, overdose deaths reduced) with
 *   extraction (institutional authority over substance users, budgets
 *   redirected to treatment providers, coercive treatment mandates, black
 *   markets persist). The reading asserts that authority derives from duty to
 *   minimize harm; the engine measures whether the constraint's operation
 *   tracks that duty or whether extraction is substantial enough to
 *   constitute a tangled rope or snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.62).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Authority: Medicalization of Substance Use").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'f4eec3ee-4d32-42e7-9f99-413205032394').
narrative_ontology:cs_kernel_codification('f4eec3ee-4d32-42e7-9f99-413205032394', distributed).
narrative_ontology:cs_authority_grounding('f4eec3ee-4d32-42e7-9f99-413205032394', expertise).
narrative_ontology:cs_interpretation_layer_present('f4eec3ee-4d32-42e7-9f99-413205032394').
narrative_ontology:cs_reading_relation('f4eec3ee-4d32-42e7-9f99-413205032394', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f4eec3ee-4d32-42e7-9f99-413205032394', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('f4eec3ee-4d32-42e7-9f99-413205032394', foundational, substance_use_medicalized_public_health_issue).
narrative_ontology:cs_axiom_status(substance_use_medicalized_public_health_issue, holdable).
narrative_ontology:cs_axiom_grounding('f4eec3ee-4d32-42e7-9f99-413205032394', substance_use_medicalized_public_health_issue, empirically_contingent).
narrative_ontology:cs_axiom('f4eec3ee-4d32-42e7-9f99-413205032394', foundational, state_authority_derives_from_harm_minimization_duty).
narrative_ontology:cs_axiom_status(state_authority_derives_from_harm_minimization_duty, holdable).
narrative_ontology:cs_axiom_grounding('f4eec3ee-4d32-42e7-9f99-413205032394', state_authority_derives_from_harm_minimization_duty, deontological).
narrative_ontology:cs_axiom('f4eec3ee-4d32-42e7-9f99-413205032394', secondary, criminalization_is_ineffective_and_harmful).
narrative_ontology:cs_axiom_status(criminalization_is_ineffective_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('f4eec3ee-4d32-42e7-9f99-413205032394', criminalization_is_ineffective_and_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('f4eec3ee-4d32-42e7-9f99-413205032394', public_health_expert_authority).
narrative_ontology:cs_drift_state('f4eec3ee-4d32-42e7-9f99-413205032394', contemporary_resistance_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f4eec3ee-4d32-42e7-9f99-413205032394', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_institutions).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_advocates).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, community_members_substance_free).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, law_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declared as the primary public health concern and target of intervention. They are offered treatment, harm reduction services (needle exchanges, naloxone distribution, medication-assisted therapy), and decriminalization of possession—genuine improvements over pure prohibition. However, they are also subject to mandatory treatment conditions, surveillance through drug testing and compliance monitoring, stigmatization as public health subjects, and coercive pressure to accept medicalized identity. Their ability to refuse treatment or self-manage without state oversight is limited by public health law and the medicalization framework itself.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, people_with_substance_use_disorder, beneficiary).

% Set the terms of substance control through public health authority rather than criminal law. They define what counts as 'harm,' what interventions are 'evidence-based,' and who gets mandatory versus voluntary treatment. They collect budgets, professional authority, and institutional legitimacy from managing the substance use population as a medical/public health problem. Their power derives from the reading that positions them as experts in harm minimization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Expand their market, professional authority, and funding through the medicalization framework. Substance use treatment is a growth sector; harm reduction expands the treatment paradigm beyond abstinence-only models to include medication-assisted therapy, supervised consumption sites, and ongoing management. They benefit from both government contracts and the expansion of treatment as a normalized state intervention.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    powerful, biographical, mobile, national).

% Gain institutional legitimacy and resources by framing substance use as a public health emergency requiring their expertise and intervention. They benefit from the shift away from pure criminalization and gain policy influence, funding for programs, and professional standing. However, their power is contingent on the continued medicalization frame and state authority over substance control.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_advocates, beneficiary,
    organized, biographical, mobile, national).

% Disproportionately subject to enforcement of public health mandates due to segregated geography and policing patterns. Harm reduction and medicalization can reduce criminal penalties but increase surveillance, coercive treatment referrals, and intrusion into community institutions (schools, social services, housing programs). They bear the cost of the state apparatus even when it is framed as therapeutic rather than punitive.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, marginalized_communities, payer,
    powerless, biographical, trapped, regional).

% Displaced from primary substance control authority under this reading but retain enforcement of residual criminal statutes (drug trafficking, distribution, driving under influence). They experience the shift as a loss of jurisdictional control and budget allocation while still bearing costs of managing substance-related social disorder. They may resist harm reduction framing while cooperating with health agencies in enforcement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, law_enforcement, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, law_enforcement, observer).

% Are not directly addressed by the harm reduction reading's framework. Those who prefer to access substances outside the medicalized system (because they reject the patient identity, distrust government, or prefer unregulated autonomy) have no legitimated voice. The persistence of black markets is treated as a failure of policy uptake rather than as an expression of excluded preferences.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_who_use_illegal_markets, excluded,
    powerless, biographical, trapped, local).

% Are structurally excluded from the harm reduction reading's authority framework. They argue that substance autonomy is a right, not a public health problem requiring expert management. The medicalization frame forecloses their reading by locating authority in public health expertise rather than individual autonomy, making their voice inaudible within the harm reduction institutional logic.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, biographical, mobile, national).

% Benefit from reduced visible drug markets, decreased criminal violence associated with drug distribution, and the framing of substance use as a health problem rather than a moral failing. They gain cultural capital from the medicalization narrative (substance use is a disease, not a choice) and public order improvements. However, they may also experience increased surveillance and health policing in their neighborhoods.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, community_members_substance_free, beneficiary,
    moderate, biographical, mobile, local).

% Examines the structural gap between the harm reduction reading's legitimacy claims (minimizing harm without criminalization) and its operation (medicalization creating new forms of coercion, black markets persisting, surveillance intensifying). This seat observes whether the constraint achieves its stated coordination function or whether it is tangled rope masquerading as pure coordination.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, observer_external_auditor, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, public_health_institutions).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces criminalization with medical/public health authority: substance use is treated as a disease requiring evidence-based intervention (medication-assisted therapy, counseling, supervised consumption sites) rather than criminal punishment. The coordination problem solved is: how to reduce harms of substance use (overdose, infection, social disintegration) without the violence and inefficiency of criminal enforcement.
% TRANSFER_FUNCTION: Moves authority, resources, and the capacity to define and manage substance users from criminal law to public health institutions. Substance users are transferred from penal subjects to medical subjects; budgets flow to treatment providers and harm reduction programs; the capacity to mandate intervention shifts from courts to public health agencies and treatment protocols.
% ABSENT_VOICES: People who reject the patient/medical identity and prefer autonomy over managed use (legalization advocates); communities that distrust health institutions due to historical medical racism and do not consent to being managed as public health populations; people who use substances outside medicalized channels and benefit from black markets or prefer unregulated access. These voices are excluded by the reading's own framing, which locates authority in expertise rather than in affected parties' consent.
% DISAPPEARANCE_RATIONALE: If harm reduction authority disappeared overnight, substance control would revert to other frames (pure criminalization, legalization, or unmanaged black markets). The institutional power and resource allocation built on the medicalization frame would collapse, treatment providers would lose funding, public health agencies would lose jurisdiction, and the substance-using population would face either criminal or unregulated management. The world depends on this constraint's persistence.
% FOUNDING_PROBLEM: Criminalization of substance use produced mass incarceration, violent drug markets, police brutality, overdose epidemics, and treatment barriers for people seeking help. Harm reduction emerged as a response: treating substance use as a public health issue allows intervention without criminal punishment, making treatment more accessible and reducing some harms.
% FOUNDING_PROBLEM_CORROBORATION: Public health institutions and harm reduction advocates attest the founding problem is live: criminalization persists in many jurisdictions and produces documented harms. Legalization advocates attest the problem is misframed: criminalization is the problem, but medicalization is also a form of control that creates different harms. Empirical researchers outside the benefiting parties document that harm reduction reduces some harms (overdose death, infection) while leaving others intact (black market violence, coercive treatment, surveillance). The problem is partially solved, partially replaced.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts at 0.42 (the coordination genuinely improves access to treatment) and rises to 0.58 (as institutions consolidate authority and treatment becomes mandatory/conditional on compliance). Suppression is substantial (0.62 at end) because the constraint's persistence requires surveillance, drug testing, and coercive treatment referrals—the apparatus must continuously monitor users and enforce participation. Theater rises from 0.25 to 0.42: early enforcement focuses on genuine treatment provision, but as the system matures, more effort goes into compliance theater—documenting uptake, managing stigma, defending the medicalization frame against critique. Accessibility collapse is moderate (0.48): alternatives (unmanaged use, black markets, legalization) persist outside the medicalized system; they are not fully foreclosed. Resistance is high (0.71): people with substance use disorder, legalization advocates, and marginalized communities all resist medicalization, even when it improves some outcomes. The reading's legitimacy claim (harm reduction) is real but partial; the extraction component grows as the system becomes established.
 *
 * PERSPECTIVAL GAP:
 *   From the public health institutional seat, the constraint is coordination: a genuine improvement over criminalization, evidence-based, beneficial to health outcomes. From the people_with_substance_use_disorder seat, the constraint is tangled rope or snare: trading one form of control (criminal) for another (medical), with added surveillance and coercion, even if some outcomes improve. The engine computes these divergent classifications from the same structural data—the asymmetry is the point. The agenda-setter seat believes it is minimizing harm; the payer seats experience mandatory identity transformation.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health institutions sit at the beneficiary end (d near 0.0–0.2): they set the frame, expand authority and budgets, and face no material extraction cost. Treatment providers sit near beneficiary (d ≈ 0.15–0.3): they collect funding and expanded markets. People with substance use disorder sit at the target end (d ≈ 0.75–0.85): they must accept the patient identity, comply with treatment, submit to monitoring, and lose autonomy in exchange for reduced criminalization. The constraint is asymmetric: beneficiaries (institutions, providers) define the problem and solution; payers (users, marginalized communities) bear the extraction cost in the form of coercive authority. Legalization advocates are excluded (d undefined): they have no seat at the table; the reading's authority framework forecloses their voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (criminalization produces harms) is partially solved (treatment access improves, overdose deaths decline) but not fully resolved (black markets persist, surveillance replaces incarceration). The constraint shows no signs of mandatrophy—the founding problem remains live, and institutions have strong incentive to maintain the medicalization frame. However, the theater_ratio rising from 0.25 to 0.42 suggests increasing performative maintenance: as the constraint matures and some harms persist despite intervention, institutions spend more effort justifying and defending the frame against critiques that it is coercive. This is not yet mandatrophy (the problem is still live) but a warning trajectory: if the founding problem dies (substance use becomes fully managed, or legalization is chosen) while the constraint persists, mandatrophy will be present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicalization_as_coercion_boundary,
    'Is the measured extraction (0.58) the cost of genuine public health coordination, or does medicalization constitute a form of coercive control structurally equivalent to criminalization, merely reframed?',
    'Comparative study: measure health outcomes and subjective autonomy in harm reduction jurisdictions versus legalization jurisdictions and unregulated/black-market access. If autonomy and outcomes are similar across frames, medicalization is equivalent control; if outcomes diverge, medicalization has net benefit despite extraction cost.',
    'If medicalization is equivalent control, the constraint is a snare masquerading as coordination—extraction is masked by the harm-reduction framing. If outcomes genuinely diverge, the extraction is the price of coordination, and the constraint is tangled rope as claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medicalization_as_coercion_boundary, empirical, 'Whether medicalization is a true public health coordination or a repackaged form of coercive control.').

omega_variable(
    black_market_persistence_mechanism,
    'Why do black markets persist despite harm reduction services? Is it refusal of the patient identity, structural barriers to access, or profit incentives driving supply?',
    'Qualitative research with people who use substances outside medicalized systems; economic analysis of price and supply in black markets; ethnographic study of preferences for unmedicated access.',
    'If persistence is identity-refusal, the excluded voices are not absent but active; the reading forecloses them by denying legitimacy to unmedicated substance use. If persistence is access barriers, harm reduction is under-resourced. If supply-driven, medicalization fails to address root causes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence_mechanism, empirical, 'Why the harm reduction reading does not eliminate alternative substance-use pathways.').

omega_variable(
    institutional_capture_risk,
    'As harm reduction institutions consolidate authority and funding, do they develop incentive to maintain substance use as a managed population rather than eliminate the problem entirely?',
    'Time-series analysis of institutional budgets and expansion; interview study of provider perspectives on problem resolution; policy analysis of institutional resistance to legalization or true decriminalization that would end the mandate.',
    'If capture is real, the constraint''s extractiveness will continue rising even as genuine harm declines—theater ratio will increase. The constraint may transition from tangled rope to piton (theaters maintenance of an atrophied function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Whether public health institutions have structural incentive to perpetuate the substance-use problem they manage.').

omega_variable(
    authority_frame_foreclosure,
    'Does the harm reduction reading''s location of authority in public health expertise logically foreclose the legalization reading''s location of authority in individual autonomy, or do both readings coexist as live positions?',
    'Philosophical analysis of axiom compatibility; empirical observation of whether jurisdictions that adopt harm reduction simultaneously block legalization advocacy, or whether both remain politically live.',
    'If foreclosure is real, the reading relation to legalization is ''forecloses'' not ''coexists_with''; the harm reduction frame structurally prevents the autonomy frame from being heard. If coexistence is real, both readings remain live but in tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_frame_foreclosure, conceptual, 'Whether medicalization authority and autonomy authority are logically incompatible or merely competing.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.62) structural (external surveillance, mandatory treatment, legal coercion) or internalized (people with substance use disorder adopt the patient identity and self-police, even after exit)?',
    'Post-exit study: measure autonomy and decision-making patterns in people who exit medicalized systems (migrate to legalization jurisdictions, stop treatment participation). If suppression persists after structural exit, it is partly internalized.',
    'If internalized, the constraint''s effective suppression is higher than 0.62; the target carries the suppression with them. The transition cost to alternative readings (legalization) is higher, making the constraint more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether medicalization suppression is structural or internalized through identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(subs_tr_t25, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(subs_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(subs_be_t25, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(subs_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(subs_su_t25, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(subs_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel has three distinct constraint readings: harm_reduction_reading (this file, medicalization authority), prohibition_reading (criminalization authority), and legalization_reading (autonomy authority). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and stakeholder configurations. The three are linked via network.affects_constraints because they compete for institutional authority over the same problem domain. The readings are not measurements of one constraint from different angles—they are structurally distinct constraints grounded in incompatible authority claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
