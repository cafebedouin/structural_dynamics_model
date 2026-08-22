% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment Originalist Civic Virtue Reading: Universal Armed Citizenry
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the originalist civic virtue reading of the
 *   Second Amendment's text as it was understood in the founding era: 'A well
 *   regulated Militia, being necessary to the security of a free State, the
 *   right of the people to keep and bear Arms, shall not be infringed.' Under
 *   this reading, the phrase 'well regulated Militia' is not a limiting
 *   condition but a statement of purpose — the militia is the armed citizenry
 *   organized for collective self-defense and as a structural check on
 *   tyranny. The operative clause protects the people's capacity to maintain
 *   this armed citizenry as the ultimate guarantor of republican government.
 *   This reading competes with two sibling interpretations: the
 *   collective-security reading (which treats the militia clause as
 *   permitting state regulation of arms for public safety) and the
 *   individual-right reading (which treats the operative clause as protecting
 *   personal self-defense independent of militia context). The constraint
 *   story describes the structure of civic virtue reading: who benefits (the
 *   political community as armed citizenry), who bears the constraint (states
 *   and federal government, which cannot monopolize armed force), and what
 *   the constraint protects (the distributed capacity for self-governance
 *   without reliance on centralized military power).
 *
 * KEY AGENTS:
 *   - armed_citizenry_political_community: beneficiary (structural beneficiary of the right; the people qua sovereign political community)
 *   - state_governments: payer (constrained by the inability to disarm the citizenry; unable to monopolize state military power)
 *   - federal_government: payer (similarly constrained; prevented from establishing unopposed military tyranny)
 *   - civic_republicans_originalists: agenda_setter (maintain the reading through scholarship, litigation, and judicial appointments; administer the interpretive framework)
 *   - gun_control_advocates: excluded (their safety-focused framing is excluded by the reading's foundational civic-republican premise; they would argue for regulatory latitude)
 *   - contemporary_courts: observer (adjudicate which reading prevails; currently split between originalist civic virtue, individual right, and collective security framings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.38).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment Originalist Civic Virtue Reading: Universal Armed Citizenry").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '9636cedd-4d09-491e-a9b2-6f6c7acf2eea').
narrative_ontology:cs_kernel_codification('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', fixed_text).
narrative_ontology:cs_authority_grounding('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', lineage).
narrative_ontology:cs_interpretation_layer_present('9636cedd-4d09-491e-a9b2-6f6c7acf2eea').
narrative_ontology:cs_reading_relation('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', second_amendment_text__individual_right_reading, influences).
narrative_ontology:cs_axiom('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', foundational, universal_militia_universalism).
narrative_ontology:cs_axiom_status(universal_militia_universalism, holdable).
narrative_ontology:cs_axiom_grounding('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', universal_militia_universalism, deontological).
narrative_ontology:cs_axiom('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', foundational, distributed_power_check_tyranny).
narrative_ontology:cs_axiom_status(distributed_power_check_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', distributed_power_check_tyranny, deontological).
narrative_ontology:cs_reference_frame('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', founding_era_civic_republican_militia).
narrative_ontology:cs_drift_state('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', contemporary_regulatory_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9636cedd-4d09-491e-a9b2-6f6c7acf2eea', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The entire able-bodied citizenry understood as the organized militia — the self-governing political community maintaining distributed capacity to defend against tyranny and foreign invasion. This reading treats the right as vested in the people collectively as the ultimate political sovereign, not merely in isolated individuals. The benefit is the preservation of civic capacity for self-rule without dependence on a standing army or centralized monopoly on armed force.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_political_community, beneficiary,
    organized, generational, mobile, national).

% Bear the structural constraint that an armed citizenry exists within their jurisdictions and that this citizenry cannot be disarmed or subjugated to the state's exclusive military control. This reading denies the state's unilateral power to monopolize armed force. The constraint is 'paid' in the form of reduced state monopoly over violence — a burden on centralized authority but not a direct revenue extraction.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Similarly constrained by the existence of an armed citizenry as a check on centralized federal power. The Founding understanding was that federal standing armies posed a tyranny risk; an armed citizenry distributed across states was the intended counterweight. The constraint 'costs' the federal government in the form of limits on its monopoly of armed force.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Advocates, scholars, and judges who interpret the Second Amendment through civic republican political theory and original public meaning. They administer the reading's interpretive framework by publishing scholarship, litigating cases, and building jurisprudence that treats the right as tied to civic virtue and distributed political power, not merely personal security. They maintain the reading's authority by grounding it in Founding-era texts and political philosophy.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_republicans_originalists, agenda_setter,
    organized, generational, mobile, national).

% Hold the competing view that the Second Amendment permits strict regulation in service of public safety and collective security. This reading brackets their claims — it does not adjudicate their arguments but excludes them from the interpretive framework by defining the right as civic rather than regulatory. They would argue the right is subject to reasonable restrictions in the contemporary context; this reading's foundational premise forecloses that framing.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_control_advocates, excluded,
    organized, biographical, mobile, national).

% Adjudicate disputes about the Second Amendment's scope and application. They examine evidence about original public meaning, competing framings, and precedent. Some courts adopt this originalist civic virtue reading; others adopt the individual-right or collective-security readings. The constraint's enforcement depends on which reading courts institutionalize.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, contemporary_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a civic republican institutional order in which the armed citizenry collectively functions as a distributed check on tyranny and as the ultimate sovereign body from which governmental authority derives. The militia qua universal armed citizenry solves the problem of how to prevent government monopoly on force without requiring constant insurrection — by making armed capability a permanent feature of civic participation.
% TRANSFER_FUNCTION: Transfers the burden of maintaining distributed armed capacity from the state to the citizenry, and transfers the check on state power from elite institutions to the people. No direct transfer of money or goods, but a structural transfer of the power to monopolize violence from the state back to the organized citizenry.
% ABSENT_VOICES: Perspectives that prioritize public health and collective safety over individual/distributed armed capacity are not seated in this reading's framework. Gun control advocates, public health researchers, and victims of gun violence who would argue the right should be subordinated to safety measures are excluded from the civic republican schema by the reading's foundational premise that an armed citizenry is prerequisite to republican self-governance.
% DISAPPEARANCE_RATIONALE: If this reading's constraint vanished — if the right to keep arms were understood as subject to plenary state regulation rather than as a check on state power — the political structure would substantially rearrange. The reading claims that without distributed armed capacity, the people lose their ultimate check on tyranny and their status as political sovereign. Gun control advocates and public-health-focused readers dispute this, arguing that democratic institutions and constitutional law provide adequate checks without relying on armed citizenry. The rearrangement vs. stability claim is the core of the contest.
% FOUNDING_PROBLEM: How to establish a republican government where ultimate sovereignty resides in the people and power is distributed such that no faction (especially not a standing army or centralized military) can tyrannize the rest. The Founding answer: the people must retain armed capacity, embodied in an organized militia of all able-bodied citizens.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers, Anti-Federalist writings, and militia statutes from the founding era attest the founding problem was preventing military tyranny and maintaining popular sovereignty. Originalist constitutional scholars (Randy Barnett, Stephen Halbrook, David Hardy) and early-republic historians (David Hackett Fischer, Gordon Wood on militia theory) corroborate the civic republican reading from outside contemporary gun-rights advocacy. Gun control advocates and legal scholars (Cass Sunstein, Laurence Tribe in some formulations) argue the founding problem is solved by modern democratic institutions and that the standing army is no longer the tyranny vector — they attest the founding problem's *premises* are no longer live, even if the problem statement itself is conceded.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading scores as moderate-extraction rope rather than pure mountain or pure snare because: (1) It does coordinate a genuine political function — preservation of distributed power against tyranny — which is the civic republican claim. (2) But it extracts from states/federal government by denying them the power to monopolize armed force, which is a real structural burden. Extractiveness is moderate (0.38) because the constraint is more structural than coercive; suppression is low (0.22) because the reading operates through constitutional text interpretation, not through active enforcement machinery preventing resistance. Theater is also low (0.18) because the constraint operates principally through interpretive coherence and originalist scholarship, not through performative institutional activity. The measurement series tracks the constraint's evolution: extractiveness rises after 1934 (when the NFA begins regulatory regimes that challenge the civic virtue reading), peaks in 1968 during the height of gun-control legislation, and moderates slightly after 2008 (DC v. Heller partially vindicated originalist readings), settling at 0.38 as a contested equilibrium. Suppression remains stable because the constraint's persistence depends on textual authority and judicial interpretation, not on active coercive prevention of competing readings.
 *
 * PERSPECTIVAL GAP:
 *   From the civic-republican originalists' seat, the reading is a rope: it genuinely coordinates distributed political power and solves the tyranny problem. From the state/federal government's seat, it is experienced as a constraint on their power — a payer role. From gun-control advocates' seat (excluded), the reading appears as a spurious natural law masking a distributional choice: why should the right to carry be tied to civic virtue rather than subject to public-health regulation? From the contemporary courts' seat, the reading is one of three competing frameworks, each with textual and historical support. The engine computes these divergences from the structural data — no single seat perceives the same constraint type. The authored claim (rope) reflects the beneficiary's framing; computed types will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The armed citizenry (beneficiary) has directionality d near 0.0 (low target, receives subsidy of protected capacity). The state/federal governments (payers) have directionality d near 1.0 (high target, lose monopoly on force). Courts sit at analytical d~0.5 (neither benefiting nor bearing cost; adjudicating the claim). Civic republicans sit at d~0.0 (beneficiaries and agenda-setters; they collects the vindicated proposition of civic virtue). Gun control advocates are excluded and have no d value within this reading's framework — their directionality would be computed in the collective_security_reading constraint, where they would be beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing tyranny through armed citizenry) has status=contested because contemporary readers disagree about whether the founding problem is live: originalists say tyranny through standing armies and centralized power is a permanent structural risk; gun-control and democratic-theory readers say democratic institutions, constitutional checks, and rule of law have solved the tyranny problem without relying on an armed citizenry. The disappearance_verdict=contested reflects this: if the originalist reading vanished (and regulation prevailed), the political structure would substantially change according to civic republicans (loss of popular sovereignty check), but would remain stable according to democratic theorists (adequate checks remain through law). This is a genuine mandatrophy candidate — the founding problem's obsolescence is the axis of contention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_militia_universalism,
    'Was the founding-era militia understood as the universal armed citizenry (all able-bodied men) or as an organized subset subject to state regulation?',
    'Historical analysis of founding-era militia statutes, state constitutions, and Federalist/Anti-Federalist commentary on militia universalism vs. regulation. Textual evidence from the founding period itself.',
    'If universalism is established (all citizens as militia), the civic virtue reading is strengthened; if militia was always understood as state-organized and regulable, the collective-security reading gains structural support. The constraint''s ε depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_militia_universalism, empirical, 'Whether founding-era militia was universal (all able-bodied citizens) or state-organized (regulable subset).').

omega_variable(
    tyranny_vector_permanence,
    'Is the tyranny vector against which the armed citizenry protects (standing armies, centralized military monopoly) a permanent structural risk or a problem solved by modern democratic institutions?',
    'Comparative institutional analysis: Do modern democracies without widespread private armed citizenry remain stable under rule of law? Are there historical instances where armed citizenry prevented tyranny after democratic institutions had formed?',
    'If the tyranny vector is permanent, the founding problem remains live and the civic virtue reading is vindicated as structurally necessary. If solved by democratic institutions, the founding problem is dead and the constraint becomes theater — a piton rather than a rope. This is the core axis of mandatrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tyranny_vector_permanence, preference, 'Whether the founding problem (tyranny through centralized armed force) remains live or is structurally solved.').

omega_variable(
    civic_virtue_vs_individual_right_boundary,
    'Is the protected right fundamentally about civic participation in distributed armed capacity, or about individual autonomy in personal self-defense?',
    'Genealogical analysis: which concept (civic virtue or individual right) was primary in founding-era political theory? Which is logically prior in the text''s structure? Linguistic analysis of ''the right of the people'' — does ''people'' denote a political collective or aggregated individuals?',
    'If civic virtue is primary, this reading forecloses the individual-right reading (they cannot coexist in a single coherent framework). If individual right is primary, this reading is an over-interpretation of civic purpose onto a text that protects personal autonomy. The relationship between this reading and the individual_right_reading depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_virtue_vs_individual_right_boundary, conceptual, 'Whether the Second Amendment''s primary function is civic republican (distributed political power) or individual (personal autonomy).').

omega_variable(
    reading_institutionalization_drift,
    'As courts adopt or retreat from this originalist civic virtue reading, does the constraint''s extractiveness from states/federal government change structurally, or only in rhetorical enforcement?',
    'Longitudinal analysis of judicial decisions and statutory responses: When courts validate the civic virtue reading, do states actually reduce gun regulation, or do they reframe existing regulation in civic-safety language (theater increase)? Do states institutionally adapt or merely perform compliance?',
    'If enforcement is structural (states actually reduce monopoly power), extractiveness remains. If enforcement is rhetorical (theater_ratio rises, actual state power unchanged), the constraint drifts toward piton — theater masking inertia. The post-2008 measurement stability (extractiveness moderates rather than declining) suggests potential theater drift, but the causal mechanism requires investigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_institutionalization_drift, empirical, 'Whether institutional responses to the civic virtue reading are structural or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.08).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1934, 0.14).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1968, 0.16).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.17).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1868, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(seco_be_t1934, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1868, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement(seco_su_t1934, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1934, 0.22).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1968, 0.21).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2008, 0.22).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__originalist_civic_virtue_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% The 'second_amendment_text' kernel grounds three constraint stories, each instantiating a different reading of the same constitutional text. The originalist_civic_virtue_reading (THIS STORY) treats the right as tied to distributed political power through universal armed citizenry. The collective_security_reading treats the militia clause as permitting state regulation for public safety. The individual_right_reading treats the operative clause as protecting personal self-defense independent of militia context. These three readings have different ε values, different beneficiary/victim structures, and different types because they answer different structural questions about who benefits from the constraint. Decomposition follows the ε-invariance principle: the same text, measured through different readings' epistemic frameworks, yields different constraints. Each reading's ε is measured against the standing arrangement IT describes (the civic virtue arrangement under the civic virtue reading's lights, the regulated arrangement under the collective security reading's lights, etc.). All three affect each other: the civic virtue reading influences the other two by grounding the right in founding-era universalism; the individual-right reading forecloses civic virtue by denying the necessity of organized militia; the collective security reading influences both by reasserting state regulatory authority. These edges propagate through the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__originalist_civic_virtue_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
