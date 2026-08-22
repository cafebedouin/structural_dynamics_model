% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Militia Clause — Collective Security Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment's text reads: 'A well regulated Militia, being
 *   necessary to the security of a free State, the right of the people to
 *   keep and bear Arms, shall not be infringed.' The collective-security
 *   reading interprets the militia clause as a conditioning or purposive
 *   statement: the right to bear arms exists to enable organized civic
 *   defense, and the state retains authority to regulate individual gun
 *   ownership to serve that collective purpose. Under this reading, licensing
 *   regimes, background checks, and restrictions on certain weapon classes
 *   are constitutionally permissible because they advance the militia
 *   function (maintaining trained, vetted citizen-soldiers) without
 *   eliminating the right itself. The state regulatory apparatus becomes the
 *   primary beneficiary of this arrangement — it gains lawful authority to
 *   structure gun ownership around public safety goals. Individual gun owners
 *   become a constrained class whose constitutional claim is qualified by the
 *   collective-security rationale.
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: administers licensing/permitting regimes; benefits from authority to condition gun ownership on public-safety criteria
 *   - public_safety_constituencies: police departments, public health authorities, crime-prevention advocates; benefit from regulatory capacity to track and restrict dangerous actors
 *   - unregulated_gun_owners: individuals who understand the operative clause as guaranteeing personal bearing of arms independent of militia service; bear compliance costs and exit friction
 *   - originalist_constitutional_scholars: maintain the individual-right reading and civic-virtue reading; excluded from the state authority that implements this reading
 *   - courts_applying_collective_security_reading: agenda-setters in jurisdictions adopting this framework; implement licensing and interpret scope of constitutional protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.62).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.58).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Militia Clause — Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'a99e6ad8-59ed-4805-adfd-56ad8f33670b').
narrative_ontology:cs_kernel_codification('a99e6ad8-59ed-4805-adfd-56ad8f33670b', fixed_text).
narrative_ontology:cs_authority_grounding('a99e6ad8-59ed-4805-adfd-56ad8f33670b', lineage).
narrative_ontology:cs_interpretation_layer_present('a99e6ad8-59ed-4805-adfd-56ad8f33670b').
narrative_ontology:cs_reading_relation('a99e6ad8-59ed-4805-adfd-56ad8f33670b', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('a99e6ad8-59ed-4805-adfd-56ad8f33670b', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('a99e6ad8-59ed-4805-adfd-56ad8f33670b', foundational, militia_clause_conditions_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('a99e6ad8-59ed-4805-adfd-56ad8f33670b', militia_clause_conditions_operative_clause, deontological).
narrative_ontology:cs_axiom('a99e6ad8-59ed-4805-adfd-56ad8f33670b', secondary, state_vetting_authority_serves_collective_defense).
narrative_ontology:cs_axiom_status(state_vetting_authority_serves_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('a99e6ad8-59ed-4805-adfd-56ad8f33670b', state_vetting_authority_serves_collective_defense, instrumental).
narrative_ontology:cs_reference_frame('a99e6ad8-59ed-4805-adfd-56ad8f33670b', regulated_militia_state_authority).
narrative_ontology:cs_drift_state('a99e6ad8-59ed-4805-adfd-56ad8f33670b', contemporary_standing_military_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a99e6ad8-59ed-4805-adfd-56ad8f33670b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_constituencies).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unregulated_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, jurisdictions_with_weak_licensing_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, jurisdictions_with_weak_licensing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers licensing regimes, background-check systems, and permit approval/denial machinery. Sets criteria for gun ownership (felony status, domestic violence history, licensing examinations, etc.). Defends these regulations as necessary to organize gun ownership around public safety and militia readiness. Collects regulatory authority and enforcement capacity directly from this reading. Can reinterpret or rewrite the reading via legislative amendment or judicial appointment.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Police departments, public-health authorities, anti-violence advocacy groups. Benefit from state regulatory capacity to background-check, track, and restrict gun ownership by dangerous actors. Support licensing regimes and restrictions on certain weapon classes. Do not directly administer regulations but align with state apparatus and influence regulatory scope.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Individuals who interpret the operative clause as guaranteeing personal right to bear arms independent of militia service. Subject to licensing requirements, background checks, permit fees, and denial on grounds established by state criteria. Exit options are limited: formally surrendering the right is available but fuses with identity as constitutional claimants; practical exit (moving to less-regulated jurisdiction) is constrained by economic/social ties. Resistance to this reading is high among this constituency.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, unregulated_gun_owners, payer,
    moderate, biographical, identity_locked, national).

% States or localities that have adopted minimal gun licensing and background-check regimes. Face pressure to align with federal standards and public-safety norms. If they resist stricter regulation, they bear externalities (firearm trafficking, higher violence rates in states with weak regulations). If they adopt stronger regimes, they bear implementation costs. Their political constituency is often split between deregulation advocates and public-safety advocates.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, jurisdictions_with_weak_licensing, payer,
    organized, biographical, constrained, regional).

% Constitutional scholars, judges, and advocacy organizations who reject the collective-security reading and assert either the individual-right reading (personal self-defense is core protected activity) or the originalist-civic-virtue reading (founding-era militia was universal, not state-controlled). Are excluded from the authority that implements the collective-security reading. Would argue for higher constitutional protection for personal gun ownership or radically different militia interpretation. Their objections feed into litigation and legislative pushback.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, originalist_scholars_and_individual_right_advocates, excluded,
    institutional, generational, constrained, national).

% Judicial bodies that decide which reading of the Second Amendment governs constitutionality. As agenda-setters, they author the reading that becomes enforceable (if they adopt collective-security reading, state regulation is permissible; if they adopt individual-right reading, regulation is narrower). As observers, they reflect jurisprudential consensus and doctrinal development over time.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, courts_interpreting_second_amendment, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, courts_interpreting_second_amendment, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates gun ownership and militia capability around state-defined public-safety standards. Replaces uncontrolled, unvetted personal gun ownership with a system where the state assesses and conditions access based on safety criteria (background checks, licensing examinations, felony/domestic-violence screening). This provides public safety authorities with visibility and capacity to restrict dangerous actors.
% TRANSFER_FUNCTION: Transfers regulatory authority and enforcement capacity from dispersed individuals to a state apparatus. Gun owners lose the claim to unrestricted personal gun bearing; the state gains authority to license, deny, and revoke. This movement of authority is presented as necessary to serve collective security; individual owners experience it as a constraint on their claimed constitutional right.
% ABSENT_VOICES: Originalist scholars and individual-right advocates are structurally excluded from implementing this reading. They would argue the militia clause is not a limiting condition on the operative clause, or that founding-era militia was universal and citizen-based, not state-regulated. Their objections are present in scholarship and litigation but do not determine state regulatory policy under this reading. Unregulated gun owners are parties to the constraint but their objection (the right should be unqualified) reflects the excluded reading.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and were replaced by the individual-right reading, gun-licensing regimes would face constitutional challenge, state regulatory authority over gun ownership would narrow, and individuals would gain stronger claims to personal bearing of arms without state conditioning. Public-safety constituencies would lose enforcement capacity; states would lose regulatory authority. Militia-organized-defense would shift from state-controlled to individual-centered framing.
% FOUNDING_PROBLEM: A well-regulated state militia requires the state to organize, vet, train, and coordinate gun owners as part of a collective security apparatus. Without regulatory authority over gun ownership, the state cannot reliably constitute or maintain such a militia. The founding problem is state capacity to defend against tyranny and invasion through an organized militia composed of conditioned citizens.
% FOUNDING_PROBLEM_CORROBORATION: State regulatory authorities, public-health epidemiologists, and law-enforcement agencies attest the founding problem is live: gun violence and mass shootings require state capacity to vet and condition access. Constitutional scholars and originalists outside the regulatory apparatus attest the founding problem is obsolete: standing armies and national guards provide collective defense without individual-gun conditioning, making the militia rationale a cover story for state extraction of regulatory authority. Expert witnesses from both sides (law enforcement and constitutional law scholars with different reading) testify to the disputed status.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the state's regulatory authority rides on a constitutional reading that conditions a claimed right on state-defined security goals — the individual's claim is structurally subordinate to the state's security assessment. Suppression (0.58) is substantial because the arrangement requires active enforcement: background checks, licensing denials, permit revocation, and legal barriers to certain weapons. Theater (0.41) is moderate — the public-safety rationale is genuinely operative (actual vetting occurs, regulations track stated safety goals), but a growing share of enforcement effort defends state licensing authority itself rather than directly advancing militia capability. The measurement series shows extractiveness and suppression_requirement rising over the interval, suggesting increasing regulatory intensity and enforcement hardening (more restrictions added, enforcement capacity built up). Resistance (0.72) is high because substantial constituencies dispute this reading's authority and press alternative framings.
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory seat perceives this arrangement as legitimate coordination: the militia clause justifies vetting gun owners, organizing them within state oversight, and maintaining public order. This seat reads the constraint as protective (enabling organized defense) and the extraction as incidental cost-recovery for administering a public good. The unregulated-gun-owner seat perceives extraction: their claimed constitutional right is qualified by a militia rationale they did not write and a state apparatus they did not authorize. Exit options differ drastically — state actors can rewrite the reading via legislative interpretation and court appointment; individual owners must comply or face criminal penalties. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory apparatus: d near 0.0 (full beneficiary). It collects rents in the form of regulatory authority, sets the terms of gun ownership, and controls the licensing and denial machinery. Its exit options are institutional (can rewrite law or reinterpret the reading), not constrained. Public-safety constituencies: d near 0.2 (beneficiary-leaning). They gain enforcement tools and capacity without bearing direct compliance costs; their exit is organizational (align or realign with state authority). Unregulated gun owners: d near 0.85 (full target). They face licensing requirements, background checks, permit denial, and criminal penalties for noncompliance. Exit is identity-locked (constitutional claim is fused with personal liberty narrative) or constrained (can surrender the right, cannot reframe it within the current judicial authority). Jurisdictions-with-weak-licensing: d near 0.75 (target-leaning). They must either adopt regulatory regimes (cost) or resist federal/state pressure; if they resist, they bear externalities (higher crime if less capacity, federal override if applicable).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading rests on is 'ensure organized militia capacity for state defense against tyranny and invasion.' As of 2026, that founding problem is contested: the state has standing armies and national guards, making volunteer organized militia less functionally necessary for collective defense. The militia reading persists despite this functional atrophy because (1) constitutional authority grounds itself in the text's preamble and founding intent, not in current necessity, and (2) the state benefits from the regulatory authority the reading provides. Mandatrophy is present: the constraint persists as a platform for state regulatory extraction despite the founding rationale being substantially obsolete. This is why theater_ratio rises over the interval — enforcement activity increasingly defends the state's authority to regulate, not militia readiness itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_conditioning_scope,
    'Does the militia clause condition the entire operative clause on organized militia service, or does it merely provide one rationale for a right that extends beyond militia contexts?',
    'Comparative textual analysis with founding-era state constitutions, contemporaneous legislative intent records, and jurisprudential evolution tracking which activities courts have protected outside militia contexts.',
    'If the clause conditions the entire operative clause, state regulation of civilian arms can be categorical and comprehensive; if it provides a rationale without a limiting scope, some individual self-defense contexts are protected outside militia frameworks. The classification remains tangled_rope either way (coordination + extraction are both present), but the extraction''s reach differs structurally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_conditioning_scope, conceptual, 'Semantic scope of the militia clause as condition versus rationale.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Can a single constitutional authority hold both the collective-security reading (state may regulate arms to serve collective security) and the individual-right reading (operative clause guarantees individual right independent of militia service) within the same framework without contradiction?',
    'Jurisprudential logic: if both readings can coexist by nesting individual rights within state regulatory authority (e.g., ''the right exists, and the state may regulate it via licensing''), the relation is coexists_with; if the readings assert contradictory empirical or normative premises about the clause''s scope, the relation is forecloses.',
    'If forecloses, the collective-security and individual-right readings cannot both be live in a single judicial framework — one rules out the other. If coexists_with, both are held by different judicial constituencies (state vs. federal, originalist vs. living-constitution courts) and the divergence is not logically fatal to either. The network topology depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether this reading logically excludes the individual-right reading or whether both can coexist across different authorities.').

omega_variable(
    civic_virtue_grounding_ambiguity,
    'Is the originalist-civic-virtue reading''s foundational claim (founding-era militia understood as universal armed citizenry) empirically contestable, or is it a pure philosophical commitment about how to read founding texts?',
    'Historical scholarship on militia organization, state constitution texts, founding-era gun ownership patterns, and explicitly stated founding intent — does the historical record support universal militia framing or does it show selective militia (state-controlled, age-restricted, property-restricted).',
    'If empirically contestable, the axiom_overriding direction applies when historical scholarship shows selective militia was the actual founding arrangement. If purely philosophical, no amount of evidence shifts the reading''s grounding — it remains holdable indefinitely. This affects the cs_structure.drift_state.direction coding for the sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civic_virtue_grounding_ambiguity, empirical, 'Historical verifiability of the civic-virtue reading''s founding-era militia thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(seco_tr_t5, second_amendment_text__collective_security_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__collective_security_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__collective_security_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__collective_security_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(seco_tr_t25, second_amendment_text__collective_security_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(seco_be_t5, second_amendment_text__collective_security_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__collective_security_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__collective_security_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(seco_be_t25, second_amendment_text__collective_security_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(seco_su_t5, second_amendment_text__collective_security_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__collective_security_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(seco_su_t15, second_amendment_text__collective_security_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(seco_su_t25, second_amendment_text__collective_security_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel decomposes into three reading-specific constraints. This story (collective_security_reading) codes the state-regulatory interpretation of the militia clause. The individual_right_reading emphasizes the operative clause as protecting personal self-defense independent of militia contexts. The originalist_civic_virtue_reading emphasizes founding-era militia as universal armed citizenry. Each reading carries a distinct ε (state authority over gun ownership; individual right prior to state; universal militia capacity), distinct beneficiary structure (state apparatus; individual owners; citizen-soldiers), and distinct classification (tangled_rope for state-centered authority; rope-to-snare spectrum for individual-right framings; rope for civic-virtue). The network links them as a constraint family: each affects the others because courts and constitutional scholars cite one reading as evidence for or against another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collective_security_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
