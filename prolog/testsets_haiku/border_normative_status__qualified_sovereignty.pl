% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified State Sovereignty in Border Control
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The qualified_sovereignty reading holds that states retain legitimate
 *   border control authority but must exercise it proportionately and
 *   consistently with human rights obligations. This reading is a KERNEL
 *   READING of the contested border_normative_status kernel, which also
 *   admits the freedom_primary reading (borders violate freedom of movement
 *   by default) and the sovereignty_primary reading (territorial boundaries
 *   are instruments of collective self-determination requiring no external
 *   justification). The qualified_sovereignty reading charts a middle course:
 *   it concedes the sovereignty_primary reading's claim that border control
 *   is legitimate, but adds the freedom_primary reading's constraint that
 *   legitimacy is conditional on proportionality and rights consistency. The
 *   kernel contest is not resolvable within the qualified_sovereignty
 *   framework itself — the framework presupposes the validity of both the
 *   state's authority and the external human rights standard that constrains
 *   it. Both sibling readings reject one of those presuppositions.
 *
 * KEY AGENTS:
 *   - state_apparatus (institutional, agenda-setter): administers border control, interprets proportionality
 *   - citizen_security_constituency (organized, beneficiary): supports control as protecting labor and resources
 *   - excluded_migrants (powerless, payer): denied entry, no formal standing
 *   - asylum_seekers (powerless, payer): fleeing persecution, depend on state discretion
 *   - displaced_persons (powerless, payer): stateless or unable to access home territory
 *   - human_rights_bodies (institutional, observer): assess proportionality and rights consistency
 *   - sovereignty_primary_advocates (organized, excluded): reject the proportionality framework itself
 *   - freedom_primary_advocates (organized, excluded): reject border authority itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.68).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.72).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified State Sovereignty in Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'fc82c9d4-1034-4590-b99b-e94d26760dc9').
narrative_ontology:cs_kernel_codification('fc82c9d4-1034-4590-b99b-e94d26760dc9', formalized).
narrative_ontology:cs_authority_grounding('fc82c9d4-1034-4590-b99b-e94d26760dc9', lineage).
narrative_ontology:cs_interpretation_layer_present('fc82c9d4-1034-4590-b99b-e94d26760dc9').
narrative_ontology:cs_reading_relation('fc82c9d4-1034-4590-b99b-e94d26760dc9', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('fc82c9d4-1034-4590-b99b-e94d26760dc9', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('fc82c9d4-1034-4590-b99b-e94d26760dc9', foundational, territorial_sovereignty_with_external_constraint).
narrative_ontology:cs_axiom_status(territorial_sovereignty_with_external_constraint, holdable).
narrative_ontology:cs_axiom_grounding('fc82c9d4-1034-4590-b99b-e94d26760dc9', territorial_sovereignty_with_external_constraint, deontological).
narrative_ontology:cs_axiom('fc82c9d4-1034-4590-b99b-e94d26760dc9', foundational, proportionality_doctrine_binding).
narrative_ontology:cs_axiom_status(proportionality_doctrine_binding, holdable).
narrative_ontology:cs_axiom_grounding('fc82c9d4-1034-4590-b99b-e94d26760dc9', proportionality_doctrine_binding, conventional).
narrative_ontology:cs_reference_frame('fc82c9d4-1034-4590-b99b-e94d26760dc9', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('fc82c9d4-1034-4590-b99b-e94d26760dc9', contemporary_security_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc82c9d4-1034-4590-b99b-e94d26760dc9', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_security_constituency).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, labor_receiving_states).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, human_rights_normativity).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the border control apparatus, interprets proportionality and legitimacy, decides which exclusions pass the human rights test. Juggles competing pressures: citizen demands for security, international human rights obligations, labor market needs, geopolitical alliances. The state claims border control is essential to self-determination; the qualified_sovereignty reading requires it to justify exclusions case-by-case.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Perceives and acts as if border control protects labor market access, housing availability, and cultural coherence. Supports the state's authority to exclude on grounds of protecting collective interests. Experiences the constraint as legitimating control rather than constraining it.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_security_constituency, beneficiary,
    organized, biographical, mobile, national).

% Denied entry because they fall outside the state's prioritized categories (economic migrants, undocumented persons, or those from disfavored origins). The qualified_sovereignty reading subjects their exclusion to proportionality review, but enforcement of that review exists only at the margins — most exclusions proceed without independent adjudication.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Fleeing persecution or violence, seeking refuge. The qualified_sovereignty reading nominally protects them via non-refoulement and international convention, but states routinely narrow asylum criteria, externalize processing to deny formal entry, or use expedited removal to avoid proportionality review.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Citizens of failed or conflict states rendered stateless or unable to access their home territory. Technically protected under international law, but enforcement is diffuse and most states treat them as security risks. The qualified_sovereignty reading requires states to justify displacement policies proportionately.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_persons, payer,
    powerless, biographical, trapped, regional).

% Regional and international human rights courts, treaty bodies, and independent monitors assess whether border exclusions meet proportionality and rights-consistency standards. They produce advisory opinions and judgments that states can ignore or comply with selectively.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Use border control to manage labor supply, regulate competition with domestic workers, and allocate housing and services. The qualified_sovereignty reading legitimates this control provided the state can show proportionality.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, labor_receiving_states, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, labor_receiving_states, agenda_setter).

% Political movements and thinkers arguing that state sovereignty and collective self-determination give states near-absolute authority to exclude. They view the qualified_sovereignty reading as constraining legitimate state power.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, sovereignty_primary_advocates, excluded,
    organized, generational, mobile, national).

% Argue that freedom of movement is a fundamental right that borders violate by default, requiring extraordinary justification. They view the qualified_sovereignty reading as too permissive.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, freedom_primary_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables states to maintain stable membership boundaries, allocate state resources (citizenship, public goods) to defined populations, and exercise collective self-determination without unlimited global redistribution pressure. Solves the collective-action problem of resource scarcity by allowing bounded communities to decide who shares in those resources.
% TRANSFER_FUNCTION: Moves access to state resources (employment, housing, public services, citizenship pathways) from excluded migrants, asylum seekers, and displaced persons to citizens and approved residents. The constraint gates entry; those excluded bear the cost of being locked out, while citizens and resident aliens benefit from exclusive access.
% ABSENT_VOICES: Excluded migrants and asylum seekers have no formal representation in border-control decisions; displaced persons' home states may be too weak to advocate. Sovereignty-primary advocates (who reject the proportionality framework) and freedom-primary advocates (who reject border control itself) are also excluded from the qualified_sovereignty discourse.
% DISAPPEARANCE_RATIONALE: Under the qualified_sovereignty reading, if the constraint disappeared (all borders opened unconditionally), states argue the world would reorganize catastrophically. Freedom-primary advocates argue exactly the opposite: stable, prosperous global equilibrium would emerge. The disagreement is about whether the state's resource-allocation role is legitimate.
% FOUNDING_PROBLEM: Early modern territorial consolidation created bounded political communities; these communities required stable membership to levy taxes, conscript armies, and distribute public goods. Without borders, the state-form itself becomes incoherent.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and states attest the founding problem is structurally live: any state without border control cannot maintain distinct fiscal or legal systems. However, global migration scholars, human rights bodies, and freedom-primary advocates attest the problem statement is anachronistic — modern communication and financial systems can operate across borders; border control persists as institutional inertia and extraction rent.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, contested).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.51 (substantial) and plateaus at 0.68 after interval midpoint. The initial value reflects the hybrid character: genuine coordination value in bounding communities for resource allocation, but substantial extraction from those excluded. The trajectory models states moving from loose proportionality enforcement (~0.51, occasional review) to tighter de-facto enforcement of security-first exclusions (0.68, routine expedited removal, narrowed asylum criteria) while maintaining the theater of proportionality review. Theater_ratio rises from 0.42 to 0.58 (moderate-to-high), modeling the increasing gap between the stated proportionality standard and the actual practice of near-automatic exclusion with minimal genuine adjudication. Suppression_requirement plateaus at 0.72, indicating that sustained extraction requires active suppression: enforcement machinery prevents excluded populations from mounting effective political resistance (no organizing rights, no standing in courts, no voice in policy). The metrics model a constraint that BEGAN as genuine coordination with proportionality oversight but has drifted toward routine extraction dressed in the language of proportionality.
 *
 * PERSPECTIVAL GAP:
 *   The state_apparatus and citizen_security_constituency would compute the constraint as rope (genuine coordination with marginal enforcement cost); the excluded_migrants and asylum_seekers would compute it as snare (pure extraction maintained by coercion). The engine computes from the structural facts: both beneficiaries and victims present, active enforcement required, and theater_ratio rising — these point toward tangled_rope. The perspectival divergence shows the constraint is doing its work of legitimating extraction by framing it as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_apparatus, citizen_security_constituency) derive directionality d toward the beneficiary end (low d, low/negative effective extraction): the state gains authority and citizens gain exclusive access. Victims (excluded_migrants, asylum_seekers, displaced_persons) derive directionality d toward the target end (high d, high effective extraction): they bear the full cost of exclusion. The state_apparatus has the interesting dual position: it is both the beneficiary (authority, legitimacy) and the enforcer (burden of maintaining the theater of proportionality). Overrides are not required — the structural derivation captures this: the state's d is intermediate because it must maintain proportionality fiction while excluding in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was territorial consolidation requiring bounded communities to levy taxes and conscript armies. That problem remains live under the qualified_sovereignty reading. However, the constraint's actual function has shifted: modern states use border control primarily to manage labor supply, housing pressure, and cultural anxiety — which are legitimate interests but not the founding problem. The mandatrophy omega (omega_founding_problem_obsolescence below) captures this tension: the constraint persists by appeal to a founding problem that has been substantially solved (modern states have other ways to maintain fiscal integrity), while extracting rents through routine exclusion. The qualified_sovereignty reading inadvertently enables this mandatrophy by accepting that border control is inherently legitimate so long as states can articulate proportionality — most states can articulate it sufficiently to pass political review, even when they don't practice it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_enforcement_gap,
    'How much of the constraint''s measured extractiveness derives from genuine abuse (states violating proportionality standards) versus states correctly applying the qualified_sovereignty framework (legitimate exclusion under proportionality)?',
    'Independent audit of state exclusion decisions against declared proportionality criteria. Sample excluded individuals, examine the state''s stated justification, assess whether the justification is (1) legitimate, (2) proportionate, and (3) consistent with human rights obligations.',
    'High enforcement gap (most exclusions abuse the standard) suggests the constraint is structurally a snare wearing rope''s clothing — extraction is primary and proportionality is theater. Low enforcement gap suggests states are genuinely applying the framework, making the constraint a true tangled rope with substantial but honest coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_enforcement_gap, empirical, 'What fraction of measured extractiveness is abuse versus correct application of the standard.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (territorial consolidation requiring bounded communities for taxation and conscription) still live, or have modern states achieved fiscal and administrative integrity through other means?',
    'Comparative analysis of state revenue systems, conscription capacity, and administrative reach in open-border and closed-border regimes. Does border closure materially improve a state''s ability to maintain fiscal integrity, military readiness, or administrative coherence? Or do modern states achieve these through other mechanisms (digital surveillance, property rights systems, diplomatic alliances)?',
    'If the founding problem is substantially solved, the constraint persists primarily as rent extraction and institutional inertia (piton or degraded snare), not as genuine coordination. If the founding problem is live, the constraint is legitimately tangled rope with substantial coordination function that justifies some extraction cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether border control remains necessary for state fiscal and administrative capacity.').

omega_variable(
    kernel_reading_boundary,
    'Can the qualified_sovereignty reading accommodate the core premises of both the sovereignty_primary and freedom_primary readings, or does it necessarily exclude one or both?',
    'Formal logical analysis: does qualified_sovereignty commit to ''states have legitimate authority to exclude'' (which entails that freedom_primary''s foundational claim is false) and ''exclusion must be proportionate and rights-consistent'' (which entails that sovereignty_primary''s foundational claim is incomplete)?',
    'If qualified_sovereignty necessarily forecloses one or both siblings, it is a genuinely distinct kernel reading, not a compromise. If it coexists with both (e.g., by allowing sovereigntist states to define proportionality by their own standards), it is less of a distinct reading and more a neutral framing within which other readings compete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether qualified_sovereignty has its own kernel commitment or is merely a framework for competing readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression primarily structural (legal barriers, economic dependency on labor access, enforcement machinery) or internalized (excluded populations have internalized the narrative that their exclusion is legitimate)?',
    'Post-exit behavioral analysis: if borders were opened, do excluded populations immediately assert labor mobility and political voice (suppression is structural), or do many continue to perceive themselves as unworthy of entry (suppression is internalized)? Long-term ethnographic study of diaspora communities and asylum-granted populations.',
    'If structural, the constraint''s extractiveness is accurate. If internalized, the effective suppression persists beyond the structural barrier, making the constraint more extractive than the scalar suppression_requirement score suggests — victims carry the suppression internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized identity.').

omega_variable(
    proportionality_standard_indeterminacy,
    'Is there a discoverable objective standard for what counts as ''proportionate'' exclusion, or is proportionality necessarily defined by the state itself?',
    'Review of judicial and treaty-body proportionality case law. Do human rights courts identify a stable, trans-cultural proportionality threshold, or do they defer to state definitions of national interest and cultural cohesion (which permits nearly any exclusion to be called proportionate)?',
    'If proportionality is objectively stable, the constraint can function as genuine coordination with external accountability. If proportionality is state-defined, the constraint''s proportionality requirement is purely performative — the state writes the standard and judges itself against it, converting the constraint to routine extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_standard_indeterminacy, conceptual, 'Whether proportionality is an objective external constraint or a state-defined performance standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bord_tr_t5, border_normative_status__qualified_sovereignty, theater_ratio, 5, 0.45).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__qualified_sovereignty, theater_ratio, 10, 0.48).
narrative_ontology:measurement(bord_tr_t15, border_normative_status__qualified_sovereignty, theater_ratio, 15, 0.52).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__qualified_sovereignty, theater_ratio, 20, 0.55).
narrative_ontology:measurement(bord_tr_t25, border_normative_status__qualified_sovereignty, theater_ratio, 25, 0.57).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__qualified_sovereignty, theater_ratio, 30, 0.58).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(bord_be_t5, border_normative_status__qualified_sovereignty, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(bord_be_t10, border_normative_status__qualified_sovereignty, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bord_be_t15, border_normative_status__qualified_sovereignty, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(bord_be_t20, border_normative_status__qualified_sovereignty, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(bord_be_t25, border_normative_status__qualified_sovereignty, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(bord_be_t30, border_normative_status__qualified_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t5, border_normative_status__qualified_sovereignty, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(bord_su_t10, border_normative_status__qualified_sovereignty, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(bord_su_t15, border_normative_status__qualified_sovereignty, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(bord_su_t20, border_normative_status__qualified_sovereignty, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(bord_su_t25, border_normative_status__qualified_sovereignty, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(bord_su_t30, border_normative_status__qualified_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__qualified_sovereignty, 0.18).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, citizenship_nexus_family_reunification).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, asylum_expedited_processing).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, labor_mobility_bilateral_treaties).

% DUAL FORMULATION NOTE:
% This constraint is one kernel reading of border_normative_status. The sibling readings (freedom_primary and sovereignty_primary) are separate constraint stories with different ε values, different victim sets, and different institutional logics. The three stories together form the border_normative_status family. This story links to downstream constraints that inherit the qualified_sovereignty framework: citizenship rules that implement the proportionality standard, asylum procedures that apply it, and labor treaties that navigate it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
