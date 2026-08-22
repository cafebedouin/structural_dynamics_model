% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member Sovereignty Reading of EU Free Movement: Welfare-Bounded Mobility
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the member sovereignty reading of the
 *   federation_membership_kernel: the claim that free movement of persons
 *   within the EU must be bounded by the fiscal capacity and social
 *   solidarity institutions of receiving member states. Under this reading,
 *   member states retain legitimate authority to exclude economically
 *   inactive migrants and to protect domestic labour markets, framing the
 *   constraint as necessary coordination for welfare state survival. The
 *   constraint operates through active legal and administrative enforcement
 *   of entry and residence conditions. It produces clear structural
 *   asymmetry: receiving state governments and native workforces are
 *   coordinated into protected solidarity systems, while economically
 *   inactive migrants and mobile workers from sending states bear the costs
 *   of exclusion and restricted access. The divergence between the
 *   coordination claim and the extractive operation is the measurement
 *   target; the engine computes per-seat classifications from this structural
 *   data.
 *
 * KEY AGENTS:
 *   - receiving_state_governments (agenda_setter/beneficiary): institutional/constrained â administer exclusion and retain sovereignty authority
 *   - native_workforces (beneficiary): moderate/constrained â receive labour market and welfare protection
 *   - economically_inactive_migrants (payer): powerless/trapped â excluded from territory and benefits
 *   - mobile_workers_from_sending_states (payer): moderate/constrained â face restricted access and brain drain externalities
 *   - supranational_court_and_commission (observer): institutional/analytical â contest exclusions under EU law
 *   - sending_state_governments (excluded): institutional/constrained â bear brain drain costs without voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.7).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.76).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member Sovereignty Reading of EU Free Movement: Welfare-Bounded Mobility").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'b8637b8a-3d2b-4270-ab82-c998ed0cddee').
narrative_ontology:cs_kernel_codification('b8637b8a-3d2b-4270-ab82-c998ed0cddee', formalized).
narrative_ontology:cs_authority_grounding('b8637b8a-3d2b-4270-ab82-c998ed0cddee', lineage).
narrative_ontology:cs_interpretation_layer_present('b8637b8a-3d2b-4270-ab82-c998ed0cddee').
narrative_ontology:cs_reading_relation('b8637b8a-3d2b-4270-ab82-c998ed0cddee', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8637b8a-3d2b-4270-ab82-c998ed0cddee', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('b8637b8a-3d2b-4270-ab82-c998ed0cddee', foundational, social_solidarity_precedes_mobility_rights).
narrative_ontology:cs_axiom_status(social_solidarity_precedes_mobility_rights, holdable).
narrative_ontology:cs_axiom_grounding('b8637b8a-3d2b-4270-ab82-c998ed0cddee', social_solidarity_precedes_mobility_rights, conventional).
narrative_ontology:cs_axiom('b8637b8a-3d2b-4270-ab82-c998ed0cddee', foundational, member_state_exclusion_authority).
narrative_ontology:cs_axiom_status(member_state_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding('b8637b8a-3d2b-4270-ab82-c998ed0cddee', member_state_exclusion_authority, conventional).
narrative_ontology:cs_reference_frame('b8637b8a-3d2b-4270-ab82-c998ed0cddee', member_state_social_sovereignty).
narrative_ontology:cs_drift_state('b8637b8a-3d2b-4270-ab82-c998ed0cddee', supranational_expansion_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b8637b8a-3d2b-4270-ab82-c998ed0cddee', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, native_workforces).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_sending_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the legal framework for entry, residence, and social benefits. Set the boundaries of economic activity and social solidarity to determine who may enter and what they may claim. Retain constitutional authority over welfare state design and labor market regulation. EU exit is legally possible but geopolitically and economically prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments, beneficiary).

% Benefit from protected labor markets and social insurance systems that exclude non-contributory claims from newly arrived inactive migrants. Their wages and social rights are insulated from direct competition by the boundary. They do not administer the constraint but receive its coordinating protection.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, native_workforces, beneficiary,
    moderate, biographical, constrained, national).

% Seek residence or social assistance in receiving states but are categorised as threats to social solidarity. Face legal exclusion from territory or benefits, deportation risk, and administrative categorisation as unreasonable burden. No effective voice in the polity that decides their status. Exit means remaining in the sending state, which is often economically precarious.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% Exercise formal free movement rights but face restricted labour market access, non-recognition of qualifications, and transitional barriers. Subject to brain drain dynamics that deplete sending state human capital while their mobility is partially blocked by receiving state labour market protections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_sending_states, payer,
    moderate, biographical, constrained, regional).

% Interpret the treaties as mandating expansive free movement and equal treatment. Contest member state exclusions through infringement procedures and preliminary rulings. They see the constraint as a violation of the EU legal order's foundational principles but lack direct power to override constitutional resistance from member states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, supranational_court_and_commission, observer,
    institutional, generational, analytical, continental).

% Bear the fiscal and developmental costs of brain drain as their skilled workers are diverted or blocked by receiving state labour market barriers. Not consulted in receiving state decisions to exclude or restrict. Would argue for unrestricted mobility and mutual recognition but are structurally absent from the receiving state's solidarity calculus.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables receiving states to maintain redistributive welfare institutions and protected labour markets by controlling the scale and composition of migration flows, preventing fiscal free-riding and social dumping that could undermine solidarity.
% TRANSFER_FUNCTION: Transfers mobility rights and social benefit claims from individual EU citizensâespecially the economically inactive and mobile workers from lower-wage statesâto the territorial authorities of receiving member states.
% ABSENT_VOICES: Sending state governments, who bear brain-drain externalities without reciprocal compensation; supranational EU institutions defending unconditional free movement; and economically inactive migrants themselves, who are categorically excluded from the political community that decides their status.
% DISAPPEARANCE_RATIONALE: Receiving states would lose the legal tool to exclude fiscal risks from their welfare calculus; labour markets would absorb higher mobility; social insurance pools would face altered claimant structures; and the federal balance would shift toward the integration reading, triggering political crises in welfare-state-centric member states.
% FOUNDING_PROBLEM: How to sustain generous national welfare states and cohesive labour markets within an economic federation that constitutionally guarantees free movement of persons, without generating fiscal externality races to the bottom or eroding public support for redistribution.
% FOUNDING_PROBLEM_CORROBORATION: Member state interior ministries and domestic labour unions attest the problem is live and justify exclusion on these grounds. The European Commission and ECJ contest the magnitude of the threat, citing low actual welfare tourism rates. Independent fiscal studies by OECD and academic political economists provide mixed corroboration: some find modest fiscal impacts, others find the threat is politically amplified beyond empirical scale.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) is high because the constraint systematically denies mobility rights and benefit access to identifiable groups on the basis of economic status and origin, transferring those rights to territorial authorities. Suppression (0.76) is higher still because the constraint's persistence depends on active border, administrative, and judicial enforcement to exclude categories of movers and to resist supranational legal challenges. Theater ratio (0.38) is moderate: the welfare-protection rhetoric has a genuine coordination basis, but an increasing share of enforcement activity serves performative sovereignty claims that exceed measured fiscal strain. Accessibility collapse (0.40) is incomplete because the integration reading remains a live legal and political alternative within the EU framework. Resistance (0.62) reflects sustained opposition from EU institutions, sending states, and mobile citizens. Measurements trace an extraction-accumulation and enforcement-ratchet trajectory over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The receiving state government seat and native workforce seat should compute toward coordination: they experience the constraint as protecting institutions they depend on and identify with. The inactive migrant and sending-state worker seats should compute toward extraction: they experience the same legal structure as an active denial of federal rights. The supranational observer seat sees a treaty violation dressed in solidarity language. The engine derives this divergence from the declared beneficiary/victim structure and differentiated exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving state governments and native workforces are structural beneficiaries of the boundary (low d): the constraint subsidises their welfare state stability and labour market position. Economically inactive migrants and mobile workers from sending states are structural targets (high d): the constraint extracts mobility rights and labour market access from them. The supranational court sits at analytical distance with no directional stake in the extraction. Sending state governments are excluded from the constraint's political calculus entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a genuine coordination problemâsustaining national solidarity institutions against uncoordinated migration shocksâand that problem has not fully disappeared. However, the classification as tangled_rope captures that the coordination function is structurally coupled to asymmetric extraction: the same legal mechanism that protects welfare pools also excludes mobile citizens from federal rights. Mandatrophy is partial. The constraint has not become a pure piton because receiving states still actively profit politically and fiscally from its operation; but the coordination component has been partially overtaken by theatrical assertions of sovereignty that exceed empirical welfare threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_welfare_strain,
    'Does economically inactive migration actually generate sufficient fiscal strain to threaten national welfare state sustainability, or is the threat politically constructed?',
    'Cross-national fiscal incidence studies comparing welfare claimant rates among EU mobile citizens versus native populations; dynamic scoring of budgetary impacts of unrestricted mobility.',
    'If strain is negligible, the extraction is disproportionate and the coordination function serves as cover for exclusion; if substantial, the constraint retains a genuine proportionality basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_welfare_strain, empirical, 'Whether the fiscal threat to welfare states is empirically real or politically constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of inactive migrants enforced through structural legal barriers alone, or through internalised norms of social deservingness that persist even when legal barriers are lifted?',
    'Comparative analysis of welfare access rates in jurisdictions with identical legal frameworks but different administrative cultures; post-legalisation uptake studies.',
    'If internalised, effective suppression exceeds the legal measure and the constraint operates partly through identity fusion of the native community against outsiders, raising the true extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalised enforcement of exclusion.').

omega_variable(
    kernel_reading_boundary,
    'At what structural point does the member sovereignty reading diverge from the welfare coordination readingâis it on the necessity of territorial exclusion, or on the locus of authority?',
    'Comparative doctrinal analysis of treaty interpretation: whether welfare coordination mechanisms (exportable benefits, anti-dumping rules) can fully substitute for exclusion authority.',
    'If coordination can substitute, the member sovereignty reading''s extraction component is unnecessary and the constraint family collapses toward welfare_coordination_reading; if not, exclusion authority is structurally indispensable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural boundary between sovereignty and coordination readings of the federal membership kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t6, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(fede_tr_t18, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t6, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(fede_be_t18, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t6, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(fede_su_t18, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 30, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% The federation_membership_kernel conflates three structurally distinct claims about the relationship between free movement, welfare state sustainability, and federal authority. This story isolates the member sovereignty reading; its siblings isolate the integration and welfare coordination readings. They are linked as a constraint family because empirical and legal arguments in one directly affect the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
