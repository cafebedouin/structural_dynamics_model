% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political/economic/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty reading of the
 *   federation-membership kernel: federation membership is a conditional
 *   treaty obligation, national governments retain the legitimate authority
 *   to control their own borders, and free movement across member states is a
 *   negotiable policy grant rather than a constitutional entitlement. Under
 *   this reading, when a member state reasserts quota systems, safeguard
 *   clauses, or permit requirements against inbound labor from other member
 *   states, it is exercising retained sovereignty, not violating a right.
 *   This produces a genuine coordination function (preserving domestic
 *   political legitimacy and labor-market stability) bundled with asymmetric
 *   extraction from citizens and workers who had structured their lives
 *   around an assumption of durable mobility. The sibling constraint, the
 *   integration reading, treats the same underlying kernel very differently
 *   and is NOT part of this story — it is a separate constraint with its own
 *   ε and stakeholder structure.
 *
 * KEY AGENTS:
 *   - national_governments: agenda_setter, sets and revokes mobility terms
 *   - local_labor_markets: beneficiary, protected from wage competition
 *   - border_enforcement_agencies: beneficiary/agenda_setter, administers the reasserted controls
 *   - mobile_citizens: payer, mobility downgraded from assumed right to revocable grant
 *   - cross_border_workers: payer, most exposed to sudden livelihood disruption
 *   - federation_institutions: excluded, structurally sidelined by this reading's premises
 *   - domestic_electorates: beneficiary/payer, gains political accountability but loses own mobility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.71).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political/economic/migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'a26ead51-0a89-4062-bc46-c45f8f52ad15').
narrative_ontology:cs_kernel_codification('a26ead51-0a89-4062-bc46-c45f8f52ad15', formalized).
narrative_ontology:cs_authority_grounding('a26ead51-0a89-4062-bc46-c45f8f52ad15', lineage).
narrative_ontology:cs_interpretation_layer_present('a26ead51-0a89-4062-bc46-c45f8f52ad15').
narrative_ontology:cs_reading_relation('a26ead51-0a89-4062-bc46-c45f8f52ad15', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('a26ead51-0a89-4062-bc46-c45f8f52ad15', foundational, border_control_as_retained_sovereign_authority).
narrative_ontology:cs_axiom_status(border_control_as_retained_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('a26ead51-0a89-4062-bc46-c45f8f52ad15', border_control_as_retained_sovereign_authority, conventional).
narrative_ontology:cs_axiom('a26ead51-0a89-4062-bc46-c45f8f52ad15', foundational, free_movement_as_revocable_policy_grant).
narrative_ontology:cs_axiom_status(free_movement_as_revocable_policy_grant, holdable).
narrative_ontology:cs_axiom_grounding('a26ead51-0a89-4062-bc46-c45f8f52ad15', free_movement_as_revocable_policy_grant, conventional).
narrative_ontology:cs_reference_frame('a26ead51-0a89-4062-bc46-c45f8f52ad15', treaty_based_conditional_accession).
narrative_ontology:cs_drift_state('a26ead51-0a89-4062-bc46-c45f8f52ad15', post_enlargement_mobility_backlash, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a26ead51-0a89-4062-bc46-c45f8f52ad15', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, domestic_electorates).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, domestic_electorates).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, conditional_treaty_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the constitutional authority to reimpose border controls, renegotiate free-movement terms, or exit the federation entirely. Treats federation membership as a treaty obligation subordinate to domestic constitutional order. Sets quotas, invokes safeguard clauses, and administers exceptions to free movement when domestic political pressure demands it.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic workers and sectoral unions who benefit when inbound labor mobility is restricted or slowed, reducing wage competition in sensitive sectors. Their leverage over national policy is real but bounded by federation-level trade dependencies.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Administer checkpoints, residency verification, and work-permit adjudication that the sovereignty reading treats as continuously legitimate national functions. Their budgets and institutional mandate expand whenever border legitimacy claims are reasserted.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, border_enforcement_agencies, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, border_enforcement_agencies, agenda_setter).

% Citizens of federation member states who planned life, work, or family arrangements around free movement as an assumed entitlement. Under this reading, their ability to relocate, work, or reunite with family across the federation is a policy grant that can be narrowed or suspended by any member state's domestic political cycle, not a right they can rely on.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Commute or seasonally migrate across a national border for employment. Quota changes, safeguard invocations, or permit revocations can end their livelihood with little notice; they hold no independent legal claim against a member state that reasserts border control, since under this reading their mobility was always a revocable policy accommodation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, immediate, trapped, regional).

% Supranational bodies that would, under the rival integration reading, treat free movement as a constitutional entitlement they adjudicate. Under the sovereignty reading their role is advisory and contractual — they can be consulted but their rulings on movement rights carry no binding force against a member state's reasserted border authority. They are structurally sidelined by this reading's premises, not merely disagreeing with them.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_institutions, excluded,
    institutional, civilizational, analytical, continental).

% Voters who gain a sense of accountable control over migration policy through national elections, but who also bear costs when reduced mobility limits their own ability to work, retire, or study elsewhere in the federation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, domestic_electorates, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, domestic_electorates, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates member states' shared market access and mutual recognition arrangements while preserving each state's capacity to manage the pace and volume of inbound labor mobility according to domestic political conditions.
% TRANSFER_FUNCTION: Moves control over entry, residency, and labor-market access from a federation-wide guarantee back to national administrations; shifts adjustment costs of economic shocks onto mobile citizens and cross-border workers rather than distributing them through guaranteed free movement.
% ABSENT_VOICES: Federation institutions and pan-federation labor advocacy groups would argue free movement is a constitutional entitlement rather than a revocable grant; they are excluded from binding adjudication under this reading and can only issue non-binding opinions.
% DISAPPEARANCE_RATIONALE: If national border-legitimacy claims disappeared overnight, member states would lose the domestic political capacity to restrict movement, border enforcement agencies would lose their reasserted mandate, and cross-border workers and mobile citizens would gain durable, judicially enforceable mobility rights — the labor-market protections currently available to domestic constituencies would erode.
% FOUNDING_PROBLEM: Federation treaties needed member-state ratification and continued political buy-in; sovereignty was retained as a condition of joining, including the understood ability to renegotiate or suspend specific commitments like free movement if domestic conditions changed.
% FOUNDING_PROBLEM_CORROBORATION: National governments and domestic labor constituencies attest the sovereign retention of border authority remains a live, foundational condition of membership. Federation institutions, migration-rights litigators, and cross-border labor associations — parties outside the beneficiary set — attest that decades of treaty practice and case law had functionally converted free movement into an entitlement, making the sovereignty reading's revival of full discretionary border control a rollback rather than a continuation of the original bargain.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71) is high because this reading, by its own structural logic, treats mobility as fully revocable — the cost of that revocability falls disproportionately on mobile citizens and cross-border workers who cannot anticipate or insure against sudden policy reversal. Suppression (0.62) reflects the enforcement apparatus (border checks, permit systems, quota administration) required to make border reassertion operative rather than merely rhetorical. Theater ratio is comparatively low (0.28) because the coordination function — genuine domestic political accountability over migration policy — is substantively real, not primarily performative, even though it is bundled with extraction from mobile populations.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national governments), this arrangement is a legitimate exercise of retained sovereignty consistent with the original treaty bargain — closer to coordination. From the payer seats (mobile citizens, cross-border workers), the same structure operates as an enforced downgrade of an entitlement they had reasonably relied upon — closer to extraction. The engine computes this divergence from the declared power/exit/scope data per seat; the claimed_type of tangled_rope reflects the authoring judgment that both the coordination function and the asymmetric extraction are structurally real, not that the seats agree.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and border enforcement agencies sit at the beneficiary end: they set and administer the terms and capture the political and institutional gains of reasserted control. Local labor markets and domestic electorates benefit from reduced wage competition and a sense of democratic accountability, though electorates also pay when their own cross-border mobility narrows. Mobile citizens and cross-border workers sit at the target end — trapped or constrained exit, immediate to biographical time horizons, and no independent legal claim against a member state's reassertion of border authority under this reading's own premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving national ratification consent and the ability to renegotiate treaty terms — remains partially live (member states retain genuine constitutional stakes in migration policy) but is contested as fully live: decades of treaty practice arguably converted free movement into a functioning entitlement before this reading's revival of full discretionary control. Classifying as tangled_rope rather than snare avoids mislabeling the genuine domestic-accountability coordination function as pure extraction; classifying as tangled_rope rather than rope avoids ignoring the asymmetric, coercively-enforced cost imposed on mobile citizens and workers who have no comparable exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the federation-membership kernel''s founding treaty text and subsequent practice support the sovereignty reading (border control as retained legitimate authority) or the integration reading (free movement as constitutional entitlement) as the operative interpretation?',
    'Constitutional court rulings, treaty amendment history, and longitudinal practice records (frequency and success of safeguard-clause invocations vs. judicial enforcement of free movement) would provide evidence for which reading better describes the actual operative kernel over time.',
    'If the integration reading is judicially vindicated as the operative kernel, this sovereignty-reading constraint''s extraction claims are undermined — the ''negotiable policy'' framing would be revealed as a rearguard reassertion against an already-settled entitlement, shifting classification toward snare. If the sovereignty reading prevails, the tangled_rope classification with genuine coordination function is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the sovereignty or integration reading is the kernel''s actual operative interpretation, per treaty text and practice.').

omega_variable(
    reversibility_of_mobility_reliance,
    'To what extent have mobile citizens and cross-border workers made irreversible life decisions (relocation, family formation, career investment) in reliance on free movement being durable, such that its later revocation constitutes a genuine harm rather than a foreseeable policy risk?',
    'Survey and administrative data on relocation patterns, family reunification cases, and economic investment decisions made under the assumption of durable mobility, cross-referenced against public awareness of the treaty''s conditional/revocable nature at the time decisions were made.',
    'High reliance with low awareness of revocability would increase the measured extraction (victims bore costs they could not reasonably have priced in); low reliance or high awareness of conditionality would suggest the sovereignty reading''s costs were more foreseeable and less extractive in character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_mobility_reliance, empirical, 'Whether mobile populations'' reliance on free movement was reasonable given the treaty''s declared conditionality.').

omega_variable(
    border_legitimacy_naturalness,
    'Is national border control best understood as a natural feature of sovereign statehood (approaching mountain-like status) or as a constructed and contestable arrangement that benefits specific domestic constituencies?',
    'Comparative analysis of federations and confederations with varying degrees of border pooling, and historical analysis of when and why the sovereignty reading was reasserted relative to economic conditions in local labor markets.',
    'If border legitimacy tracks domestic labor-market protection interests rather than an inherent feature of statehood, the sovereignty reading''s coordination framing weakens further toward extraction; if border control is closer to an irreducible feature of any federation short of full political union, some of the measured extraction should be treated as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(border_legitimacy_naturalness, conceptual, 'Whether national border legitimacy is closer to a natural feature of federated sovereignty or a constructed arrangement serving identifiable domestic beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t4, federation_membership__sovereignty_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(fede_tr_t8, federation_membership__sovereignty_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership__sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(fede_tr_t16, federation_membership__sovereignty_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t4, federation_membership__sovereignty_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(fede_be_t8, federation_membership__sovereignty_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(fede_be_t12, federation_membership__sovereignty_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(fede_be_t16, federation_membership__sovereignty_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t4, federation_membership__sovereignty_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(fede_su_t8, federation_membership__sovereignty_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(fede_su_t12, federation_membership__sovereignty_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(fede_su_t16, federation_membership__sovereignty_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint and federation_membership__integration_reading are two readings of the same federation_membership kernel, decomposed per the ε-invariance principle: measuring 'what federation membership means for mobility' under the sovereignty framing yields high ε (0.71, tangled_rope) while the integration framing yields a very different structure with mobile citizens as beneficiaries rather than victims. These are not the same constraint viewed from two angles — they have different beneficiary/victim sets and different classifications, hence two files linked here rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
