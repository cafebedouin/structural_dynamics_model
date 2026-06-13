% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation Reading: Practice Reversal via Prophetic Revelation
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   In the 1890s, a major American religious institution faced federal legal
 *   pressure to abandon a core doctrinal practice (plural marriage). Rather
 *   than explicitly admitting doctrinal error or external capitulation, the
 *   institutional leadership claimed a new prophetic revelation
 *   reinterpreting God's will. This constraint story models the ENDOGENOUS
 *   REINTERPRETATION READING: the institutional narrative that the reversal
 *   came from within the theological framework (a prophet receiving updated
 *   divine guidance) rather than from external coercion. The extractiveness
 *   (0.62) and suppression (0.71) reflect that institutional leadership
 *   preserves its authority and theological legitimacy while practitioners
 *   and the theological tradition absorb the cost of explaining why a
 *   doctrine claimed as revealed and eternal was suddenly reinterpreted as
 *   temporary. Theater ratio rises sharply (0.22 to 0.58 in the first six
 *   time units) as the reinterpretation narrative requires increasing
 *   performative reinforcement — the institutional machinery must
 *   continuously assert that the revelation is real and the prophet's
 *   authority remains unbroken. Theater then stabilizes (0.58 to 0.61 to 0.58
 *   at t=12, 18, 25) as the narrative matures and the contradiction between
 *   doctrine and practice becomes normalized within the community.
 *
 * KEY AGENTS:
 *   - Institutional leadership: sets and enforces the reinterpretation narrative; benefits from preserved authority.
 *   - Prophet: claims the new revelation; maintains status as God's mouthpiece; identity locked into the role.
 *   - Existing practitioners: bear the cost of theological incoherence; trapped by identity fusion and community dependence.
 *   - Theological tradition: non-agent; the doctrinal corpus that must now explain divine will-change.
 *   - Federal authorities: excluded from the institutional narrative; their pressure is reframed as irrelevant to a purely internal spiritual reinterpretation.
 *   - Dissident members: structurally silenced; objecting to the revelation means challenging the prophet.
 *   - Comparative theology observers: analytical seat; can document the contradiction but cannot adjudicate its internal legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.71).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation Reading: Practice Reversal via Prophetic Revelation").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'aff10be7-331c-4b65-b249-cdfaee67b9fd').
narrative_ontology:cs_kernel_codification('aff10be7-331c-4b65-b249-cdfaee67b9fd', fixed_text).
narrative_ontology:cs_authority_grounding('aff10be7-331c-4b65-b249-cdfaee67b9fd', lineage).
narrative_ontology:cs_interpretation_layer_present('aff10be7-331c-4b65-b249-cdfaee67b9fd').
narrative_ontology:cs_reading_relation('aff10be7-331c-4b65-b249-cdfaee67b9fd', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('aff10be7-331c-4b65-b249-cdfaee67b9fd', marriage_commitment_reversal__practice_doctrine_gap, coexists_with).
narrative_ontology:cs_axiom('aff10be7-331c-4b65-b249-cdfaee67b9fd', foundational, living_revelation_doctrine_operative).
narrative_ontology:cs_axiom_status(living_revelation_doctrine_operative, holdable).
narrative_ontology:cs_axiom_grounding('aff10be7-331c-4b65-b249-cdfaee67b9fd', living_revelation_doctrine_operative, theological).
narrative_ontology:cs_axiom('aff10be7-331c-4b65-b249-cdfaee67b9fd', foundational, prophetic_interpretive_authority_preserved).
narrative_ontology:cs_axiom_status(prophetic_interpretive_authority_preserved, holdable).
narrative_ontology:cs_axiom_grounding('aff10be7-331c-4b65-b249-cdfaee67b9fd', prophetic_interpretive_authority_preserved, deontological).
narrative_ontology:cs_reference_frame('aff10be7-331c-4b65-b249-cdfaee67b9fd', eternal_revealed_doctrine_binding).
narrative_ontology:cs_drift_state('aff10be7-331c-4b65-b249-cdfaee67b9fd', post_federal_legal_threat, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aff10be7-331c-4b65-b249-cdfaee67b9fd', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_authority).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_claim).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, existing_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, living_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrinal framework and enforces compliance with the reinterpretation. Faces contradiction: federal law prohibits the practice, the original doctrine claims it is revealed and eternal. Resolves via claiming new revelation reinterpreting God's will. This preserves institutional authority (the prophet still speaks for God) while enabling public compliance. The reinterpretation must be internally consistent enough to maintain theological credibility.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Claims direct revelation from the divine reinterpreting prior doctrine. Maintains legitimacy and interpretive authority by framing reversal not as capitulation but as updated divine will. The revelation narrative preserves the prophet's status as God's mouthpiece even as the practice is abandoned.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet, beneficiary).

% Have organized their lives and identities around the original revealed doctrine. The reinterpretation invalidates their practice retroactively. They must accept either that their prior obedience was wrong or that God changed His will mid-practice. Both frames are theologically and psychologically costly. They bear the cost of theological incoherence and identity disruption.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, existing_practitioners, payer,
    moderate, biographical, identity_locked, global).

% The entire doctrinal corpus claiming the original practice was revealed and binding. The reinterpretation requires explaining why a permanent revelation was temporary, straining the coherence of the authority structure itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_tradition, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_tradition).

% Applied legal and political pressure to end the practice. They are excluded from the internal theological conversation — their pressure is reframed as external coercion irrelevant to a purely internal spiritual reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_authorities, excluded,
    institutional, generational, mobile, national).

% Members who see the reinterpretation as illegitimate — either because God does not change His will, or because the decision was driven by external coercion. They cannot voice this objection within the institutional framework without challenging the prophet's authority, the foundation of their membership identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissident_membership, excluded,
    powerless, biographical, trapped, global).

% External scholars and analysts documenting the doctrine-practice reversal and comparing theological claims before and after. They see the structural properties but have no standing to adjudicate internal legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, comparative_theology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional authority and community continuity under existential pressure. The endogenous reinterpretation reading coordinates the need to comply with federal law while maintaining the narrative that the institution obeys God, not government. This is a coordination problem for leadership: how to reverse practice without admitting doctrinal error or external capitulation.
% TRANSFER_FUNCTION: Transfers the cost of the contradiction (theological inconsistency, identity disruption, loss of doctrinal coherence) from the institution to existing practitioners and the theological tradition. Existing practitioners and the tradition bear the burden of explaining why God's will changed; the institution collects the benefit of surviving external pressure while preserving its claim to prophetic authority.
% ABSENT_VOICES: Federal authorities are excluded from the theological justification narrative — the reinterpretation is framed as purely internal revelation, not response to external coercion. Dissident members who question the reinterpretation's legitimacy are silenced: objecting to the revelation means challenging the prophet, which dissolves the membership identity.
% DISAPPEARANCE_RATIONALE: If the reinterpretation narrative vanished and explicit acknowledgment replaced it that the reversal was external political capitulation, institutional leadership would lose its claim to ongoing prophetic authority. The entire theological legitimacy structure depends on the narrative that God, not federal pressure, reinterpreted the doctrine. The institution would face either admission of error or schism.
% FOUNDING_PROBLEM: The institution faced simultaneous demands it could not satisfy: federal law prohibited the practice, but the doctrine claimed it was revealed and binding. The reinterpretation solves this by claiming a new revelation superseding the old one, preserving theological authority while enabling compliance.
% FOUNDING_PROBLEM_CORROBORATION: The institution attests the founding problem was resolved by prophetic revelation. Federal authorities and independent theological historians attest the problem was resolved by federal coercion and institutional accommodation. Scholarly analysis shows the reinterpretation was authored after the legal threat, which supports the coercion reading. No corroborating source outside the benefiting parties (institutional leadership) accepts the revelation narrative as primary evidence rather than post-hoc theological cover.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness floor at 0.62 reflects that this constraint performs genuine institutional work: it preserves authority continuity and community coherence under existential threat. But it is not pure coordination because the cost of the reinterpretation is concentrated on practitioners and the theological tradition, not distributed across all beneficiaries and costs. Suppression at 0.71 is high because the institutional machinery must actively prevent members from articulating the contradiction (federal coercion + doctrine reversal = capitulation). Theater rising from 0.22 to 0.58 in the first six time units captures the moment when the narrative shifts from fact to performance: the reinterpretation must be continuously asserted and reinforced because it contradicts the prior revelation on its face. Theater stabilizes at 0.58+ thereafter because the narrative achieves normalization — the community has internalized the frame and no longer requires as much performative assertion. Accessibility_collapse at 0.48 reflects that alternatives remain visible: dissident members see the federal pressure, comparative theologians see the timing gap, the original doctrine is still written down. The constraint persists not because alternatives are invisible but because the institutional narrative is enforced more powerfully than any alternative. Resistance at 0.74 is high because substantial opposition exists within the membership — the shared time grid at every measurement point captures this ongoing resistance as the theater machinery must be continuously rebuilt.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership (agenda_setter, institutional power, identity_locked) should compute toward rope or tangled_rope: they solved a coordination problem (how to comply with federal law while preserving theological authority) and the solution preserved institutional continuity. Existing practitioners (payer, moderate power, identity_locked) should compute toward snare: they are trapped by identity fusion, face suppression of doctrinal objection, and bear the cost of theological incoherence while receiving no benefit. Federal authorities (excluded, institutional, mobile) have no seat in this constraint's computation — they are external to it. The dissident members (excluded, powerless, trapped) compute toward snare: they are silenced by the authority structure itself. The engine derives these per-seat divergences from the structural data — beneficiary/victim declarations, exit_options, power atoms — without needing explicit override. The claim (tangled_rope) reflects the institutional narrative: genuine coordination with asymmetric costs. The metrics (extractiveness 0.62, suppression 0.71, theater 0.58) describe the actual operation: the coordination is hybrid (real institutional problem solved, real theological cost imposed), which is exactly the tangled_rope definition.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership: beneficiary role, institutional power, identity_locked exit. Derives d near 0.15–0.25 (beneficiary end). Prophet: beneficiary role, institutional power, identity_locked exit. Same derivation. Existing practitioners: payer role, moderate power, identity_locked exit. Derives d near 0.65–0.75 (target end); the identity lock amplifies the target position because exit would require identity dissolution. Theological tradition: non-agent, payer role, civilizational time horizon. Derives d = 0.50 (neutral, though the non-agent status means it does not feed directionality). Federal authorities: excluded role, institutional power, mobile exit. Derives d near 0.0 (beneficiary end, though they are excluded from the constraint's direct operation). Dissident members: excluded role, powerless, trapped exit. Derives d near 0.75 (target end). The commentary on 'perspectival_gap' above captures why the payers and targets compute this way: the reinterpretation preserves the institution at their expense, and the identity_locked exit ensures they cannot escape the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint should NOT be classified as piton under the endogenous reinterpretation reading. Piton requires (a) the primary function has atrophied, and (b) the constraint persists by inertia with no concentrated beneficiary. Under this reading, the constraint's function is LIVE: institutional leadership has a genuine structural interest in preserving the reinterpretation narrative, and practitioners have a genuine (if costly) interest in maintaining community membership under the new doctrine. The institution has not abandoned the reinterpretation — it actively maintains it through teaching, priesthood training, institutional memory work, and pulpit enforcement. Theater is high (0.58) but theater does not define piton; theater measures performative ratio, not atrophied function. Under the EXOGENOUS reading (a sibling constraint in the same kernel), the constraint would approach piton: if the reversal is primarily caused by external coercion, the institution might maintain the reinterpretation only to avoid admitting capitulation, which would be pure inertial performance. But this reading models endogenous reinterpretation, which preserves the function. Mandatrophy arises if the founding problem (how to resolve the federal-doctrine contradiction) has become moot — not through resolution, but through the passage of time. If federal law is no longer contested and practitioners have normalized the reinterpretation over decades, the constraint might approach mandatrophy: the founding problem is dead (nobody challenges the law anymore), the world has rearranged (plural marriage is simply gone from public practice), but the reinterpretation narrative persists as institutional doctrine. The founding_problem_status field (contested) reflects this ambiguity: the problem is contested within the community (dissident members and theological historians question it) even if it is uncontested in the broader society.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_ambiguity,
    'Is the prophetic reinterpretation a genuine internal revelation of God''s changed will, or a post-hoc theological narrative authored to justify external capitulation?',
    'Doctrinal archaeology: compare the timing of the revelation claim to the federal legal threat; examine archival records of the revelation process; analyze whether the theological arguments for reinterpretation were developed before or after the coercive pressure.',
    'If the revelation is authentic (temporally primary, independently motivated), the constraint is genuine coordination (internal reinterpretation solving an institutional contradiction). If the revelation is post-hoc (temporally secondary, motivated by coercion), the constraint is extractive cover story — institutional leadership extracts the benefit of appearing obedient to God while actually obeying government.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_authenticity_ambiguity, empirical, 'Whether the revelation is primary cause or secondary justification.').

omega_variable(
    doctrine_invariance_question,
    'Does the claim that God''s will changed fundamentally undermine the authority of the original doctrine?',
    'Theological analysis: examine whether the institutional framework explicitly endorses living revelation, or whether prior revelation is claimed as eternal.',
    'If living revelation is core doctrine, the reinterpretation is coherent and the constraint measures institutional flexibility. If prior revelation is claimed as binding, the reinterpretation is a core contradiction and extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_invariance_question, conceptual, 'Whether institutional theology can sustain divine will-changes coherently.').

omega_variable(
    identity_lock_mechanism,
    'For practitioners, is the identity-lock mechanism structural (community dependence, isolation) or internalized (belief fusion, psychological investment)?',
    'Post-exit trajectory: do members who leave retain the reinterpretation narrative as legitimate?',
    'If structural, identity lock can be broken by material changes. If internalized, members carry the suppression with them. The measurement conflates both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Suppression mechanism: structural or internalized.').

omega_variable(
    kernel_reading_constraint_identity,
    'This constraint is ONE reading of the marriage_commitment_reversal kernel. The kernel has three readings: endogenous_reinterpretation (this one), exogenous_override, and practice_doctrine_gap. Are these three constraints or three measurements of one constraint?',
    'ε-invariance test: the three readings assign different causes (internal revelation vs. external pressure vs. structural contradiction), different victim sets (theological consistency vs. practitioners vs. the doctrine itself), and different extraction mechanisms. Under ε-invariance principle (DP-001), different causal stories with different observable consequences are different constraints, not different measurements.',
    'If they are three constraints (correct interpretation), each gets its own story file with its own ε, metrics, and classification. If they are one constraint with three measurement bases (wrong interpretation), the story would need to fold all three causal narratives into one ε-invariance statement, which is incoherent. The three readings are linked via network.affects_constraints, not folded into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_constraint_identity, conceptual, 'Whether kernel readings are separate constraints or multiple measurements of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t3, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement_basis(marr_tr_t3, observed).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement_basis(marr_tr_t6, observed).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(marr_tr_t12, observed).
narrative_ontology:measurement(marr_tr_t18, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 18, 0.61).
narrative_ontology:measurement_basis(marr_tr_t18, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(marr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t3, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(marr_be_t3, observed).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(marr_be_t6, observed).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(marr_be_t12, observed).
narrative_ontology:measurement(marr_be_t18, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(marr_be_t18, observed).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(marr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t3, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(marr_su_t3, observed).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(marr_su_t6, observed).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(marr_su_t12, observed).
narrative_ontology:measurement(marr_su_t18, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(marr_su_t18, observed).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(marr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_reversal kernel, which decomposes into three structurally distinct constraints with different causal mechanics. The endogenous_reinterpretation_reading models the institutional narrative (reversal via prophetic revelation). The exogenous_override_reading models the federal-pressure narrative (reversal despite doctrine). The practice_doctrine_gap reading models the structural contradiction (doctrine preserved, practice suspended, incoherence institutionalized). The three readings are mutually exclusive causal stories about the same historical event; ε-invariance requires separate files. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
