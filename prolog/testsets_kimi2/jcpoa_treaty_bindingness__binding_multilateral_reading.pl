% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA Binding Multilateral Treaty Framework
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   The JCPOA is read here as a binding multilateral treaty requiring
 *   consensus-based modification or dissolution. This reading embeds the
 *   arrangement in UNSC Resolution 2231, privileges the Joint Commission and
 *   UNSC over unilateral state action, and treats the IAEA verification
 *   architecture as a mandatory trigger for multilateral dispute resolution
 *   before any snapback. The constraint extracts asymmetrically from the
 *   Iranian state while subsidizing multilateral institutions and the
 *   non-proliferation regime. It is contested by sibling readings that treat
 *   the JCPOA as a provisional transactional framework or a graduated
 *   compliance scheme.
 *
 * KEY AGENTS:
 *   - Iranian state: Primary target (powerful/constrained) â bears extraction via enrichment caps, stockpile limits, and enhanced monitoring
 *   - US executive: Agenda setter (institutional/constrained) â co-architect bound by consensus constraints on unilateral withdrawal and sanctions reimposition
 *   - E3/EU collective: Agenda setter and secondary beneficiary (institutional/constrained) â administers dispute resolution and benefits from diplomatic centrality
 *   - IAEA verification regime: Beneficiary (institutional/constrained) â gains mandate, budget, and personnel from its verification role
 *   - UNSC mechanism: Beneficiary (institutional/constrained) â retains exclusive authority over sanctions snapback
 *   - NPT state parties: Beneficiary (organized/constrained) â benefit from norm stability and multilateral precedent
 *   - Regional actors excluded: Excluded (organized/trapped) â lack seat at the consensus table despite direct security exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.8).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA Binding Multilateral Treaty Framework").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '24d631a0-808d-4160-9e1c-0785674a7b13').
narrative_ontology:cs_kernel_codification('24d631a0-808d-4160-9e1c-0785674a7b13', formalized).
narrative_ontology:cs_authority_grounding('24d631a0-808d-4160-9e1c-0785674a7b13', lineage).
narrative_ontology:cs_interpretation_layer_present('24d631a0-808d-4160-9e1c-0785674a7b13').
narrative_ontology:cs_reading_relation('24d631a0-808d-4160-9e1c-0785674a7b13', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('24d631a0-808d-4160-9e1c-0785674a7b13', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('24d631a0-808d-4160-9e1c-0785674a7b13', foundational, unanimity_modification_required).
narrative_ontology:cs_axiom_status(unanimity_modification_required, holdable).
narrative_ontology:cs_axiom_grounding('24d631a0-808d-4160-9e1c-0785674a7b13', unanimity_modification_required, conventional).
narrative_ontology:cs_axiom('24d631a0-808d-4160-9e1c-0785674a7b13', foundational, multilateral_snapback_exclusive).
narrative_ontology:cs_axiom_status(multilateral_snapback_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('24d631a0-808d-4160-9e1c-0785674a7b13', multilateral_snapback_exclusive, conventional).
narrative_ontology:cs_reference_frame('24d631a0-808d-4160-9e1c-0785674a7b13', unsc_endorsed_multilateral_binding).
narrative_ontology:cs_drift_state('24d631a0-808d-4160-9e1c-0785674a7b13', post_us_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('24d631a0-808d-4160-9e1c-0785674a7b13', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council_mechanism).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_collective).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_state_parties).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepts quantitative limits on enrichment capacity, stockpile size, and related R&D, and permits an unprecedented monitoring regime by the IAEA. It receives sanctions relief in exchange, but under this reading the relief is embedded in a binding structure that other parties cannot undo unilaterally. Its exit is constrained by the threat of multilateral snapback and by the political and economic cost of abandoning the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_state, payer,
    powerful, generational, constrained, global).

% Co-chairs the Joint Commission and is bound by the consensus requirement to modify or dissolve the arrangement. Under this reading, unilateral withdrawal is a legally constrained act that violates the binding framework, and reimposing sanctions requires UNSC consensus rather than national determination. Its autonomy to shift Iran policy unilaterally is curtailed by the multilateral structure it helped create.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, us_executive, agenda_setter,
    institutional, biographical, constrained, global).

% Coordinates sanctions relief, convenes the Joint Commission, and manages the dispute resolution mechanism. They benefit from a process that elevates European diplomatic architecture and prevents unilateral great-power disruption of the non-proliferation order. Their capacity to alter the framework alone is limited by the consensus rule.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_collective, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_collective, beneficiary).

% Conducts monitoring and verification under expanded access arrangements mandated by the JCPOA. Its technical findings feed into the multilateral dispute resolution channel rather than triggering unilateral responses. The regime gains budget, personnel, and institutional authority from its central role, but its mandate is tied to the treaty's survival.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_regime, beneficiary,
    institutional, generational, constrained, global).

% Resolution 2231 endorsed the JCPOA and embedded the snapback procedure in UNSC procedure. Under this reading, the Council retains exclusive jurisdiction over reimposing sanctions, and any modification requires its consensus. The mechanism benefits from sustained relevance in non-proliferation governance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council_mechanism, beneficiary,
    institutional, civilizational, constrained, global).

% The broader community of NPT states benefits from the precedent that enrichment programs can be constrained through binding multilateral negotiation rather than coercion or conflict. The regime's stability depends on the framework holding, and their diplomatic capital is invested in its continuation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_state_parties, beneficiary,
    organized, civilizational, constrained, global).

% Israeli and Gulf state security establishments are directly affected by Iranian nuclear capacity but are not parties to the JCPOA and have no vote in the consensus mechanism. They argue that the binding framework constrains their allies more than it constrains Iran, and they are structurally excluded from snapback decisions despite bearing the greatest security risk.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_actors_excluded, excluded,
    organized, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral disruption of a negotiated nuclear limitation regime by embedding modification and dissolution in consensus-based multilateral processes, ensuring that no single party can alter or exit without collective agreement.
% TRANSFER_FUNCTION: Transfers decision-making autonomy over sanctions reimposition and treaty modification from individual state parties to the UNSC consensus mechanism and Joint Commission; transfers Iranian enrichment sovereignty to IAEA monitoring and multilateral dispute resolution.
% ABSENT_VOICES: Regional actors directly threatened by Iranian nuclear activity, such as Israeli and Gulf state security establishments, are not parties to the consensus mechanism and cannot block sanctions relief or dispute outcomes; they would argue for unilateral security guarantees but are structurally excluded.
% DISAPPEARANCE_RATIONALE: If the binding multilateral framework vanished, the snapback mechanism would revert to unilateral national determinations, sanctions reimposition would fragment across jurisdictions, IAEA verification would lose its legal mandate, and the non-proliferation norm would weaken as parties pursue bilateral or unilateral alternatives.
% FOUNDING_PROBLEM: The risk of unilateral military action against Iran's nuclear program and the collapse of the NPT-inspired sanctions regime due to fragmented great-power coordination.
% FOUNDING_PROBLEM_CORROBORATION: E3/EU and IAEA attest the problem remains live, citing Iranian enrichment escalations post-2018. US political factions and regional actors attest the problem is better addressed by unilateral pressure or military containment; independent nuclear-policy analysts outside the beneficiary set note that the binding framework has partially solved the immediate proliferation risk but that the consensus requirement now impedes timely response.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.65 reflects significant but not total extraction: Iran surrenders substantial sovereign nuclear autonomy, yet receives real sanctions relief and dispute-process protections. Suppression at 0.80 reflects the active enforcement required to maintain consensus against unilateral withdrawal and to keep Iran within the monitoring architecture. Theater ratio at 0.40 reflects increasing performative diplomacy as US withdrawal and Iranian partial compliance eroded the binding framework's practical coherence while its formal structure remained intact. Accessibility collapse at 0.50 indicates that alternatives (unilateral military action, bilateral deals, unilateral sanctions) remain rhetorically available but are structurally delegitimized by the binding framework. Resistance at 0.50 captures Iranian incremental non-compliance and US political rejection of the consensus constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian seat experiences the constraint as heavy extraction with limited exit, while the institutional seats (UNSC, IAEA, E3/EU) experience it as legitimate coordination that stabilizes their authority and the non-proliferation norm. The US executive seat is split: it helped architect the constraint but chafes under its limits on unilateral action. The engine will compute high directionality for Iran and low directionality for the institutions, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   BeneficiariesâUNSC mechanism, IAEA verification regime, E3/EU collective, and NPT state partiesâare structurally subsidized by the constraint: it amplifies their authority, budgets, or diplomatic centrality. These agents have constrained exit because their identities and functions are fused with the treaty architecture, but their costs are low. The Iranian state is the declared payer: it bears quantifiable limits on enrichment and accepts intrusive monitoring. Its exit is constrained by the threat of multilateral snapback and economic isolation. Directionality therefore runs from beneficiary toward target, with Iran sitting near the full-target end and the institutions near the full-beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy mislabeling because it carries a live coordination functionâpreventing nuclear proliferation through verified limitsâand a live extraction functionâtransferring Iranian nuclear sovereignty to multilateral bodies. Neither function has atrophied into pure performance. The theater ratio is moderate, not piton-level, because the IAEA still conducts real inspections and the Joint Commission still convenes. The binding multilateral reading prevents the coordination from being dismissed as a cover story (snare) while acknowledging that the coordination is asymmetrically costly (tangled rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the JCPOA''s structural classification change if the kernel is read as a provisional transactional framework or a graduated compliance scheme rather than a binding multilateral treaty?',
    'Comparative analysis of the three compiled constraint stories in the jcpoa_treaty_bindingness kernel family, examining directionality, beneficiary structure, and effective extraction per seat.',
    'If the transactional provisional reading is more descriptively accurate, the current constraint''s extractiveness and suppression are overstated and the constraint collapses toward a lower-extraction coordination mechanism or a snare depending on enforcement; if the graduated compliance reading dominates, the dispute-resolution delay is a feature not a bug and the coordination-extraction balance shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural uncertainty from contested kernel readings').

omega_variable(
    consensus_enforceability,
    'Is the consensus-based modification requirement enforceable against a permanent UNSC member determined to withdraw or reimpose sanctions unilaterally?',
    'Historical case study of the 2018 US withdrawal and subsequent failure to activate snapback via UNSC consensus; legal analysis of whether Resolution 2231 created genuine procedural locks or merely political expectations.',
    'If unenforceable, the suppression metric is overstated, the constraint is less binding than claimed, and the classification tilts toward a piton (theatrical maintenance of a binding fiction) or a rope (voluntary coordination); if enforceable, the binding reading is structurally intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_enforceability, empirical, 'Whether the consensus lock is real or performative').

omega_variable(
    iranian_sovereignty_cost,
    'What is the quantifiable strategic and economic cost to the Iranian state of the enrichment caps, stockpile limits, and Additional Protocol-like monitoring under the JCPOA?',
    'Independent economic assessment of foregone nuclear-industry development and sovereign control over the fuel cycle, compared to the value of sanctions relief received.',
    'A high net cost would raise base extractiveness and strengthen the target classification for the Iranian seat; a cost near zero or negative would suggest symmetric coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iranian_sovereignty_cost, empirical, 'Magnitude of extraction from the Iranian seat').

omega_variable(
    regional_exclusion_legitimacy,
    'Does the structural exclusion of regional security actors from the consensus mechanism delegitimize the binding multilateral framework or merely relocate resistance outside the constraint?',
    'Analysis of regional-state behavior (covert action, lobbying for unilateral pressure) as a proxy for whether exclusion amplifies extraconstraint resistance.',
    'If exclusion fuels extraconstraint resistance, the accessibility_collapse metric is overstated and the constraint''s stability is lower than the multilateral framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_exclusion_legitimacy, empirical, 'Impact of excluded regional actors on constraint stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, un_sc_sanctions_architecture).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_nuclear_program).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jcpoa_treaty_bindingness kernel family, decomposed per the epsilon-invariance principle because the colloquial label 'JCPOA' conflates structurally distinct readings (binding multilateral, graduated compliance, transactional provisional).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
