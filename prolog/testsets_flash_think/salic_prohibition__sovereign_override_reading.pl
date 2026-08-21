% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Revocable Sovereign Prerogative
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint is the 'sovereign override' reading of the Salic Law
 *   kernel, asserting that Salic Law is a positive law subject to sovereign
 *   legislative authority. This reading posits that a monarch, through acts
 *   like the Pragmatic Sanction, can modify or set aside traditional Salic
 *   prohibitions on female succession to ensure dynastic continuity.
 *   Challengers to such sovereign acts are considered rebels against
 *   legitimate authority, and their suppression is a necessary component of
 *   maintaining the constraint. This reading contrasts with the 'immutable
 *   mandate' reading (Salic Law as divine/natural law) and the 'cognatic
 *   reversion' reading (Salic Law as a Frankish anachronism).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.45).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.65).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Revocable Sovereign Prerogative").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'edfd606a-11dd-400d-8143-b6deea49f258').
narrative_ontology:cs_kernel_codification('edfd606a-11dd-400d-8143-b6deea49f258', formalized).
narrative_ontology:cs_authority_grounding('edfd606a-11dd-400d-8143-b6deea49f258', lineage).
narrative_ontology:cs_interpretation_layer_present('edfd606a-11dd-400d-8143-b6deea49f258').
narrative_ontology:cs_reading_relation('edfd606a-11dd-400d-8143-b6deea49f258', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('edfd606a-11dd-400d-8143-b6deea49f258', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('edfd606a-11dd-400d-8143-b6deea49f258', foundational, sovereign_legislative_supremacy).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('edfd606a-11dd-400d-8143-b6deea49f258', sovereign_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('edfd606a-11dd-400d-8143-b6deea49f258', foundational, dynastic_continuity_paramount).
narrative_ontology:cs_axiom_status(dynastic_continuity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('edfd606a-11dd-400d-8143-b6deea49f258', dynastic_continuity_paramount, instrumental).
narrative_ontology:cs_reference_frame('edfd606a-11dd-400d-8143-b6deea49f258', sovereign_legislative_supremacy).
narrative_ontology:cs_drift_state('edfd606a-11dd-400d-8143-b6deea49f258', contemporary_constitutional_monarchy_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('edfd606a-11dd-400d-8143-b6deea49f258', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, dynastic_lineage).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_claimants_superseded).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rival_dynastic_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, national_nobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises the authority to modify succession laws, ensuring dynastic continuity and stability. Benefits directly from the flexibility to adapt succession rules to political realities, consolidating power and legitimacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the clarity and stability of succession, even if the rules can be changed by the sovereign. This reading ensures the continuation of their house, albeit under the sovereign's ultimate discretion.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_lineage, beneficiary,
    institutional, generational, constrained, national).

% Their claims to the throne are denied or made conditional by sovereign legislative acts, despite potential genealogical proximity. They bear the cost of being excluded from succession by a sovereign decree.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_claimants_superseded, payer,
    powerless, generational, trapped, national).

% Their challenges to the sovereign's authority or to the modified succession are suppressed, often violently. They pay the cost of opposing the established sovereign prerogative.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_dynastic_factions, payer,
    organized, generational, constrained, national).

% Benefits from the stability of the realm and clear succession, avoiding civil war, even if their preferred candidate is not chosen. They are coordinated into supporting the sovereign's chosen line.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, national_nobility, beneficiary,
    organized, generational, constrained, national).

% Analyze the historical and legal precedents of sovereign authority over succession, documenting its evolution and contestation. They provide an analytical perspective on the constraint's operation and legitimacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit flexible, line of succession to prevent civil war and dynastic instability, allowing for sovereign adaptation to political realities and ensuring the continuity of the state.
% TRANSFER_FUNCTION: Transfers the right to rule and dynastic legitimacy to the sovereign's chosen heir, potentially overriding traditional Salic prohibitions, and transfers power away from those whose claims are denied by sovereign decree.
% ABSENT_VOICES: Those who believe in an immutable, divinely ordained Salic Law (the 'immutable mandate' reading) or those who believe it was never truly binding on their territories (the 'cognatic reversion' reading) are excluded from the sovereign's legislative process and are often suppressed as rebels or anachronists.
% DISAPPEARANCE_RATIONALE: If the sovereign's prerogative to define and adapt succession laws vanished overnight, the state would face immediate and severe instability, potentially leading to civil war, foreign intervention, or the collapse of the monarchy as various claimants (including female ones) would vie for power without a clear, enforceable framework.
% FOUNDING_PROBLEM: To prevent endless dynastic disputes and civil wars over succession, particularly when direct male heirs were lacking or unsuitable, and to consolidate sovereign power by providing a mechanism for the monarch to adapt succession rules to ensure stability.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of succession crises, diplomatic treaties (e.g., the Pragmatic Sanction itself), and constitutional documents from outside the immediate dynastic beneficiaries attest to the problem and the solution's intent. Legal scholars and historians corroborate the ongoing relevance of clear succession rules for state stability.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).
:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as the sovereign's prerogative, while ensuring stability, still extracts from those whose claims are superseded. Suppression is substantial (0.65) because the sovereign's authority to override tradition often requires active enforcement against rival claimants or factions. Theater ratio is low (0.20) as the acts of sovereign override are direct assertions of power, not primarily performative. The temporal measurements reflect a period where sovereign power to define succession was increasingly asserted and enforced, leading to a gradual rise in extractiveness and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign's perspective, this constraint is a necessary tool for statecraft and dynastic survival, a legitimate exercise of authority. From the perspective of superseded claimants or rival factions, it is an arbitrary imposition of power, denying their rightful claims. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch and the dynastic lineage are clear beneficiaries, gaining stability and flexibility in succession. Female claimants and rival dynastic factions are payers, bearing the cost of their exclusion or suppression. The national nobility benefits from the stability provided by clear succession, even if their preferred candidate is not chosen. Legal scholars observe and analyze the system without direct benefit or cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_power_vs_legitimacy,
    'Is the sovereign''s ability to override Salic Law based purely on legal prerogative and constitutional principle, or on their political and military power to enforce such a decree?',
    'Analysis of historical instances where sovereign overrides were attempted but failed due to lack of political support or military strength, versus those that succeeded purely on legal grounds.',
    'If primarily based on raw power, the constraint''s effective suppression and extractiveness are higher, as its persistence relies more on coercion than on accepted legal norms. If purely legal, it leans more towards a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_power_vs_legitimacy, empirical, 'Distinguishes between legal authority and de facto power in sovereign override.').

omega_variable(
    historical_contingency_vs_principle,
    'Is the ''revocable positive law'' status a consistent legal principle applied across different historical contexts, or a post-hoc justification for politically expedient dynastic changes?',
    'Comparative legal history across multiple monarchies and time periods, examining the consistency of legal arguments for sovereign override versus the political circumstances surrounding each instance.',
    'If primarily a contingent justification, the constraint''s claimed coordination function is weaker, and its extractiveness is more pronounced, as it serves to legitimize power grabs rather than consistent governance. If a consistent principle, its coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_contingency_vs_principle, conceptual, 'Examines the principled vs. contingent nature of sovereign override.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__sovereign_override_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__sovereign_override_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(sali_tr_t80, salic_prohibition__sovereign_override_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__sovereign_override_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__sovereign_override_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__sovereign_override_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(sali_be_t80, salic_prohibition__sovereign_override_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__sovereign_override_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__sovereign_override_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__sovereign_override_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(sali_su_t80, salic_prohibition__sovereign_override_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__sovereign_override_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
