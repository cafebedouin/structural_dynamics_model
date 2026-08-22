% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: State Commitment Installation via Hybrid Cascade (Apex-Initiated, Fringe-Stabilized)
 *   domain: historical/political/cultural
 *
 * SUMMARY:
 *   A state apex institution (crown, central government, ideological
 *   authority) installs a new commitment — legal doctrine, administrative
 *   procedure, religious reform, or cultural norm — as formal policy. This
 *   reading asserts that the commitment cascades downward through
 *   institutional hierarchies and reaches fringe communities (local
 *   administrators, provincial courts, folk practitioners, cultural
 *   custodians). Critically, the commitment's stabilization depends on fringe
 *   actors' validation through local reinterpretation, adaptive application,
 *   and tacit acceptance. Without fringe validation, the commitment remains
 *   theatrically enforced but structurally unstable. With it, the commitment
 *   embeds into practice. The apex institution benefits from both the
 *   commitment's original design AND from the legitimacy borrowed from fringe
 *   acceptance; fringe communities must align their interpretive frameworks
 *   and practices to the mandate while gaining limited influence over how it
 *   is locally expressed. This reading is ONE interpretation of how state
 *   authority diffuses and stabilizes new claims — neither fully endogenous
 *   (fringe climbing upward) nor fully exogenous (unilateral imposition), but
 *   hybrid: initiated from apex, requiring fringe stabilization, extracting
 *   legitimacy from below.
 *
 * KEY AGENTS:
 *   - apex_institution: Central authority (state, church, crown) with mandate to install new commitment. Structural position: powerful, generational horizon, arbitrage exit (can shift commitment if necessary).
 *   - legitimation_intermediaries: Institutional layers (provincial courts, bishops, regional administrators, licensed practitioners) that translate apex commitment downward and validate it upward. Structural position: organized, biographical horizon, constrained exit.
 *   - fringe_communities: Local communities, folk practitioners, cultural custodians, non-elite populations directly subject to the commitment. Structural position: powerless to moderate, biographical horizon, identity-locked or trapped exit.
 *   - competing_authority_structures: Pre-existing local legitimacy claims (customary law, rival ideological authorities, established cultural practices) displaced or subordinated by the new commitment. Structural position: excluded, generational horizon, trapped exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "State Commitment Installation via Hybrid Cascade (Apex-Initiated, Fringe-Stabilized)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical/political/cultural").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '366ba537-ac7b-4e9b-9640-aa0f636fd545').
narrative_ontology:cs_kernel_codification('366ba537-ac7b-4e9b-9640-aa0f636fd545', fixed_text).
narrative_ontology:cs_authority_grounding('366ba537-ac7b-4e9b-9640-aa0f636fd545', lineage).
narrative_ontology:cs_interpretation_layer_present('366ba537-ac7b-4e9b-9640-aa0f636fd545').
narrative_ontology:cs_reading_relation('366ba537-ac7b-4e9b-9640-aa0f636fd545', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('366ba537-ac7b-4e9b-9640-aa0f636fd545', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('366ba537-ac7b-4e9b-9640-aa0f636fd545', foundational, fringe_validation_structurally_necessary).
narrative_ontology:cs_axiom_status(fringe_validation_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('366ba537-ac7b-4e9b-9640-aa0f636fd545', fringe_validation_structurally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('366ba537-ac7b-4e9b-9640-aa0f636fd545', foundational, cascade_legitimacy_asymmetric_extraction).
narrative_ontology:cs_axiom_status(cascade_legitimacy_asymmetric_extraction, holdable).
narrative_ontology:cs_axiom_grounding('366ba537-ac7b-4e9b-9640-aa0f636fd545', cascade_legitimacy_asymmetric_extraction, deontological).
narrative_ontology:cs_reference_frame('366ba537-ac7b-4e9b-9640-aa0f636fd545', apex_hierarchical_diffusion).
narrative_ontology:cs_drift_state('366ba537-ac7b-4e9b-9640-aa0f636fd545', contemporary_globalized_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('366ba537-ac7b-4e9b-9640-aa0f636fd545', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_institution).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimation_intermediaries).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, cultural_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimation_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority (state, church, crown, ideological body) that initiates and mandates the new commitment. Sets the formal policy, enforces compliance at institutional levels, and benefits from the commitment's adoption by fringe communities because their acceptance legitimates the policy. Can shift or abandon the commitment if necessary, or impose it more forcefully if fringe validation is inadequate. Primary beneficiary of the validation mechanism.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Provincial administrators, bishops, licensed practitioners, institutional layers between apex and fringe. They translate the apex mandate downward, validate it through formal procedures and ceremonies, absorb and redirect fringe resistance, and report the fringe's acceptance back to apex. They benefit from the cascade by gaining enforcement authority and from the validation (reduced direct resistance when the commitment appears locally accepted). They also bear costs: pressure from apex to enforce, pressure from fringe to moderate, and the constant work of brokering between mandate and reality.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimation_intermediaries, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimation_intermediaries, payer).

% Local communities, folk practitioners, cultural custodians, populations directly subject to the new commitment. They must adapt their existing practices and interpretive frameworks to the mandate or be subject to enforcement. They have limited choice: exit means abandoning their community and cultural identity, not just the constraint. They gain the coordination benefit of a unified framework, but the cost is subordination of local authority structures. Their reinterpretation of the commitment is necessary for stabilization but is presented as validating the apex's original vision rather than as genuine local adaptation. They are the structural victims of the arrangement.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_communities, payer,
    powerless, biographical, identity_locked, local).

% Pre-existing sources of legitimacy and authority in fringe communities (customary law, rival religious authorities, established cultural practices, elder councils). The cascade structurally subordinates and displaces these sources, and they are actively excluded from the validation process. They would object that the new commitment is imported and illegitimate, but their objections are managed through enforcement, co-optation of their representatives, or marginalization. They are locked into place by institutional geography but barred from the conversation that restructures their authority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, competing_authority_structures, excluded,
    moderate, generational, trapped, local).

% Scholars and analysts who examine the historical record of how apex commitments diffuse and stabilize. They trace whether fringe validation is genuinely constraining on apex design, whether the mechanism is necessary for stability, and whether fringe interpretation is local autonomy or sophisticated extraction. They have no seat in the arrangement but can render judgment on its structural character.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, comparative_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_institution).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The apex institution must install new commitments (legal doctrines, administrative procedures, cultural norms, religious reforms) across heterogeneous, pre-existing local structures. The cascade mechanism solves the problem of diffusing authority claims through institutional hierarchies and stabilizing them in local practice. Without the fringe validation layer, commitments remain unembedded, theatrically enforced, and unstable. With it, they become self-reproducing through local adoption and reinterpretation.
% TRANSFER_FUNCTION: Moves legitimacy upward (fringe acceptance is reported as apex validation, boosting the commitment's authority) and moves interpretive constraint downward (fringe communities must align their practices to the mandate, losing autonomy over meaning-making). Also moves enforcement cost: fringe communities bear the cost of adaptation, displacement of competing local authorities, and identity disruption.
% ABSENT_VOICES: Competing pre-existing authority structures (customary law, rival religious authorities, elder councils, folk practitioners) are structurally excluded. They would testify that the commitment is imported and illegitimate, that fringe communities are coerced into acceptance, and that local meaning-making is not consent. They are kept out of the validation process through institutional mechanism (they have no official seat at the table) and through enforcement pressure.
% DISAPPEARANCE_RATIONALE: If the cascade mechanism vanished, apex commitments would either fail to stabilize (they would remain unembedded, theatrically enforced but not self-reproducing), or the apex would shift to unilateral exogenous imposition (more direct enforcement, less fringe validation, more resistance). The fringe communities would lose the coordination benefit of a unified framework, but they would regain interpretive autonomy and local authority structures would re-emerge. The intermediate institutional layer would lose its brokering function and enforcement authority.
% FOUNDING_PROBLEM: State formation and institutional integration across diverse, pre-existing local structures require new commitments to cascade through hierarchies and be stabilized in local practice. The apex institution cannot govern directly at the fringe; it must work through intermediaries and rely on fringe communities' tacit acceptance. The founding problem is: how does a new commitment gain legitimacy and stability as it moves downward through heterogeneous local contexts?
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live in contemporary state formation, religious reform, and administrative expansion. Historians of the French Revolution, the Meiji Restoration, colonial administration, and religious reformation all document that apex commitments require fringe validation to stabilize. Scholars outside the benefiting institutions (the apex and intermediaries) attest that the founding problem persists: commitments that skip fringe validation persistently fail to embed or generate resistance disproportionate to their scope. The problem is not solved; the mechanism is the primary method for managing it.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the apex institution extracts legitimacy from fringe validation (the fringe actors' tacit or explicit acceptance becomes the authority's cover story) while constraining fringe interpretive autonomy. The constraint is NOT pure extraction because genuine coordination happens: the fringe communities do need a unified framework, and the apex institution does provide one (even if it arrives as mandate). Suppression is substantial (0.58) because fringe resistance to the new commitment is actively managed through enforcement, co-optation, and the threat of exit denial. Theater is moderate-high (0.41): a significant share of enforcement activity is performative validation — the cascade generates public ceremonies, formal acceptances, and ritual confirmations that stage fringe endorsement, whether or not the underlying practice has shifted. The measurement series shows extractiveness rising to midpoint (t=15) as the commitment penetrates and stabilizes, then stabilizing thereafter; suppression requirement drops as fringe adaptation makes explicit coercion less necessary. Theater rises initially (validation ceremonies are most dense when stabilization is uncertain) then plateaus (once embedded, the commitment requires less performative validation). This pattern is consistent with the hybrid reading: early-stage extraction of legitimacy through staged validation, later-stage stability from genuine fringe adoption.
 *
 * PERSPECTIVAL GAP:
 *   The seated classification divergence is the core signal of this reading. From the apex institution's seat, the cascade is ROPE (genuine coordination + beneficence). From the fringe community's seat, it is SNARE (imposed mandate + extraction of legitimacy). From the intermediary seat, it is TANGLED ROPE (coordination function + asymmetric extraction). The engine computes each seat independently from the structural data (power, exit, beneficiary/victim declaration); the authored claim reflects the hybrid reading's assertion that all three seats are simultaneously accurate descriptions of the same constraint, seen from different structural positions. The divergence is NOT a measurement error — it is the phenomenon the hybrid reading is about.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex institution and legitimation intermediaries experience the constraint differently than fringe communities. From apex/intermediary position: the cascade is a coordination mechanism solving the problem of diffusing new authority claims across heterogeneous populations — the constraint BENEFITS them (they gain the legitimacy of fringe acceptance). Exit options are mobile/arbitrage at apex level (the institution can change the commitment if necessary), constrained at intermediary level (the intermediaries must operate within the mandate). Directionality for apex/intermediaries leans toward beneficiary (low d, d ≈ 0.25-0.35). From fringe position: the cascade is an imposed mandate requiring their reinterpretation and tacit acceptance under threat — they are TARGETS of the constraint (they bear the cost of adaptive interpretation, identity disruption, subordination of local authority). Exit options are identity_locked (fringe communities cannot simply leave their locality, and their cultural identity is implicated in the local authority structures being displaced) or trapped (geographical/economic ties to the locality). Directionality for fringe communities leans toward target (high d, d ≈ 0.75-0.85). Legitimation intermediaries occupy a liminal position: they benefit from the cascade (they gain enforcement authority and reduced local resistance when the commitment is validated) but also bear costs (they must broker between apex mandate and fringe resistance, absorbing pressure from both directions). Their directionality is near symmetric (d ≈ 0.45-0.55).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what the cascade was installed to solve) is the integration and legitimation of new state/apex authority across heterogeneous, pre-existing local structures. The fringe validation mechanism addresses this genuinely: without it, the commitment remains a dead letter in localities that refuse tacit acceptance. However, the mechanism also extracts legitimacy — the apex institution claims the fringe's acceptance as evidence of the commitment's legitimacy, even when that acceptance is coerced, performed, or a bare minimum necessary adaptation. The constraint does not meet the mandatrophy gate because the founding problem remains LIVE: contemporary state formation, religious reform, and cultural policy still depend on cascading commitments through institutional hierarchies and stabilizing them via fringe validation. The mechanism persists because it genuinely solves a real coordination problem — but the extraction persists alongside it, not as drift but as structural design. This is the distinguishing feature of tangled rope: the coordination function is real AND the extraction is real AND they are structurally entangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apex_autonomy_vs_fringe_cooptation,
    'Does the fringe validation process genuinely constrain the apex institution, or does it merely absorb local resistance through performative incorporation?',
    'Structural comparison of apex commitments that underwent fringe validation vs. those imposed without it: do validated commitments show measurable content drift from their apex formulation, or do they pass through fringe response unchanged?',
    'If genuinely constrained, the arrangement is hybrid coordination + extraction (tangled rope). If performative, it is pure extraction disguised as consultation (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_autonomy_vs_fringe_cooptation, empirical, 'Whether fringe validation is structurally binding or theatrically absorptive.').

omega_variable(
    hybrid_vs_exogenous_boundary,
    'What distinguishes this reading (cascade requiring stabilization) from the exogenous imposition reading (top-down installation by authority)?',
    'The pivot is the necessity claim: does the apex institution''s mandate itself demand fringe participation for legitimacy, or is fringe participation an optional supplementary layer? This reading asserts the cascade is STRUCTURALLY DEPENDENT on fringe validation; exogenous reading asserts authority can install without it.',
    'If the boundary is empirically unstable (apex can and does install without fringe validation), the readings are not distinct constraints but two readings of the same constraint at different confidence levels. If apex has historically failed to stabilize commitments that skipped fringe validation, the hybrid reading''s dependency claim is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_exogenous_boundary, conceptual, 'Whether fringe validation is structurally necessary (hybrid) or strategically optional (exogenous imposition).').

omega_variable(
    endogenous_climb_vs_cascade_inversion,
    'Can commitments originating from fringe institutional actors climb to apex authority through demonstrated superiority, or does this reading assert that only apex-initiated commitments cascade downward?',
    'Historical trace of commitment origin: do documented cases show fringe-origin successful climbs to apex adoption, or are all durable commitments apex-installed and then cascaded?',
    'If climb-from-fringe cases are empirically present, the endogenous and hybrid readings coexist (different pathways to legitimacy). If all durable commitments follow apex-cascade, the endogenous reading is foreclosed or empirically hollow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_climb_vs_cascade_inversion, empirical, 'Whether fringe-to-apex trajectory is possible (endogenous coexists) or structurally barred (hybrid monopolizes apex-initiated pathways).').

omega_variable(
    local_interpretation_as_extraction,
    'When the apex commitment is reinterpreted by fringe communities during validation, does this reinterpretation constitute genuine local autonomy or a sophisticated capture of local meaning-making for apex legitimacy?',
    'Narrative analysis of fringe actors'' internal discourse: do they experience their interpretations as adaptive compromises (coordination frame) or as necessary tactical neutralizations of an unwanted mandate (extraction frame)?',
    'If experienced as compromise, the extraction score should be lower and the arrangement leans rope. If experienced as neutralization, extraction score should be higher and the constraint leans snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_interpretation_as_extraction, preference, 'Whether local interpretation represents genuine bargaining power or sophisticated legitimacy extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stat_tr_t0, projected).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(stat_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stat_be_t0, projected).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(stat_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(stat_su_t0, projected).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(stat_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'state_commitment_installation_mechanism', alongside 'endogenous_climb_reading' and 'exogenous_imposition_reading'. Each reading instantiates a different ε and different beneficiary/victim structure, though they share a common referent (the historical process of how apex commitments stabilize through institutions). The three readings form a kernel family; all members are linked via affects_constraints to indicate they are alternative decompositions of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
