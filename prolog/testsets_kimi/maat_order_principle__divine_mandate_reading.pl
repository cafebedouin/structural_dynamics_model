% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate Reading â Royal Conduit of Cosmic Order
 *   domain: political/religious/ancient_history
 *
 * SUMMARY:
 *   This constraint story instantiates the divine_mandate_reading of the
 *   maat_order_principle kernel. In this reading, Ma'at is not a mutual or
 *   distributed obligation but a unidirectional flow from cosmic divine order
 *   through the Pharaoh to Egyptian society. The ruler embodies Ma'at and
 *   cannot violate it by definition, placing him outside the constraint
 *   system as its source rather than its subject. This reading justifies
 *   royal extraction of surplus and labor as cosmic necessity, while actively
 *   suppressing alternative theological interpretations (reciprocity and
 *   distributed maintenance) that would impose obligations on the ruler or
 *   decentralize responsibility. It functions as a commitment system with a
 *   theological kernel, where the priesthood and scribal class serve as the
 *   interpretive layer absorbing empirical challenges (famine, dynastic
 *   collapse, military defeat) without allowing kernel revision.
 *
 * KEY AGENTS:
 *   - Pharaonic house: primary agenda setter and beneficiary (institutional/universal/arbitrage) â claims exclusive divine conduit status and receives absolute extraction.
 *   - Temple establishment: secondary agenda setter and beneficiary (institutional/national/constrained) â administers cultic legitimation and suppresses alternative readings.
 *   - Scribal bureaucracy: beneficiary (organized/national/constrained) â produces and archives the ideological narrative.
 *   - Peasant cultivators: primary payer (powerless/local/trapped) â bear surplus extraction under cosmic necessity framing.
 *   - Alternative theologians: excluded (moderate/national/trapped) â hold suppressed reciprocal or distributed readings of Ma'at.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.9).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate Reading â Royal Conduit of Cosmic Order").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "political/religious/ancient_history").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '9b87641a-dfc0-4fe9-80d0-af45d7149149').
narrative_ontology:cs_kernel_codification('9b87641a-dfc0-4fe9-80d0-af45d7149149', formalized).
narrative_ontology:cs_authority_grounding('9b87641a-dfc0-4fe9-80d0-af45d7149149', lineage).
narrative_ontology:cs_interpretation_layer_present('9b87641a-dfc0-4fe9-80d0-af45d7149149').
narrative_ontology:cs_reading_relation('9b87641a-dfc0-4fe9-80d0-af45d7149149', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('9b87641a-dfc0-4fe9-80d0-af45d7149149', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('9b87641a-dfc0-4fe9-80d0-af45d7149149', foundational, pharaoh_exclusive_maat_conduit).
narrative_ontology:cs_axiom_status(pharaoh_exclusive_maat_conduit, holdable).
narrative_ontology:cs_axiom_grounding('9b87641a-dfc0-4fe9-80d0-af45d7149149', pharaoh_exclusive_maat_conduit, theological).
narrative_ontology:cs_axiom('9b87641a-dfc0-4fe9-80d0-af45d7149149', foundational, royal_action_self_justifying).
narrative_ontology:cs_axiom_status(royal_action_self_justifying, holdable).
narrative_ontology:cs_axiom_grounding('9b87641a-dfc0-4fe9-80d0-af45d7149149', royal_action_self_justifying, theological).
narrative_ontology:cs_reference_frame('9b87641a-dfc0-4fe9-80d0-af45d7149149', pharaonic_divine_absolutism).
narrative_ontology:cs_drift_state('9b87641a-dfc0-4fe9-80d0-af45d7149149', first_intermediate_period, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9b87641a-dfc0-4fe9-80d0-af45d7149149', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_house).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_establishment).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, peasant_cultivators).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, divine_kingship_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, centralized_cosmological_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims direct descent from the solar deity and exclusive conduit status for Ma'at. Receives agricultural surplus, corvÃ©e labor, tribute, and absolute political obedience framed as cosmic necessity. Royal action is definitionally Ma'at-faithful and beyond mortal or institutional judgment.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_house, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaonic_house, beneficiary).

% Administers state temple cults, royal rituals, and theological education that naturalize Pharaoh's exclusive Ma'at conduit status. Receives land grants, tax exemptions, and social authority in exchange for suppressing alternative cosmological readings and legitimizing extraction.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, temple_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, temple_establishment, agenda_setter).

% Records and transmits the official theological framework in administrative and monumental texts. Benefits from state employment, literacy monopoly, and social status by producing narratives that define royal action as identical with Ma'at and by excluding counter-narratives from the archive.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Provide agricultural surplus and corvÃ©e labor under the theological frame that this sustains cosmic order. Have no institutional channel to challenge extraction because the ruler's will is definitionally identical with Ma'at, and geographic and economic immobility prevent exit.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, peasant_cultivators, payer,
    powerless, biographical, trapped, local).

% Hold readings of Ma'at that impose obligations on Pharaoh or distribute cosmic responsibility across society. Are structurally excluded from temple discourse, royal patronage, and scribal curriculum; their theological alternatives are not copied, taught, or ritually enacted.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, alternative_theologians, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_house).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes cosmological and political legitimacy in the Pharaoh, preventing fragmentation of authority by asserting a single divine conduit for cosmic order.
% TRANSFER_FUNCTION: Moves agricultural surplus, corvÃ©e labor, and absolute political obedience from peasant cultivators to the pharaonic house and temple establishment under the justification of sustaining Ma'at.
% ABSENT_VOICES: Alternative theologians who read Ma'at as reciprocity or distributed responsibility are excluded from temple discourse, scribal curriculum, and royal patronage, removing any institutional voice that would impose obligations on the ruler.
% DISAPPEARANCE_RATIONALE: If the divine mandate reading disappeared, the theological basis for unconditional royal extraction would collapse, the temple-scribal apparatus would lose its central legitimizing narrative, and peasant obligations would require renegotiation under alternative cosmological frames.
% FOUNDING_PROBLEM: Political and cosmological fragmentation in the Nile Valley prior to unification; need for a centralized theological-political framework to coordinate irrigation, labor, and cult.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological and Egyptological scholarship outside the beneficiary set attests that the unification problem was solved by the Early Dynastic Period, while the divine mandate framework persisted and intensified; dissenting theological voices were structurally excluded from the record.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because the reading licenses total appropriation of surplus, labor, and political submission under a non-negotiable cosmic frame. Suppression is even higher (0.9) because alternative readings are structurally excluded from temple discourse, scribal transmission, and royal patronage. Theater ratio is substantial (0.65) because the daily performance of divine kingship (temple rituals, monumental titulary, coronation drama) constitutes a growing share of the constraint's actual operation, especially when empirical events such as military defeat, drought, or dynastic collapse contradict the claim that Pharaoh automatically sustains Ma'at. Accessibility collapse is high (0.85): once the reading is accepted, no internal theological check on royal action remains. Resistance is moderate (0.3) because while peasant flight, tomb robbery, and regionalism occurred, open ideological challenge was structurally impossible. The measurement series shows a shared time grid with a notable dip in suppression requirement around the First Intermediate Period (time point 10), when central authority fragmentation temporarily weakened the enforcement apparatus, followed by restoration and intensification.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic house and temple establishment compute this as necessary cosmic coordination; peasant cultivators and alternative theologians compute it as unilateral extraction. The engine derives this divergence from the same structural data: beneficiaries with universal scope and arbitrage exit versus trapped payers with local scope. The scribal bureaucracy sits near the beneficiary end but is identity-locked to the state framework, producing a middle d value.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic house and temple establishment are structural beneficiaries (d near 0.0), receiving surplus and authority while controlling the rules. Peasant cultivators are structural targets (d near 1.0), paying labor and crops with trapped exit. Alternative theologians are excluded rather than coordinated; their structural relationship is defined by absence. The scribal bureaucracy occupies an intermediate position: they benefit materially but their exit is constrained by professional identity fusion.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by exposing that the coordination function (maintenance of cosmic and social order) is structurally fused with an extraction mechanism (unilateral surplus transfer to the pharaonic house) that exempts the agenda setter from the constraint. A pure coordination reading would require that Ma'at could constrain royal action, which this reading explicitly denies by definitional fiat. The absence of a sunset clause and the presence of active theological enforcement against alternative readings distinguish it from a scaffold. Because the founding problem (political fragmentation) was solved millennia before the constraint's late-period operation, the R5 genealogy flags potential mandatrophy; the high theater ratio and continued extraction confirm that the constraint persists beyond its functional origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_subject_status,
    'Is the Pharaoh genuinely outside the Ma''at constraint system, or does the divine mandate reading merely obscure reciprocal obligations that existed in practice?',
    'Archaeological evidence of royal accountability rituals (e.g., Sed festival as renewal of mandate) versus theological claims of automatic embodiment.',
    'If reciprocal obligations existed in practice, the divine mandate reading is ideological cover for a tangled rope that is more symmetric than it appears; if truly absent, extraction is total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_subject_status, empirical, 'Whether Pharaoh was practically constrained by Ma''at.').

omega_variable(
    theological_sincerity,
    'Did the pharaonic house and temple establishment sincerely believe the divine mandate reading, or was it a strategic extraction narrative?',
    'Comparative analysis of royal behavior during crises (e.g., Pepi II''s inscriptions versus First Intermediate Period self-justification texts).',
    'High theater ratio and sustained extraction despite empirical contradiction suggest performative maintenance; sincere belief would lower theater ratio and potentially reclassify the constraint''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_sincerity, conceptual, 'Sincerity of belief versus strategic use of divine mandate.').

omega_variable(
    suppression_mechanism,
    'Was suppression of alternative readings achieved through physical coercion, institutional exclusion, or identity fusion of subjects?',
    'Analysis of scribal curriculum, temple archive exclusion patterns, and peasant self-identification in administrative texts.',
    'Identity-locked suppression would amplify effective extraction beyond the structural measure; purely physical suppression would leave room for resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Mechanism of alternative-reading suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_dm_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(maat_dm_tr_t5, maat_order_principle__divine_mandate_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(maat_dm_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(maat_dm_tr_t15, maat_order_principle__divine_mandate_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(maat_dm_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(maat_dm_tr_t25, maat_order_principle__divine_mandate_reading, theater_ratio, 25, 0.7).
narrative_ontology:measurement(maat_dm_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(maat_dm_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(maat_dm_be_t5, maat_order_principle__divine_mandate_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(maat_dm_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(maat_dm_be_t15, maat_order_principle__divine_mandate_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(maat_dm_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(maat_dm_be_t25, maat_order_principle__divine_mandate_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(maat_dm_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(maat_dm_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(maat_dm_su_t5, maat_order_principle__divine_mandate_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(maat_dm_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(maat_dm_su_t15, maat_order_principle__divine_mandate_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(maat_dm_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(maat_dm_su_t25, maat_order_principle__divine_mandate_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement(maat_dm_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the maat_order_principle kernel, decomposed per the Îµ-invariance principle because different readings produce structurally distinct epsilon values and victim/beneficiary profiles. The divine mandate reading centralizes extraction through exclusive royal conduit status; the reciprocity reading imposes bilateral obligations; the distributed reading lateralizes responsibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
