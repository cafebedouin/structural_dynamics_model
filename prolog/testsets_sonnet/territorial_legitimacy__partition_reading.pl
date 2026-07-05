% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via UN Partition and International Recognition (1948/1967 lines)
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   This story instantiates the partition/recognition reading of territorial
 *   legitimacy in Israel/Palestine: legitimacy flows from UN Resolution 181's
 *   partition logic, subsequent state practice, and the 1967 lines as the
 *   operative territorial baseline. On this reading, both an Israeli state
 *   within recognized borders and a prospective Palestinian state are
 *   legitimate; settlement activity beyond the 1967 lines is not. This is a
 *   distinct constraint from the security-necessity reading (which grounds
 *   legitimacy in defensible strategic depth past 1967) and the
 *   indigenous-continuity reading (which treats 1948 as Nakba and grounds
 *   legitimacy in continuous habitation and anti-colonial self-determination
 *   rather than partition). Each reading has its own beneficiary/victim
 *   structure and its own epsilon; they are linked, not merged, per the
 *   kernel-reading protocol.
 *
 * KEY AGENTS:
 *   - israeli_state_within_1948_lines: primary beneficiary and co-agenda-setter — institutional/arbitrage
 *   - prospective_palestinian_state_administration: beneficiary with unrealized enforcement capacity — moderate/constrained
 *   - international_legal_order_institutions: agenda_setter administering the framework — institutional/analytical
 *   - settler_population_beyond_1967_lines: payer under this reading's verdict — organized/constrained
 *   - palestinian_residents_of_annexed_territory: payer bearing the enforcement gap — powerless/trapped
 *   - displaced_1948_refugee_descendants: excluded from the framework's remit — powerless/trapped
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.42).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.55).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via UN Partition and International Recognition (1948/1967 lines)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '980d1847-aaa4-40e0-8474-1d1a6be18ec5').
narrative_ontology:cs_kernel_codification('980d1847-aaa4-40e0-8474-1d1a6be18ec5', formalized).
narrative_ontology:cs_authority_grounding('980d1847-aaa4-40e0-8474-1d1a6be18ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('980d1847-aaa4-40e0-8474-1d1a6be18ec5').
narrative_ontology:cs_reading_relation('980d1847-aaa4-40e0-8474-1d1a6be18ec5', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('980d1847-aaa4-40e0-8474-1d1a6be18ec5', territorial_legitimacy__indigenous_continuity_reading, influences).
narrative_ontology:cs_axiom('980d1847-aaa4-40e0-8474-1d1a6be18ec5', foundational, un_resolution_181_constitutes_valid_partition_authority).
narrative_ontology:cs_axiom_status(un_resolution_181_constitutes_valid_partition_authority, holdable).
narrative_ontology:cs_axiom_grounding('980d1847-aaa4-40e0-8474-1d1a6be18ec5', un_resolution_181_constitutes_valid_partition_authority, conventional).
narrative_ontology:cs_axiom('980d1847-aaa4-40e0-8474-1d1a6be18ec5', foundational, territorial_legitimacy_tracks_1967_armistice_lines).
narrative_ontology:cs_axiom_status(territorial_legitimacy_tracks_1967_armistice_lines, holdable).
narrative_ontology:cs_axiom_grounding('980d1847-aaa4-40e0-8474-1d1a6be18ec5', territorial_legitimacy_tracks_1967_armistice_lines, conventional).
narrative_ontology:cs_axiom('980d1847-aaa4-40e0-8474-1d1a6be18ec5', secondary, settlement_beyond_recognized_lines_is_per_se_illegitimate).
narrative_ontology:cs_axiom_status(settlement_beyond_recognized_lines_is_per_se_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('980d1847-aaa4-40e0-8474-1d1a6be18ec5', settlement_beyond_recognized_lines_is_per_se_illegitimate, conventional).
narrative_ontology:cs_reference_frame('980d1847-aaa4-40e0-8474-1d1a6be18ec5', un_partition_resolution_181_baseline).
narrative_ontology:cs_drift_state('980d1847-aaa4-40e0-8474-1d1a6be18ec5', post_oslo_annexation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('980d1847-aaa4-40e0-8474-1d1a6be18ec5', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_within_1948_lines).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, prospective_palestinian_state_administration).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settler_population_beyond_1967_lines).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_residents_of_annexed_territory).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, displaced_1948_refugee_descendants).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_general_assembly_partition_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, two_state_framework_viability).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, uti_possidetis_juris_applicability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds UN membership and near-universal recognition grounded in the partition resolution and subsequent state practice. Benefits from the legal architecture that legitimates its existence within recognized borders, and participates in shaping how that legitimacy is invoked or contested in diplomatic and legal fora.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_within_1948_lines, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_within_1948_lines, agenda_setter).

% Derives its clearest international legal claim to statehood from the same partition/recognition framework, with growing bilateral and UN recognitions citing 1967 lines as the basis for a prospective state. Lacks full territorial control and enforcement capacity, so the legal claim exceeds the administrative reality on the ground.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, prospective_palestinian_state_administration, beneficiary,
    moderate, generational, constrained, national).

% UN bodies, the ICJ, and treaty-monitoring institutions administer and periodically restate the partition/recognition framework — resolutions, advisory opinions, and diplomatic recognition practice. They gain systemic legitimacy from having a working precedent for partition-based state formation, but bear none of the territorial costs directly.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order_institutions, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, international_legal_order_institutions, observer).

% Lives in communities whose presence is treated as illegitimate under this reading regardless of domestic legal status, since they sit beyond the recognized 1967 lines. Faces the prospect that any resolution honoring the partition/recognition framework requires their relocation, annexation-reversal, or renegotiated status; heavily organized politically but constrained by the framework's verdict on the land itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settler_population_beyond_1967_lines, payer,
    organized, biographical, constrained, regional).

% Live under administrative arrangements in areas beyond the Green Line where legal status, movement, and property rights are constrained by military and civil administration structures. This reading declares the territory they inhabit outside recognized Israeli sovereignty, which affirms their claim on paper but does not by itself alter facts on the ground; they carry the daily cost of the unresolved gap between legal verdict and enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_residents_of_annexed_territory, payer,
    powerless, biographical, trapped, local).

% Descendants of those displaced in 1948 and after, dispersed across neighboring states and camps. The partition/recognition framework as operationalized in this reading addresses statehood and borders, not the return or compensation claims tied to 1948 displacement; their central grievance sits largely outside what this reading's legal apparatus is built to resolve.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, displaced_1948_refugee_descendants, excluded,
    powerless, generational, trapped, regional).

% Governments extending or withholding diplomatic recognition of one or both prospective states, calibrating their positions partly by reference to the 1948 partition resolution and the 1967 lines. They shape the framework's practical force through recognition and aid policy without bearing the territorial consequences themselves.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, third_party_recognizing_states, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, internationally legible baseline — a UN resolution plus subsequent state practice and armistice lines — against which competing sovereignty claims over the same territory can be adjudicated, recognized, and negotiated, rather than settled purely by force or unilateral assertion.
% TRANSFER_FUNCTION: Confers international legal standing and access to diplomatic, economic, and institutional benefits (UN membership, treaty capacity, aid eligibility) on entities that can plausibly claim conformity with the partition/1967-lines framework, while withholding equivalent standing from settlement activity and unresolved refugee claims that fall outside it.
% ABSENT_VOICES: Displaced 1948 refugee descendants have no seat in the recognition apparatus this reading operates through — their claims sound in a different register (return, restitution) that the partition/statehood framework was not built to process. Settler communities beyond 1967 lines are organized and vocal but structurally excluded from the framework's legitimating logic by its own terms.
% DISAPPEARANCE_RATIONALE: If international legal recognition tied to the 1948 partition and 1967 lines vanished as an organizing reference, diplomatic recognition, aid conditionality, UN voting blocs, peace-process architecture, and the legal basis for characterizing settlements as unlawful would all lose their current anchor — negotiations and recognition practice would have to reconstruct a baseline from scratch or default to pure territorial control.
% FOUNDING_PROBLEM: In 1947-48, competing Jewish and Arab claims to Mandatory Palestine needed an internationally sanctioned mechanism to allocate sovereignty and end colonial administration without simply ratifying whichever side achieved battlefield control.
% FOUNDING_PROBLEM_CORROBORATION: UN member states outside the immediate parties (via repeated General Assembly and Security Council resolutions, and ICJ advisory opinions) continue to invoke the partition/1967-lines framework as live and operative. Israeli governmental positions and settler-movement advocacy dispute that the 1947 partition or 1967 lines retain binding force absent negotiated agreement; Palestinian refugee advocacy organizations, corroborating from outside the framework's beneficiary institutions, argue the founding problem was never solved for displaced populations and that the framework structurally cannot address it.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).
:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the coordination function is genuine — an internationally legible baseline for adjudicating competing claims exists and is not merely cover — but enforcement gaps and the settlement/annexation dynamic create real asymmetric costs, concentrated on settler communities (whose presence the framework delegitimizes) and on Palestinian residents in annexed areas (who bear the daily cost of the framework's non-enforcement). Suppression (0.55) reflects the active diplomatic, legal, and at times coercive apparatus required to maintain the 1967-line baseline against unilateral annexation claims and against refugee-return claims alike. Theater ratio rose over the interval (0.15 to 0.38) as repeated resolutions and recognitions increasingly substitute for enforcement capacity — resolutions accumulate without corresponding change on the ground, a Goodhart-style drift from function toward performative reaffirmation.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of international legal institutions, this framework is functioning coordination: a stable reference point that prevents purely force-based resolution. From the seat of settler communities or annexed-territory residents, the same framework operates as an externally imposed verdict on their location and status that they had no voice in setting. The engine computes these divergent seat-level readings from the structural power/exit data; the claimed type (tangled_rope) already reflects that both a genuine coordination function and asymmetric extraction coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli statehood within 1948 lines and prospective Palestinian statehood both derive legitimating force from this framework — both sit near the beneficiary end, though the Palestinian entity's benefit is largely juridical rather than administratively realized (arbitrage vs. constrained exit distinguishes them). International institutions administer the framework and gain systemic legitimacy from a working partition precedent without bearing territorial costs. Settlers beyond 1967 lines and Palestinian residents of annexed territory are structural targets: the framework's verdict falls on their status and location, and they cannot exit that verdict by relocating politically. Refugee descendants are neither clean beneficiaries nor clean victims of this specific reading — they are outside its remit entirely, which is itself a directional fact worth flagging via the excluded role rather than forcing them into beneficiary or victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating sovereignty over contested Mandatory Palestine without ratifying pure battlefield control — remains partially live (no negotiated two-state resolution has been reached) but the framework's operative content has drifted from active adjudication toward repeated symbolic reaffirmation (rising theater_ratio) without corresponding enforcement (rising suppression_requirement alongside stagnant on-the-ground change). This is not yet mandatrophy-resolved because the underlying coordination need persists, but the widening gap between legal verdict and enforced reality is the signal the classification is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_baseline_naturalness_vs_construction,
    'Is the 1948 partition / 1967-line framework a genuinely stabilizing legal baseline that most parties would independently converge on, or a constructed reference point whose persistence depends on the ongoing interests of institutions and states invested in a working partition precedent?',
    'Comparative analysis of other partition-based state formations (Cyprus, India-Pakistan, Korea) to assess whether international law treats partition lines as durable defaults absent enforcement interest, versus counterfactual analysis of whether the 1967-line baseline would persist if major UN member states withdrew diplomatic investment in it.',
    'If the baseline is substantially constructed and institutionally maintained rather than naturally converged-upon, the coordination-function claim weakens relative to the extraction reading and the classification should weight toward snare/tangled_rope more heavily than toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_baseline_naturalness_vs_construction, conceptual, 'Whether the partition/1967-line baseline is a natural convergence point or an institutionally sustained construction.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does this partition reading''s acceptance of 1948 as a legitimate legal founding act structurally foreclose the indigenous_continuity_reading''s characterization of 1948 as Nakba/dispossession within a single legal framework, or can both readings coexist as competing but non-exclusive claims held by different parties?',
    'Doctrinal analysis of whether any international legal body has attempted to hold both characterizations simultaneously (e.g., in ICJ advisory opinions that reference both partition legality and continuing refugee dispossession) versus treating them as mutually exclusive premises.',
    'If the two readings genuinely foreclose each other, the kernel''s cs_structure reading_relations should record forecloses rather than coexists_with, which would materially change how cross-reading contamination propagates in network analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the partition reading and the indigenous-continuity reading are logically exclusive or merely rival within ongoing dispute.').

omega_variable(
    refugee_claim_remit_ambiguity,
    'Is the exclusion of 1948 refugee descendants from this framework''s remit a structural feature of the partition/recognition legal architecture (i.e., statehood and refugee-return are genuinely separable legal questions), or an artifact of which claims happened to get institutionalized in UN machinery (UNRWA vs. state-recognition bodies)?',
    'Historical and legal analysis of UN General Assembly Resolution 194 (right of return) and its institutional separation from Resolution 181, tracing whether the separation was a deliberate design choice or an administrative accident of which bodies were created when.',
    'If the separation is an administrative artifact rather than a principled legal boundary, the exclusion of refugee descendants may itself be an extraction mechanism (a claim structurally routed out of the venue with enforcement power) rather than a neutral scope limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_claim_remit_ambiguity, empirical, 'Whether refugee-return claims are legitimately outside partition-based statehood law or administratively excluded from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__partition_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__partition_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__partition_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__partition_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.33).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__partition_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__partition_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__partition_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.42).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__partition_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__partition_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy kernel. security_necessity_reading grounds legitimacy in defensive strategic depth past 1967 lines and directly contests this reading's territorial baseline (coexists_with: both are live positions held by different state/political actors, neither logically forecloses the other within international discourse broadly, though they cannot both be simultaneously adopted as a single state's operative policy). indigenous_continuity_reading grounds legitimacy in continuous habitation and anti-colonial self-determination, treating 1948 as dispossession rather than legitimate partition; this reading's acceptance of 1948 as a founding legal act creates downstream pressure on (influences) the indigenous_continuity_reading's legitimacy conditions without fully foreclosing it, since the indigenous reading can be held as a moral/historical claim independent of this reading's legal-formalist framework. Each reading carries its own epsilon, beneficiary/victim structure, and classification; the network edges route contamination analysis between them without merging their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
