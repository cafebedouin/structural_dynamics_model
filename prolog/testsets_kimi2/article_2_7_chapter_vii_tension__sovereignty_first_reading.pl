% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: UN Charter Sovereignty-First Non-Intervention Norm
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-first reading of the UN
 *   Charter Article 2(7) and Chapter VII kernel. Under this reading, state
 *   sovereignty is foundational and non-intervention is the default norm;
 *   military or coercive intervention is permitted only with explicit state
 *   consent or UN Security Council authorization under Chapter VII,
 *   traditionally interpreted as limited to inter-state aggression and
 *   cross-border threats. Post-colonial and authoritarian states benefit from
 *   the legal shield against external interference, while populations
 *   suffering domestic atrocity bear the costs when their governments refuse
 *   consent and the Council is paralyzed by veto. The arrangement coordinates
 *   a stable inter-state order but extracts from domestic populations by
 *   blocking humanitarian protection pathways. This is authored as a kernel
 *   reading: the sibling R2P reading would dissolve the domestic-atrocity
 *   victim set by conditioning sovereignty on population protection.
 *
 * KEY AGENTS:
 *   - post_colonial_state_governments: Primary beneficiary (institutional/constrained) â uses sovereignty to resist neo-colonial pressure and preserve policy autonomy.
 *   - authoritarian_regimes: Primary beneficiary (institutional/constrained) â invokes sovereignty to shield internal repression from international scrutiny.
 *   - populations_under_domestic_atrocity: Primary target (powerless/trapped) â denied external protection by the sovereignty-veto structure.
 *   - unsc_permanent_five: Agenda-setter (institutional/arbitrage) â controls Chapter VII authorization and selectively enforces the sovereignty boundary.
 *   - human_rights_advocates: Excluded observer (organized/mobile) â documents atrocities but lacks formal seat in the enforcement architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.68).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "UN Charter Sovereignty-First Non-Intervention Norm").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '31209759-fcbd-486b-9001-b77742a263e9').
narrative_ontology:cs_kernel_codification('31209759-fcbd-486b-9001-b77742a263e9', formalized).
narrative_ontology:cs_authority_grounding('31209759-fcbd-486b-9001-b77742a263e9', lineage).
narrative_ontology:cs_interpretation_layer_present('31209759-fcbd-486b-9001-b77742a263e9').
narrative_ontology:cs_reading_relation('31209759-fcbd-486b-9001-b77742a263e9', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('31209759-fcbd-486b-9001-b77742a263e9', foundational, unconditional_domestic_jurisdiction).
narrative_ontology:cs_axiom_status(unconditional_domestic_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('31209759-fcbd-486b-9001-b77742a263e9', unconditional_domestic_jurisdiction, conventional).
narrative_ontology:cs_axiom('31209759-fcbd-486b-9001-b77742a263e9', foundational, chapter_vii_inter_state_limitation).
narrative_ontology:cs_axiom_status(chapter_vii_inter_state_limitation, holdable).
narrative_ontology:cs_axiom_grounding('31209759-fcbd-486b-9001-b77742a263e9', chapter_vii_inter_state_limitation, conventional).
narrative_ontology:cs_reference_frame('31209759-fcbd-486b-9001-b77742a263e9', un_charter_sovereignty_default).
narrative_ontology:cs_drift_state('31209759-fcbd-486b-9001-b77742a263e9', post_cold_war_atrocity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('31209759-fcbd-486b-9001-b77742a263e9', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 2(7) and sovereign equality to resist external pressure, condition development aid on non-interference, and retain territorial integrity guarantees secured at decolonization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments, beneficiary,
    institutional, generational, constrained, global).

% Use sovereignty norms to reject UN human rights monitoring, block ICC referrals, and criminalize foreign funding of civil society while retaining UN membership benefits.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, global).

% Live under governments committing genocide, crimes against humanity, or war crimes against them; international military intervention is legally blocked by sovereignty claims and Council paralysis; appeals to the UN are routed back to the consent of the attacking state.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, national).

% Control the legal threshold for intervention through Article 39 determinations and veto power; administer the boundary between permissible Chapter VII action and prohibited intervention; their geopolitical interests heavily influence whether atrocity receives authorization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, unsc_permanent_five, agenda_setter,
    institutional, generational, arbitrage, global).

% Document atrocities and lobby for intervention in UN fora, but are structurally excluded from the veto-bearing Security Council chamber where the sovereignty-intervention boundary is enforced.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates, observer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a centralized, consent-based system for authorizing international coercion, replacing unilateral great-power intervention with collective Security Council decision-making and stabilizing the post-war and post-colonial state system.
% TRANSFER_FUNCTION: Moves impunity for domestic atrocities from the international community to sovereign state governments, and moves the risk of unaddressed mass violence onto domestic populations when their state refuses consent or the Council is paralyzed.
% ABSENT_VOICES: Populations under domestic atrocity are structurally excluded from the Security Council chamber where the sovereignty-intervention boundary is enforced; R2P-advocate states and human rights advocates are present in UN discourse but excluded from veto-bearing decision architecture.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first norm vanished overnight, territorial integrity guarantees would weaken, unilateral humanitarian intervention would proliferate without centralized checks, the post-colonial settlement would face destabilizing challenges, and the UN's monopoly on legitimate force would fracture.
% FOUNDING_PROBLEM: Prevent unilateral great-power predation, colonial interventionism, and interstate war by establishing sovereign equality and centralized use-of-force authorization after 1945 and decolonization.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states corroborate the founding problem as live, citing neo-colonial risks. Independent human rights organizations, academic security studies, and some Western governments attest the problem has shifted: interstate aggression is now less salient than intra-state atrocity, but the arrangement persists and now shields the latter. Corroboration from outside the beneficiary set supports the shifted-function reading.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the sovereignty-first frame systematically blocks coercive protection for populations under domestic atrocity, transferring impunity to their governments. Suppression is substantial (0.68) because the constraint persists through active UN Charter enforcement, diplomatic isolation of interveners, and selective Council paralysis. Theater is moderate (0.48): much sovereignty rhetoric remains functional (it genuinely prevents interstate predation), but a growing share is performative (veto justifications that mask geopolitical convenience, legal arguments that preserve impunity). The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (state governments) and the payer seat (atrocity populations) should compute to very different types: from the state perspective the arrangement is protective coordination against neo-colonialism; from the population perspective it is enforced abandonment. The P5 seat experiences the constraint as a flexible instrument of geopolitical management. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments (post-colonial and authoritarian) are structural beneficiaries of the sovereignty shield (low d, subsidized by the constraint). Populations under domestic atrocity are the structural targets (high d, amplified extraction through trapped exit). The P5 agenda-setters sit near the beneficiary end (d low) because the constraint subsidizes their geopolitical flexibility and monopoly on authorization. Human rights advocates sit near symmetric (d ~0.5) because they neither collect impunity nor pay atrocity costs directly, but are structurally excluded from altering the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing interstate aggression and colonial intervention) is contested: post-colonial states argue it remains live, while independent analysts argue it has shifted. The constraint persists despite substantial practice drift (Kosovo, Libya, Syria), suggesting the coordination function has partially atrophied into a shield for domestic atrocity. However, because the interstate coordination function remains genuine and historically significant, the classification is tangled_rope rather than snare â the coordination is not mere cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sovereignty_first,
    'This constraint instantiates the sovereignty-first reading of the article_2_7_chapter_vii_tension kernel. The sibling r2p_reading would reduce epsilon by treating sovereignty as conditional on population protection, eliminating the domestic-atrocity victim set. Which structural element differentiates the readings?',
    'Analysis of which reading''s axiom set (unconditional sovereignty vs. conditional sovereignty) is operatively dominant in recent UN Security Council practice.',
    'If the r2p_reading becomes dominant, this constraint''s epsilon and victim structure dissolve; if this reading remains dominant, the extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sovereignty_first, conceptual, 'Kernel reading identity and structural delta from sibling R2P reading.').

omega_variable(
    chapter_vii_intra_state_gap,
    'Does the Charter''s Chapter VII limitation to ''threats to the peace, breaches of the peace, and acts of aggression'' structurally exclude intra-state atrocity from coercive intervention absent cross-border effects?',
    'ICJ advisory opinions and Security Council practice on whether Article 39 encompasses mass atrocity without inter-state spillover.',
    'If intra-state atrocity is structurally excluded from Chapter VII authorization, the sovereignty-first reading''s extraction is encoded in the text; if not, the extraction is interpretive choice and may be mutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chapter_vii_intra_state_gap, conceptual, 'Whether the textual architecture encodes domestic atrocity exclusion.').

omega_variable(
    post_colonial_beneficiary_ambiguity,
    'Do post-colonial states benefit from sovereignty norms as genuine protection against neo-colonialism, or have authoritarian regimes captured the same norm for extraction?',
    'Distinguishing democratic post-colonial states'' use of sovereignty from authoritarian use; incidence of invoking sovereignty to block human rights scrutiny.',
    'If the beneficiary set is genuinely protective for decolonized peoples, the victim set is smaller than claimed; if authoritarian capture is dominant, extraction is concentrated on domestic populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_beneficiary_ambiguity, empirical, 'Whether sovereignty benefits are protective or captured by repressive regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(arti_tr_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 45, 0.48).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(arti_tr_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(arti_be_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(arti_be_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(arti_su_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(arti_su_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sovereignty-first reading of the article_2_7_chapter_vii_tension kernel. The sibling r2p_reading instantiates a structurally distinct claim with a different epsilon (lower extraction, different victim/beneficiary structure). They are not the same constraint viewed from two angles; they share a kernel but emit different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
