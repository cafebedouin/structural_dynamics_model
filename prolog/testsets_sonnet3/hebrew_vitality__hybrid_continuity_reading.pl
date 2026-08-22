% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality Kernel — Hybrid Continuity Reading (Substrate + Reconstruction)
 *   domain: sociolinguistics/jewish_studies/language_revitalization
 *
 * SUMMARY:
 *   This story instantiates the hybrid_continuity_reading of the
 *   hebrew_vitality kernel: the claim that centuries of liturgical Hebrew
 *   preservation supplied a necessary substrate (lexicon, orthography,
 *   continuous textual engagement) but that this substrate was insufficient
 *   by itself to produce vitality — vernacular revival additionally required
 *   deliberate reconstruction (grammatical expansion, register creation, and
 *   the specific mechanism of raising children as native speakers, most
 *   visibly through Ben-Yehuda's household and the Second Aliyah
 *   settlements). This reading treats the liturgical_reading (ritual use as
 *   vitality) and the native_daily_reading (only native generation as
 *   vitality) as each capturing one true half of a two-factor causal
 *   structure, and it attempts to resolve their contest by reframing
 *   'vitality' as jointly produced rather than adjudicating which single
 *   factor deserves the title.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality Kernel — Hybrid Continuity Reading (Substrate + Reconstruction)").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/jewish_studies/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '2e406f1d-3b4d-4ab8-a095-a3ba0e28a727').
narrative_ontology:cs_kernel_codification('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', distributed).
narrative_ontology:cs_authority_grounding('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', distributed).
narrative_ontology:cs_reading_relation('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', foundational, joint_necessity_dual_causation).
narrative_ontology:cs_axiom_status(joint_necessity_dual_causation, holdable).
narrative_ontology:cs_axiom_grounding('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', joint_necessity_dual_causation, empirically_contingent).
narrative_ontology:cs_axiom('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', foundational, substrate_alone_insufficient_for_vitality).
narrative_ontology:cs_axiom_status(substrate_alone_insufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', substrate_alone_insufficient_for_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', pre_revival_diaspora_liturgical_continuity).
narrative_ontology:cs_drift_state('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', contemporary_comparative_revitalization_scholarship, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2e406f1d-3b4d-4ab8-a095-a3ba0e28a727', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, revival_era_linguists).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speaking_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, liturgical_hebrew_tradition_bearers).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, necessary_but_insufficient_causation_thesis).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, substrate_plus_reconstruction_dual_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ben-Yehuda and contemporaries treated liturgical Hebrew as raw material — a lexical and morphological substrate — and deliberately engineered vocabulary, register, and child-acquisition pathways on top of it. They set the terms of what counted as 'reviving' the language, distinguishing preservation work from generative reconstruction work.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, revival_era_linguists, agenda_setter,
    moderate, generational, analytical, regional).

% Native and near-native speakers today inherit a language that could not have existed without either component: the liturgical corpus supplied lexicon, script, and prestige continuity; the deliberate reconstruction supplied the grammar, register expansion, and child-transmission mechanism that produced native speakers. They benefit from the finished synthesis without needing to litigate which ingredient mattered more.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speaking_community, beneficiary,
    organized, civilizational, arbitrage, national).

% Communities that maintained Hebrew in prayer and study across the diaspora supplied the substrate this reading depends on, but their own framework (liturgical use as vitality) is treated by this reading as necessary-but-insufficient — a partial demotion of their contribution's sufficiency claim that they would contest if consulted directly.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_hebrew_tradition_bearers, excluded,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, liturgical_hebrew_tradition_bearers, beneficiary).

% Scholars and advocates who hold that only native daily generation constitutes vitality would reject this reading's attempt to credit liturgical preservation with any constitutive role — for them the reconstruction period is the whole story and the liturgical centuries are prehistory, not partial vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_generation_purists, excluded,
    moderate, biographical, analytical, national).

% Researchers comparing Hebrew revival to other revitalization attempts (Cornish, Hawaiian, Wampanoag) use this hybrid framing as an analytical tool to explain why some revivals succeed and others stall — testing whether substrate availability without reconstruction effort, or reconstruction effort without substrate, reliably fails to produce native vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, sociolinguistic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing accounts of what caused Hebrew's revival by decomposing 'vitality' into two jointly necessary components — an inherited substrate (lexicon, script, liturgical continuity) and a deliberate reconstruction effort (grammar expansion, register creation, child-transmission engineering) — so that comparative revitalization research and historical causal attribution are not forced into a single-cause framing.
% TRANSFER_FUNCTION: Moves interpretive credit rather than resources: reallocates causal weight away from pure-preservation narratives and pure-reconstruction narratives toward a joint-necessity account. No material transfer occurs; what moves is scholarly and communal attribution of which historical actors' contributions were constitutive versus merely enabling.
% ABSENT_VOICES: Liturgical tradition-bearing communities are not consulted as co-authors of this synthesis; their own self-understanding (that unbroken ritual use is itself vitality, not mere enabling substrate) is subordinated to a two-factor model built largely by linguists analyzing the revival after the fact. Native-generation purists who deny liturgical preservation any constitutive status are also outside this reading's frame, which credits both factors by design.
% DISAPPEARANCE_RATIONALE: This is an analytical/interpretive reading, not an operative institutional arrangement. If the hybrid-continuity framing disappeared from scholarly discourse tomorrow, Modern Hebrew would keep being spoken exactly as it is; only the causal-historical explanation offered for its existence would revert to being contested between the liturgical and native-daily readings. No stakeholder's material position depends on this reading persisting.
% FOUNDING_PROBLEM: Historians and linguists needed to explain why Hebrew revival succeeded where dozens of other liturgical-only languages (e.g., Coptic, Ge'ez in some registers) never produced native speakers, and why reconstruction efforts without deep textual substrate (some constructed-language projects) also fail to achieve intergenerational vitality — a single-factor account could not explain the asymmetry.
% FOUNDING_PROBLEM_CORROBORATION: Comparative revitalization researchers working on unrelated cases (Hawaiian, Māori, Wampanoag reclamation projects) independently converge on substrate-plus-deliberate-reconstruction as the operative pattern, without reference to Hebrew specifically — this is corroboration from outside both the liturgical and native-daily interest groups, since neither camp within Hebrew-revival historiography benefits from the joint-necessity account winning.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and theater ratio are both authored low and essentially flat across the interval because this is an analytical/historiographic reading with no operative extraction mechanism and no institution that must be actively defended — its 'persistence' is a matter of scholarly plausibility, not enforcement. Suppression is low: no party is coerced into accepting the hybrid framing, and it faces live, articulate rivals in both other readings. Resistance is moderate (0.35) because both sibling readings actively contest this synthesis rather than deferring to it — liturgical partisans resist being cast as merely 'enabling,' native-daily partisans resist being told preservation was constitutive at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Because this reading is interpretive rather than extractive, beneficiary status here means 'whose historical contribution is credited as causally necessary,' not 'who collects rent.' Revival-era linguists and the modern speaking community are named beneficiaries because the hybrid account is the one that credits both their inherited substrate and their deliberate labor; no victim group exists because no one bears a material cost from this reading's adoption or rejection. Liturgical tradition-bearers and native-generation purists are marked excluded rather than payer/victim, because what they lose is interpretive primacy, not resources.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable in the classic extraction-outlives-its-function sense: this reading's 'founding problem' (explaining revival's causal structure) remains live because comparative revitalization science still needs it. There is no mandatrophy risk because there is no mandate — only an explanatory claim competing with two others for descriptive adequacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_sufficiency_boundary,
    'Is there a principled, non-arbitrary line between ''substrate that liturgical preservation supplied'' and ''reconstruction that made it vital,'' or is this a post-hoc analytical partition imposed on a continuous historical process?',
    'Counterfactual comparative analysis: examine revitalization attempts with substrate but no organized reconstruction effort (e.g. some Coptic liturgical communities) against attempts with reconstruction effort but thin substrate (some constructed/planned languages) to see whether the two-factor model predicts outcomes better than single-factor models.',
    'If the line cannot be drawn non-arbitrarily, the hybrid reading collapses into either the liturgical or native_daily reading depending on how the analyst weights the ambiguous middle cases, undermining its claim to be a genuine third reading rather than a rhetorical compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_sufficiency_boundary, conceptual, 'Whether the substrate/reconstruction partition is principled or an artifact of the reframing move itself.').

omega_variable(
    kernel_committer_reframing_status,
    'Does this hybrid reading genuinely resolve the contest between liturgical_reading and native_daily_reading, or does it merely relabel the same disagreement as a matter of degree rather than kind?',
    'Examine whether liturgical_reading and native_daily_reading proponents, on encountering the hybrid formulation, treat it as a satisfactory synthesis (converging behavior) or continue to assert their original single-factor claims unchanged (non-convergence) — this is the sibling-reading contest the committer frame requires be routed here rather than folded into base metrics.',
    'If proponents treat this as satisfactory synthesis, the hybrid reading functions as a genuine third position with its own constituency. If proponents on both sides continue asserting their original claims, the hybrid reading is better understood as an analytical observer''s frame layered atop an unresolved dispute rather than a competing claim held by any party.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_reframing_status, conceptual, 'Whether the hybrid reading is a held position or an observer''s synthesis of two held positions.').

omega_variable(
    beneficiary_attribution_naturalness,
    'Is naming revival-era linguists and the modern speaking community as ''beneficiaries'' of this reading a meaningful structural claim, or a category mismatch imported from the extraction-oriented base_properties schema onto a purely interpretive constraint?',
    'Compare against other analytical/historiographic constraint stories in the corpus to see whether the beneficiary field, when authored for low-ε interpretive readings, produces engine classifications that track anything beyond ''this reading credits these parties.''',
    'If beneficiary attribution in interpretive contexts does not correspond to any real directional flow, the engine''s directionality computation for this story should be read as a formal artifact rather than a substantive claim about who gains from the reading''s adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_attribution_naturalness, conceptual, 'Whether beneficiary declarations are meaningful for a non-extractive, purely interpretive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1920, 0.06).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1948, 0.07).
narrative_ontology:measurement(hebr_tr_t1975, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1880, 0.02).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1920, 0.04).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1948, 0.05).
narrative_ontology:measurement(hebr_be_t1975, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1975, 0.06).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement(hebr_be_t2025, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2025, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, native_daily_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language label 'Hebrew vitality debate': liturgical_reading (ritual preservation constitutes vitality), native_daily_reading (only native generation constitutes vitality), and this hybrid_continuity_reading (both are jointly necessary, neither sufficient alone). Each reading is authored as a separate, ε-invariant constraint with its own stakeholder set. This story's ε is deliberately low (0.08) relative to what either sibling might author for a claim they consider contested and stakes-bearing, because this reading's own structure is interpretive synthesis rather than an actionable arrangement with parties who gain or lose materially from its truth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
