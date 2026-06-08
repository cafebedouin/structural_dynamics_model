% ============================================================================
% CONSTRAINT STORY: performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_only
 *   human_readable: Sacrifice Law Requires Material Instantiation (Performance-Only Reading)
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of sacrifice law holds that religious
 *   obligation under the Temple sacrificial system requires material
 *   instantiation — actual performance of the ritual acts. Study of sacrifice
 *   law in the post-Temple period (70 CE to present) is therefore archival
 *   preservation of a defunct practice, not active occupation of the
 *   religious commitment. This reading produces a dormant commitment system:
 *   the kernel (Temple sacrifice law) persists in textual form with near-zero
 *   current extractiveness because the performance requirement has not been
 *   met for nearly two millennia. The constraint coordinates textual
 *   preservation, scholarly training, and community identity maintenance
 *   without creating a victim class, because no resources are allocated, no
 *   hierarchy is enforced, and no agents are excluded from participation in
 *   the study itself. Extractiveness (0.12) reflects only the minimal
 *   institutional overhead of yeshiva systems and textual preservation
 *   infrastructure. The reading's ethical consequence: it forecloses
 *   restoration of the sacrificial cult without substantial modification,
 *   because resuming performance would reactivate the extractive mechanisms
 *   (resource flows, labor hierarchies, purity-based exclusions) that
 *   dormancy has suspended.
 *
 * KEY AGENTS:
 *   - Halakhic Study Community: Primary beneficiary (organized/mobile) — coordinates preservation of sacrifice law texts, maintains scholarly tradition, sustains communal identity through shared study practices
 *   - Text Preservation Institutions: Secondary beneficiary (institutional/mobile) — yeshivot, academic departments, publishing houses that preserve and transmit the corpus
 *   - Contemporary Jewish Layperson: Participant (powerless/constrained) — engages with sacrifice law study as cultural-historical knowledge; no extraction because participation is voluntary and no performance is required
 *   - Reform and Reconstructionist Movements: Organized agents (organized/mobile) — explicitly reject restoration theology; see study-without-performance as coordination for historical consciousness and ethical evolution away from animal sacrifice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only, 0.12).
domain_priors:suppression_score(performance_only, 0.2).
domain_priors:theater_ratio(performance_only, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only, extractiveness, 0.12).
narrative_ontology:constraint_metric(performance_only, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(performance_only, theater_ratio, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only, rope).
narrative_ontology:human_readable(performance_only, "Sacrifice Law Requires Material Instantiation (Performance-Only Reading)").
narrative_ontology:topic_domain(performance_only, "religious_law/halakhic_tradition/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only, 'eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f').
narrative_ontology:cs_kernel_codification('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', fixed_text).
narrative_ontology:cs_authority_grounding('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', lineage).
narrative_ontology:cs_interpretation_layer_present('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f').
narrative_ontology:cs_reading_relation('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', performance_only__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', performance_only__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', foundational, material_instantiation_required).
narrative_ontology:cs_axiom_status(material_instantiation_required, holdable).
narrative_ontology:cs_axiom_grounding('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', material_instantiation_required, empirically_contingent).
narrative_ontology:cs_axiom('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', foundational, study_is_archival_not_constitutive).
narrative_ontology:cs_axiom_status(study_is_archival_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', study_is_archival_not_constitutive, deontological).
narrative_ontology:cs_reference_frame('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', temple_era_performance_obligation).
narrative_ontology:cs_drift_state('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', contemporary_post_temple_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('eccd5eb5-b2fd-49a6-86ae-f53e6295dc1f', '').
narrative_ontology:cs_kernel_id(performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_only, halakhic_study_community).
narrative_ontology:constraint_beneficiary(performance_only, text_preservation_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_only, contemporary_jewish_layperson).
narrative_ontology:constraint_beneficiary(performance_only, reform_reconstructionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinates preservation of sacrifice law texts and transmission of halakhic knowledge across generations. Benefits from institutional legitimacy and communal resources (yeshiva funding, publication infrastructure) dedicated to textual study. Exit is possible — members can leave Orthodox Judaism or shift to non-halakhic Jewish denominations — but most remain by choice.
narrative_ontology:constraint_stakeholder(performance_only, halakhic_study_community, beneficiary,
    organized, generational, mobile, global).

% Yeshivot, academic departments, and publishing houses derive function and funding from preserving and transmitting the sacrifice law corpus. Low extraction: the coordination service (knowledge preservation) is genuine, and the institutional overhead is typical of educational systems.
narrative_ontology:constraint_stakeholder(performance_only, text_preservation_institutions, beneficiary,
    institutional, generational, mobile, global).

% Engages with sacrifice law as cultural-historical knowledge that sustains communal identity and continuity with tradition. No performance obligation, voluntary participation, no ritual hierarchy imposing exclusions. Exit is costly (social ties, community membership) but possible.
narrative_ontology:constraint_stakeholder(performance_only, contemporary_jewish_layperson, beneficiary,
    powerless, biographical, constrained, regional).

% Explicitly reject restoration theology and see the performance-only reading's dormancy logic as enabling ethical evolution away from animal sacrifice. Benefit from the constraint because it forecloses extractive reactivation of the sacrificial cult without requiring textual repudiation.
narrative_ontology:constraint_stakeholder(performance_only, reform_reconstructionist_movements, beneficiary,
    organized, generational, mobile, national).

% Non-agent entry for narrative completeness. In the counterfactual restoration scenario, animals would bear the direct material cost of resumed sacrifice. Currently excluded from the constraint because dormancy means no performance occurs.
narrative_ontology:constraint_stakeholder(performance_only, counterfactual_sacrificial_animals, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(performance_only, counterfactual_sacrificial_animals).

% In the counterfactual restoration scenario, lower-status workers (kohanim performing slaughter and altar service, those handling ritual impurity) would bear labor extraction and status hierarchy costs. Currently excluded because dormancy suspends the performance economy.
narrative_ontology:constraint_stakeholder(performance_only, counterfactual_ritual_labor_class, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual knowledge of Temple sacrifice law for potential future application; maintains scholarly tradition and communal identity through shared study practices; coordinates collective memory of pre-70 CE religious framework.
% TRANSFER_FUNCTION: Minimal material transfer in current state: study community members contribute time and tuition to yeshivot; yeshivot provide textual training and communal legitimacy. No sacrificial economy (no animal procurement, no ritual labor hierarchy, no resource flows to priestly class) because performance is suspended.
% ABSENT_VOICES: Counterfactual victims of restoration: animals who would be sacrificed, lower-status ritual workers who would bear labor costs, those excluded by purity-status requirements. Also absent: secular Israelis and Diaspora Jews who would object to restoration but are not currently in the halakhic conversation because dormancy makes the dispute theoretical rather than urgent.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared (i.e., if one of the sibling readings — study_as_exercise or hybrid_preparatory — became normative), the halakhic community would rearrange its understanding of sacrifice law's current status. Study would shift from archival preservation to active obligation-fulfillment, changing the extractiveness profile and creating different beneficiary/victim structures. The material world would not change immediately (no Temple yet exists), but the normative framework governing how the community relates to the potential for restoration would change, affecting resource allocation to Temple movement activism and discourse about restoration conditions.
% FOUNDING_PROBLEM: Post-70 CE halakhic crisis: Temple destruction eliminated the material basis for sacrifice, but the Torah's commandments regarding sacrifice remain textually binding. How does the community maintain continuity with the sacrificial tradition when performance is impossible? The performance-only reading resolves this by framing study as preservation rather than fulfillment, suspending the mandate until material restoration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live because the Temple has not been rebuilt and the material basis for sacrifice remains absent. Corroboration: the continued existence of yeshiva curricula dedicated to sacrifice law (Kodashim order of the Talmud), rabbinic literature addressing the post-Temple status of sacrificial obligations, and the real institutional infrastructure (yeshivot, text publication) dedicated to preserving this knowledge. Corroborating sources outside the direct beneficiary set: academic scholars of rabbinic Judaism (e.g., Neusner, Heschel) document the post-Temple transformation of sacrifice into study as a live halakhic development, not a resolved historical question.
narrative_ontology:disappearance_verdict(performance_only, world_rearranges).
narrative_ontology:founding_problem_status(performance_only, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTEMPORARY JEWISH LAYPERSON (ROPE) — Experiences the constraint as pure coordination: study preserves knowledge for potential future application, maintains cultural continuity, coordinates community identity around shared textual tradition. No extraction — the absence of performance means no resource allocation, no sacrificial economy, no victim class.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: YESHIVA SYSTEM (ROPE) — Institutional beneficiary: the constraint's performance-only framework transforms sacrifice law into pure study coordination. The yeshiva preserves the text, trains scholars, coordinates halakhic discourse — all without requiring material instantiation. Low extraction: study is voluntary, exit is possible, and the coordination function (textual preservation) is genuine.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: REFORM/RECONSTRUCTIONIST MOVEMENTS (ROPE) — Organized agents who explicitly reject restoration theology see the constraint as coordination mechanism for historical consciousness: study without performance preserves collective memory while foreclosing the extractive potential of a restored sacrificial cult. Negligible extraction — the constraint enables ethical evolution away from animal sacrifice.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the performance-only reading maintains a dormant commitment system: the kernel (sacrifice law) persists in textual form with no active enforcement mechanism. Study coordinates preservation and potential reactivation, but current extractiveness is minimal because the dormancy is near-total. The constraint becomes extractive only if restoration is attempted without ethical modification — a counterfactual not reflected in current ε.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_tests).
:- end_tests(performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint coordinates textual preservation and community identity with minimal resource extraction. The yeshiva system collects some institutional rents (tuition, donor support), but these are typical costs of educational coordination, not extraction specific to the sacrifice-law-study constraint. No victim class exists because: (1) study is voluntary, (2) no material performance is required, (3) no hierarchy is enforced through ritual purity status, (4) no animals or lower-status workers bear sacrificial labor. The extractiveness is near the Boltzmann floor for information_standard coordination. Suppression (0.20): Low. The constraint does not suppress alternatives — Reform and Reconstructionist Jews exit the performance framework entirely; secular Jews ignore it; Orthodox Jews who study sacrifice law do so voluntarily within a chosen tradition. The modest suppression reflects only the social cost of opting out within Orthodox communities (reputational penalty, reduced marriage-market access), not structural coercion. Theater ratio (0.08): Very low. Study is functionally preserving knowledge for potential future application (even if restoration is subjectively unlikely, the study is preparing for that contingency). The theater component reflects only the portion of study motivated by scholarly-tradition maintenance rather than genuine contingency preparation — a small fraction because most scholars do believe the knowledge could theoretically be applied if circumstances changed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as rope, though from different structural positions. The layperson sees coordination for cultural continuity. The yeshiva system sees coordination for knowledge preservation and scholarly training. Reform/Reconstructionist movements see coordination for historical memory that enables ethical evolution. The analytical observer sees coordination for maintaining a dormant commitment system with potential reactivation pathway. The gap that exists is subtle: the layperson and institutional perspectives see the study as intrinsically valuable (coordination for its own sake), while the analytical and Reform perspectives see the study's value as partly instrumental (preserving knowledge that could be applied, or could be decisively rejected, depending on future normative choices). But all agree the current extraction is minimal because dormancy has suspended the performance economy.
 *
 * DIRECTIONALITY LOGIC:
 *   All indexed perspectives classify as rope because the constraint has no current victim set and beneficiaries experience the constraint as genuine coordination (textual preservation, community identity). The halakhic study community and text preservation institutions are explicit beneficiaries — they derive their function and institutional legitimacy from preserving the sacrifice law corpus. The contemporary Jewish layperson is not a victim because participation is voluntary and no performance obligation exists. Reform and Reconstructionist movements are beneficiaries from a different angle: the performance-only reading's dormancy logic enables their ethical evolution away from animal sacrifice without textual repudiation. The analytical observer sees low extraction because the kernel is dormant — the sacrifice law commitment system has no active enforcement mechanism and no resource flows. The constraint becomes extractive only in the counterfactual restoration scenario, where resuming performance would reactivate hierarchies, resource extraction, and exclusions (omega variable: restoration_counterfactual_victims).
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading resolves a potential mandatrophy by explicitly framing sacrifice law study as archival preservation rather than active religious occupation. If study were treated as equivalent to performance (the study_as_exercise sibling reading), the mandate would persist in full force and the constraint would classify as tangled_rope or snare (depending on how resource extraction and hierarchy are maintained through study itself). The performance-only reading avoids this by declaring the commitment dormant: the mandate is suspended pending restoration, so study serves preservation rather than obligation-fulfillment. This prevents mislabeling the coordination function (knowledge preservation) as extraction. However, the reading creates a new mandatrophy risk in the counterfactual: if restoration occurs, the dormant extraction mechanisms reactivate without modification, and the constraint that was rope becomes snare. The ethical mandate then shifts: either modify the sacrifice system to eliminate victim classes (unlikely while maintaining halakhic continuity) or maintain dormancy permanently (which the performance-only reading does not explicitly require).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure,
    'Is the performance-only reading the only coherent interpretation of sacrifice law''s post-Temple status, or is it one position in an ongoing dispute over the kernel''s dormancy?',
    'This constraint is one reading of the temple_sacrifice_commitment kernel. Sibling readings (study_as_exercise, hybrid_preparatory) produce structurally different constraints with different victim sets and extraction profiles. The readings coexist across different halakhic communities.',
    'If performance-only is uniquely correct: sacrifice law is definitively dormant and study is archival preservation. If sibling readings are also coherent: the kernel supports multiple incompatible commitment structures, and the choice between them is a live normative dispute with different downstream extraction risks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-frame ambiguity: which reading of the sacrifice kernel is structurally live').

omega_variable(
    restoration_counterfactual_victims,
    'If the Temple were restored and sacrifice resumed under the performance-only reading''s framework (where study was mere preparation), who would bear the costs?',
    'Historical analysis of sacrificial economies: resource flows, labor extraction, purity-status hierarchies. Cross-reference with contemporary Temple movement rhetoric about restoration logistics.',
    'If victim set is empty even under restoration: performance-only reading remains rope. If restoration produces identifiable victims (animals, lower-status ritual workers, those excluded by purity requirements): the constraint''s current low ε reflects dormancy rather than intrinsic coordination character, and restoration would reclassify to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_counterfactual_victims, empirical, 'Identity of victim set under counterfactual restoration scenario').

omega_variable(
    study_theater_content,
    'Is sacrifice law study under the performance-only reading genuinely functional (preserving knowledge for potential application) or substantially theatrical (maintaining scholarly tradition with no realistic restoration pathway)?',
    'Survey halakhic scholars: do they study sacrifice law as contingent preparation (functional) or as historical-cultural preservation with no expectation of application (theatrical)? Quantify proportion of study hours vs realistic restoration probability assessments.',
    'If functional: theater_ratio remains low (~0.08). If substantially theatrical: theater_ratio should be higher (~0.30-0.50), indicating the constraint is partly piton — maintained through scholarly tradition rather than genuine preparation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_theater_content, empirical, 'Functional vs theatrical content of sacrifice law study').

omega_variable(
    axiom_empirical_grounding_drift,
    'The material_instantiation_required axiom is empirically_contingent (grounded in the claim that effective religious obligation requires physical performance). Has systematic evidence from other traditions (Buddhist meditation, Christian contemplative prayer, Islamic intention-based worship) undermined this empirical premise?',
    'Comparative religious studies: efficacy of non-material religious practice in sustaining community commitment, transmitting tradition, and shaping ethical behavior. If non-performance-based traditions show equal or greater commitment stability, the empirical grounding weakens.',
    'If evidence supports material-only efficacy: axiom remains empirically grounded. If evidence shows non-material practices equally effective: axiom''s empirical contingency is exposed as contestable rather than settled, potentially routing to foreclosed via axiom_overriding drift in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_empirical_grounding_drift, empirical, 'Empirical validity of the material-instantiation-required premise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_only_theater_70ce, performance_only, theater_ratio, 0, 0.05).
narrative_ontology:measurement(perf_only_theater_1570, performance_only, theater_ratio, 500, 0.06).
narrative_ontology:measurement(perf_only_theater_2070, performance_only, theater_ratio, 1000, 0.07).
narrative_ontology:measurement(perf_only_theater_2570, performance_only, theater_ratio, 1500, 0.08).

% Extraction over time
narrative_ontology:measurement(perf_only_extract_70ce, performance_only, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(perf_only_extract_1570, performance_only, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(perf_only_extract_2070, performance_only, base_extractiveness, 1000, 0.11).
narrative_ontology:measurement(perf_only_extract_2570, performance_only, base_extractiveness, 1500, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The kernel (temple_sacrifice_commitment) decomposes into three constraint stories (performance_only, study_as_exercise, hybrid_preparatory), each with different ε, beneficiary sets, and victim sets. The performance_only reading has the lowest ε because it treats the commitment as dormant; the other readings have higher ε because they sustain active obligation in different forms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
