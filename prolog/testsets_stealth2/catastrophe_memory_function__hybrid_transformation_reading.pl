% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover Seder Complex — Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the Passover seder complex: the
 *   annual household performance of the Exodus retelling, the week-long
 *   leaven prohibition, the mandated bitter herbs, and the child-directed
 *   questioning structure that together constitute the most widely observed
 *   practice in diaspora Jewish life. This file instantiates ONE reading of
 *   the catastrophe_memory_function kernel — the
 *   hybrid_transformation_reading — under which the ritual fuses two content
 *   streams in a single structure: bitter herbs and loss-narrative encode
 *   catastrophe-taste memory (mourning-practice, D1/D4), while the ordered
 *   seder performance rehearses survival competencies (haste-readiness in the
 *   matzah script, decentralized household continuity, intergenerational
 *   questioning discipline, D5). Per the one-reading discipline, the sibling
 *   readings (mourning_practice_reading, survival_competence_reading) are
 *   separate constraints linked through the network surface; nothing about
 *   them is averaged into this file. Epsilon's referent is the standing
 *   arrangement as actually practiced, assessed by this reading's own lights
 *   — the fusion claim raises the assessed functionality and lowers the
 *   assessed waste, but does not relocate the referent to any endorsed
 *   alternative. Stated assumption: interval t approximates year 1930+t, with
 *   the measurement record centered on the Ashkenazi diaspora, where the
 *   richest observational literature exists.
 *
 * KEY AGENTS:
 *   - - diaspora_jewish_households: primary coordinated unit (organized/identity_locked) — convenes, funds, and staffs the annual performance; simultaneously bears its direct costs and collects its continuity goods
 *   - - seder_children: transmission targets (powerless/trapped) — recite the questions, taste the memorial foods, absorb the narrative; cannot opt out of the family performance
 *   - - household_preparers_women: concentrated cost-bearers (moderate/constrained) — kashering, leaven-purging, and multi-course production historically unnamed in the liturgy itself
 *   - - rabbinic_authorities: rule-setters (institutional/arbitrage) — adjudicate seder order, leaven definitions, and customary expansions; adapt the structure without exiting it
 *   - - kosher_food_industry: seasonal commercial collector (powerful/arbitrage) — captures premium-priced demand created by the prohibition calendar
 *   - - assimilating_jews: marginal participants (moderate/constrained) — attend under relational pressure while identifying secularly; pay friction costs without collecting much benefit
 *   - - ritual_scholars: analytical observers (analytical/analytical) — document the memory mechanics and drift from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.3).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover Seder Complex — Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'e2fab73f-56a4-405c-9265-73c53584b426').
narrative_ontology:cs_kernel_codification('e2fab73f-56a4-405c-9265-73c53584b426', fixed_text).
narrative_ontology:cs_authority_grounding('e2fab73f-56a4-405c-9265-73c53584b426', lineage).
narrative_ontology:cs_interpretation_layer_present('e2fab73f-56a4-405c-9265-73c53584b426').
narrative_ontology:cs_reading_relation('e2fab73f-56a4-405c-9265-73c53584b426', catastrophe_memory_function__mourning_practice_reading, influences).
narrative_ontology:cs_reading_relation('e2fab73f-56a4-405c-9265-73c53584b426', catastrophe_memory_function__survival_competence_reading, influences).
narrative_ontology:cs_axiom('e2fab73f-56a4-405c-9265-73c53584b426', foundational, mourning_survival_mutual_dependence).
narrative_ontology:cs_axiom_status(mourning_survival_mutual_dependence, holdable).
narrative_ontology:cs_axiom_grounding('e2fab73f-56a4-405c-9265-73c53584b426', mourning_survival_mutual_dependence, empirically_contingent).
narrative_ontology:cs_axiom('e2fab73f-56a4-405c-9265-73c53584b426', secondary, embodied_annual_rehearsal_necessary).
narrative_ontology:cs_axiom_status(embodied_annual_rehearsal_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e2fab73f-56a4-405c-9265-73c53584b426', embodied_annual_rehearsal_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('e2fab73f-56a4-405c-9265-73c53584b426', dual_function_memorial_rehearsal).
narrative_ontology:cs_drift_state('e2fab73f-56a4-405c-9265-73c53584b426', contemporary_diaspora_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e2fab73f-56a4-405c-9265-73c53584b426', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, seder_children).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, kosher_food_industry).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, household_preparers_women).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, assimilating_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, exodus_liberation_narrative).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, generational_telling_obligation).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, decentralized_household_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the annual seder, fund and produce it, and hold the week's dietary discipline. From the household's position the practice returns the family-gathering good, the continuity of identity across generations, and the annual reaffirmation of belonging. Leaving means absenting themselves from the family table and from communal recognition — the practice and the belonging have fused, so the door out is also the door out of the identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households, payer).

% Recite the four questions, taste the memorial foods, and absorb the narrative and the behavioral scripts it carries. Participation is not elective at their age; the performance is the family event they cannot decline. What they receive — memory, competence scripts, membership — arrives before they could consent to it and compounds as they grow into the adult obligations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, seder_children, beneficiary,
    powerless, biographical, trapped, global).

% Kashering kitchens, purging leaven, sourcing substitute foods, and producing multi-course meals for extended family — labor that the liturgy itself historically never names. The work is seasonal, intense, and concentrated on them by household-division convention; declining it carries family-role penalties, and the recognized honorifics of the table go to the performers of the retelling rather than the producers of the meal.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, household_preparers_women, payer,
    moderate, biographical, constrained, global).

% Adjudicate the seder order, the boundaries of the leaven prohibition, and permissible customary additions; publish haggadot; rule on new cases. They administer the structure and can revise it — movements have shortened, expanded, and retranslated it repeatedly — without ever having to leave it. Their authority is renewed annually by the practice's observance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Sells matzah, seder staples, and certified substitute products into a demand spike the prohibition calendar guarantees, at documented seasonal price premiums. Bears none of the practice's costs, sets none of its rules, and can redirect production freely if demand shifts.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, kosher_food_industry, beneficiary,
    powerful, biographical, arbitrage, continental).

% Attend the family seder while identifying secularly elsewhere in life. They absorb the preparation demands, the dietary week, and the relational friction of non-observance the rest of the year, while collecting little of the identity good their more affiliated relatives collect. Voicing their objection inside the family setting costs relationships, so it surfaces instead in memoir, sociology, and simple gradual absence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, assimilating_jews, payer,
    moderate, biographical, constrained, national).

% Study the seder's memory mechanics — how a dispersed population transmits catastrophe memory and practical scripts across centuries without centralized infrastructure. They collect no benefit from observance and bear none of its costs; their seat is the documentary record, from Yerushalmi-lineage historiography to contemporary demography of Jewish continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, kosher_food_industry).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a geographically dispersed population on an annual, household-hosted, embodied rehearsal that transmits catastrophe memory and survival scripts (haste-readiness, questioning discipline, decentralized continuity) across generations without requiring any centralized institution, building, or clergy for its core performance.
% TRANSFER_FUNCTION: Moves time, labor, and attention from household members — disproportionately the preparers — into the ritual economy; moves memory and competence scripts from elder to child generations; and moves seasonal revenue toward the certified-food and haggadah-publishing industries.
% ABSENT_VOICES: Assimilating Jews who experience the obligation as imposition voice it outside the family setting, in memoir and exit, not at the table; women whose labor sustains the meal were historically unnamed in the liturgy that commands it; children perform without consent. All three are present in the sociological and feminist-haggadah record rather than in the halakhic conversation where the structure is administered.
% DISAPPEARANCE_RATIONALE: If the seder complex vanished overnight, the highest-attendance event in diaspora Jewish life would disappear, intergenerational transmission of both the catastrophe memory and the embedded survival scripts would lose its primary vehicle, the annual identity-boundary reaffirmation would lapse to weaker substitutes, and the seasonal economic segment built on the prohibition calendar would collapse — the community's continuity arrangements would visibly reorganize around whatever partial replacements it could improvise.
% FOUNDING_PROBLEM: Preserve the memory of catastrophe (slavery and liberation) and maintain group continuity for a population living dispersed among host societies, without territory, temple, or state — transmitting both the loss-memory and the practical capacity to endure dispersion to each new generation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: historiography of Jewish memory (the Zakhor tradition) documents the transmission problem as the community's persistent organizing challenge; demographers of Jewish continuity attest the live status of the continuity problem; Holocaust-commemoration scholarship independently attests the ongoing functional relevance of catastrophe memory for this population. Rabbinic sources also self-attest the obligation, but the external scholarly record is the load-bearing corroboration here.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30: the arrangement imposes real, unevenly distributed costs — concentrated preparation labor, seasonal expense amplified by prohibition-driven premium pricing, and obligation pressure on marginally affiliated members — against heavily reciprocal benefit, so the extraction is bounded rather than dominant. Suppression is 0.35: enforcement is social and normative (communal visibility, family expectation, calendrical ubiquity) rather than coercive, with a significant internalized component; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio is 0.18: the pedagogical core (questions, retelling, tasting disciplines) is demonstrably functional, and rote performance is a minority segment even in low-affiliation households. Accessibility_collapse is 0.45: alternatives exist (secular commemoration, informal family storytelling, museum memorial practice) but none substitutes the integrated dual structure, so alternatives persist yet partially collapse for anyone inside the tradition. Resistance is 0.35: reform-era abbreviations, feminist haggadot, and secular attrition are real and documented but have reshaped rather than displaced the structure. The temporal series run on one shared ten-point grid (every tracked metric authored at every point, t approximately 1930+t). Two dynamics are traced: a generational oscillation in theater_ratio (postwar roteness peaking at t=30, declining through the havurah, feminist-haggadah, and engagement renewals) driven by assimilation-revival cycles rather than by intermittent reinforcement — the annual recurrence is the design, not an extraction mechanism; and a declining-then-stabilizing suppression_requirement series, authored because the story specifically tracks enforcement-capacity change: dense pre-war communal sanction eroded across the dispersal era while identity-lock absorbed the enforcement load, which is why the series falls and then flattens rather than continuing to zero.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same table. The preparer seat bears the arrangement's largest uncompensated cost with constrained exit and no historical liturgical recognition — from that chair the structure operates as asymmetric burden riding on a valued practice. The rabbinic seat holds agenda power with arbitrage-grade exit (movements have reformed the seder repeatedly without leaving it) — from that chair the structure is a living obligation successfully administered. The child seat collects the transmission benefit immediately while deferring its costs into adult obligation. The assimilating seat pays relational friction for a benefit it barely collects. The engine derives these divergent classifications from the authored power, exit, and role data; the divergence between the preparer's and the rabbi's experienced arrangement is the perspectival fact this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. Diaspora_jewish_households are declared beneficiaries but are simultaneously the arrangement's direct cost-bearers with identity_locked exit — the structural derivation reading the beneficiary list alone would push them too far toward the beneficiary pole, so an override sets their power-atom directionality to 0.45 (near-symmetric): they are coordinated-and-paying in the same gesture. Seder_children derive low directionality (declared beneficiaries, though trapped exit moderates this). Household_preparers_women derive high directionality (declared victims, constrained exit). Rabbinic_authorities derive low directionality (declared beneficiaries with agenda power and arbitrage exit). Kosher_food_industry derives the lowest directionality (positional beneficiary, arbitrage exit, bears none of the practice costs). Assimilating_jews derive high directionality (declared victims paying without collecting). Spatial scopes are global for the practice-bearing seats — verification of observance is diffuse and personal, which the engine's scope treatment reflects — and continental for the industry seat, whose exposure is regional markets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting catastrophe memory and survival capacity across generations for a population with no territorial institutions — remains live, corroborated from outside the beneficiary set, so no mandatrophy resolution is declared and none is keyed to any metric. The classification guards against two opposite mislabels. Reading the arrangement as pure coordination would erase the preparer seat's documented asymmetric burden and the obligation pressure on marginal members — costs that are real and that the same structure produces. Reading it as pure extraction would erase the transmission function, which is demonstrably load-bearing: populations that abandoned the structure lost the continuity it carried, which is why the structure persisted through maximal-cost conditions. The tangled_rope placement keeps both truths in one object: a genuine dual coordination function (memory preservation plus competence rehearsal) and a bounded, nameable asymmetric cost stream (concentrated labor, premium-priced compliance, relational coercion of the marginally affiliated) operating through the same structure under active normative enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the catastrophe_memory_function kernel correctly models the seder''s persistent structure — hybrid fusion, mourning-only preservation, or survival-competence transmission?',
    'Comparative study of communities practicing stripped variants: celebratory or novelty haggadot with mourning content minimized, and museum-style or memorial-only catastrophe observance without survival-rehearsal content; track retention and intergenerational transmission outcomes against full-structure communities.',
    'If stripped variants transmit as well as the full structure, this hybrid reading collapses into whichever surviving sibling is correct; if stripped variants measurably degrade, fusion is load-bearing and this reading stands as the correct unit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-level ambiguity over which of three readings captures the ritual''s operative structure.').

omega_variable(
    mourning_survival_separability,
    'Are the mourning components (bitter herbs, loss-narrative recitation) and the survival components (haste-scripts encoded in matzah, child-questioning discipline, household-decentralized performance) mechanically separable within the ritual?',
    'Component-ablation comparison across communities: measure identity-retention and practical adaptive-knowledge scores where one content stream is de-emphasized while the other is retained.',
    'Demonstrated separability would decompose this constraint into the two sibling constraints as independent structures; demonstrated codependence confirms the hybrid as the correct unit of classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_survival_separability, conceptual, 'Whether the ritual''s two content streams are structurally fused or severable — the hybrid reading''s distinctive bet.').

omega_variable(
    gendered_labor_contingency,
    'Is the concentration of preparation labor on women constitutive of the ritual structure, or contingent on surrounding household-division conventions?',
    'Cross-community comparison of egalitarian-household seders against conventionally divided households: does burden redistribution persist without loss of ritual function or attendance?',
    'If contingent, the asymmetric cost component is removable without function loss and the arrangement sits nearer pure coordination; if constitutive, the asymmetry is structural and the hybrid coordination-plus-asymmetric-cost characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_labor_contingency, empirical, 'Whether the ritual''s principal asymmetric cost is structural or conventional.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (communal visibility, family expectation, calendrical ubiquity) versus internalized (obligation experienced as self-definition)?',
    'Longitudinal panels of disaffiliating Jews: does participation pressure persist after community exit, and does it decay on sustained contact with unstructured environments?',
    'If largely internalized, effective suppression exceeds the structural measure and exit is weaker than it appears; if structural, pluralistic environments should progressively erode it without identity work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized composition of the arrangement''s suppressive force.').

omega_variable(
    post_holocaust_function_layering,
    'Did post-Holocaust memory layer additional mourning content onto the seder''s original catastrophe-reference, altering the mourning share of the dual structure?',
    'Haggadot textual history and liturgical comparison tracking added commemorative segments in modern haggadot against pre-war baseline texts.',
    'If layered, part of the measured mourning function is historically recent, affecting drift assessment and the presumed stability of the hybrid balance; if not, the dual structure is older and more stable than drift readings suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_holocaust_function_layering, empirical, 'Historical layering of additional mourning content onto the ritual''s catastrophe-reference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 70, 0.17).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(cata_tr_t90, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.29).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 70, 0.31).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(cata_be_t90, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 90, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 70, 0.36).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(cata_su_t90, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 90, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Passover as catastrophe memory' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel. This file authors the hybrid_transformation_reading; the mourning_practice_reading and survival_competence_reading are separate stories with their own epsilon values, beneficiary/victim structures, and classifications. The epsilon values differ because each reading weights different structural features of the SAME referent arrangement: the mourning reading foregrounds obligation and boundary maintenance, the survival reading foregrounds transmitted competence, and the hybrid reading asserts mutual dependence of the two streams. Evidence flow runs upstream from historical continuity studies into all three; the hybrid sits downstream of both siblings, citing each one's evidence base for half of its fusion claim. All three files link one another through network.affects_constraints so contamination and confirmation propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
