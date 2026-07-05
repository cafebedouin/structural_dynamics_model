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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Hybrid Continuity Reading of Hebrew Vitality (Substrate + Reconstruction)
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This story generates ONE reading (hybrid_continuity_reading) of the
 *   contested Hebrew-vitality kernel. The claim is that liturgical
 *   preservation across roughly two millennia of diaspora functioned as a
 *   necessary enabling substrate — orthography, lexicon, textual corpus,
 *   ritualized fluency in reading and recitation — but was not itself
 *   sufficient to produce vitality in the sense of a living,
 *   natively-acquired vernacular. Vernacular revival in late 19th/early 20th
 *   century Palestine required, in addition, a deliberate reconstruction
 *   effort: lexical coinage for modern domains, grammatical regularization,
 *   and above all transmission to children as a first language, which
 *   converted a preserved liturgical register into a spoken mother tongue.
 *   This reading is offered as low-ε analytical synthesis, not an operative
 *   constraint with beneficiaries extracting from victims — its main effect
 *   is on how credit and causal weight get distributed in scholarly and
 *   popular narrative, not on any actor's resources or exit options. The
 *   sibling readings — liturgical_reading (ritual preservation constitutes
 *   vitality on its own) and native_daily_reading (only native generation
 *   counts; liturgical use was mere preservation of something otherwise
 *   inert) — are separate constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - revival_historiography_scholars: primary framers and beneficiaries of the synthesis (analytical/analytical)
 *   - language_revitalization_practitioners: adopt the framing as a planning template for other endangered languages (moderate/mobile)
 *   - liturgical_continuity_advocates: excluded sibling-reading holders who would contest the 'insufficient' framing (organized/mobile)
 *   - native_generation_purists: excluded sibling-reading holders who would contest the 'necessary' framing (organized/mobile)
 *   - comparative_linguists: analytical observers testing generalizability against other revival cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.06).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.04).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hybrid Continuity Reading of Hebrew Vitality (Substrate + Reconstruction)").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, 'bd6286fe-a810-4980-918a-9e1e1c409a08').
narrative_ontology:cs_kernel_codification('bd6286fe-a810-4980-918a-9e1e1c409a08', distributed).
narrative_ontology:cs_authority_grounding('bd6286fe-a810-4980-918a-9e1e1c409a08', distributed).
narrative_ontology:cs_reading_relation('bd6286fe-a810-4980-918a-9e1e1c409a08', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('bd6286fe-a810-4980-918a-9e1e1c409a08', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('bd6286fe-a810-4980-918a-9e1e1c409a08', foundational, liturgical_continuity_necessary_but_not_sufficient).
narrative_ontology:cs_axiom_status(liturgical_continuity_necessary_but_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('bd6286fe-a810-4980-918a-9e1e1c409a08', liturgical_continuity_necessary_but_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('bd6286fe-a810-4980-918a-9e1e1c409a08', foundational, deliberate_reconstruction_required_for_vitality).
narrative_ontology:cs_axiom_status(deliberate_reconstruction_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('bd6286fe-a810-4980-918a-9e1e1c409a08', deliberate_reconstruction_required_for_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('bd6286fe-a810-4980-918a-9e1e1c409a08', conjunctive_two_factor_revival_model).
narrative_ontology:cs_drift_state('bd6286fe-a810-4980-918a-9e1e1c409a08', post_comparative_revitalization_scholarship, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('bd6286fe-a810-4980-918a-9e1e1c409a08', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, revival_historiography_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_practitioners).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, necessary_but_insufficient_continuity_thesis).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, dual_requirement_substrate_and_reconstruction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance an analytical synthesis of the Hebrew revival case that treats liturgical continuity as necessary but not sufficient, requiring deliberate vernacular reconstruction (Ben-Yehuda-era coinage, syntactic innovation, child-acquisition transmission) as the second, independent ingredient. This framing gives them an integrative account that neither the pure-liturgical nor pure-native camp can offer, and it circulates well in comparative revitalization literature (Irish, Welsh, Hawaiian) where the analogy needs both terms to travel.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, revival_historiography_scholars, beneficiary,
    analytical, generational, analytical, global).

% Use the hybrid framing as a practical planning template for other endangered-language projects: it tells them liturgical/ceremonial maintenance alone will not produce a native-speaking generation, and that reconstruction efforts (corpus planning, immersion transmission) must be built on top of whatever substrate of continuous use already exists. They are not extracted from by this reading; they select it because it is more actionable than either pure reading alone.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_practitioners, beneficiary,
    moderate, generational, mobile, global).

% Hold that unbroken liturgical and textual use across two millennia already constitutes vitality in the relevant sense, and would object to being cast as having provided merely a 'necessary but insufficient' substrate — a demotion of ritual continuity to raw material for a later, more celebrated reconstruction effort. They are not consulted within this analytical synthesis; their reading is a sibling constraint, not a voice inside this one.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_continuity_advocates, excluded,
    organized, civilizational, mobile, global).

% Hold that only native daily generation constitutes vitality and that liturgical recitation was never more than preservation of a corpse — they would object to this hybrid reading's insistence that liturgical continuity was 'necessary,' since on their account the native-generation event could in principle have drawn on any sufficiently documented dead language. They are not represented inside this synthesis either.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_generation_purists, excluded,
    organized, biographical, mobile, national).

% Evaluate the hybrid continuity claim against other revival and revitalization cases to test whether the two-factor (substrate + reconstruction) model generalizes or is a post-hoc description fitted to the one successful full-revival case in the historical record.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates disagreement between the liturgical-continuity camp and the native-generation camp by supplying a synthesis that assigns each faction's preferred mechanism a role (enabling substrate vs. sufficient cause) rather than requiring either camp to fully concede.
% TRANSFER_FUNCTION: Moves explanatory credit, not resources: liturgical preservation is credited with supplying the necessary lexical, orthographic, and textual substrate, while reconstruction efforts (coinage, grammar planning, child transmission) are credited with supplying the sufficient condition for vitality. No money, labor, or coercive power changes hands through this reading itself.
% ABSENT_VOICES: The liturgical_reading and native_daily_reading camps are the parties most directly affected by how credit is distributed, and both would contest the hybrid framing's allocation — the liturgical camp because 'necessary but insufficient' undersells continuity, the native-generation camp because 'necessary' oversells continuity's causal role. Neither camp is a party inside this analytical synthesis; each exists as a separate sibling constraint.
% DISAPPEARANCE_RATIONALE: If this specific analytical synthesis vanished, the underlying historical facts about Hebrew's liturgical continuity and its 19th-20th century vernacular reconstruction would be unchanged, and the two contesting readings (liturgical, native-daily) would continue to be argued directly without this reconciling middle term. No institution, funding stream, or enforced practice depends on this reading persisting — it is a scholarly framing, not an operative arrangement.
% FOUNDING_PROBLEM: Historiographical and comparative-linguistics debate needed a way to explain the Hebrew case (which both camps cite as evidence) without either camp's single-factor account fully fitting the historical record: liturgical continuity alone did not produce native speakers for ~1700 years, and reconstruction efforts alone (absent millennia of textual/liturgical substrate) have no comparable success case among fully dead languages.
% FOUNDING_PROBLEM_CORROBORATION: Comparative linguists working outside both the liturgical-preservation and native-revival advocacy communities (e.g., scholars of Irish and Hawaiian revitalization who cite Hebrew as the outlier case) attest that neither single-factor account survives comparison across revival attempts, and that some two-factor model is needed to explain why Hebrew succeeded where other liturgically-preserved languages did not revive natively.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-floor (0.06) because this reading transfers no resources, coerces no compliance, and gates no access to anything materially scarce — it is a historiographical synthesis competing for adoption in academic and popular narrative. Suppression is likewise near-floor (0.04): no party is coerced into adopting this reading, and the sibling readings continue to be argued openly and vigorously in the same literature. Theater ratio is low but nonzero (0.12, drifting slightly upward) reflecting that some popularizations invoke the 'necessary and insufficient' formula performatively, as a rhetorical device to seem to have resolved the liturgical/native-daily dispute without actually engaging either camp's strongest objections. Accessibility collapse is moderate (0.2) — the hybrid framing has become a default textbook synthesis in some venues, which does modestly crowd out engagement with either pure reading, though both remain fully articulable and are still argued by their advocates. Resistance is moderate (0.35): both sibling camps actively contest the hybrid framing's allocation of causal credit, which is exactly the kind of live disagreement an ε-invariant single reading should not try to average away.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no genuine victim class here — the beneficiaries (scholars and practitioners who find the synthesis useful) do not extract from the excluded parties (advocates of the sibling readings); the excluded parties are excluded from THIS reading's internal conversation, not harmed by it, since their own readings persist as fully live alternative constraints elsewhere in the kernel. This is why beneficiaries are declared but victims are not: the structural delta expected for this reading (no clear beneficiary/victim structure) is honored by leaving the victims array empty rather than manufacturing an asymmetry that is not present in an analytical synthesis with no coercive or resource-transferring mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (neither single-factor account survives comparison across revival attempts) is still live per comparative linguists working outside the two contesting camps, so this is not a case of an arrangement outliving its function — it is closer to an ongoing, contested piece of analytical work that has not disappeared because the puzzle it addresses has not been solved to the contesting parties' mutual satisfaction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_as_genuine_synthesis_or_reframing_move,
    'Is the hybrid_continuity_reading a genuinely distinct causal-structure claim supported by comparative evidence, or is it primarily a rhetorical reframing move that lets historiographers avoid adjudicating between the liturgical_reading and native_daily_reading camps?',
    'Systematic comparison across multiple language revival/revitalization attempts (Irish, Cornish, Welsh, Hawaiian, Wampanoag) coding for presence/absence of (a) sustained liturgical/ceremonial substrate and (b) deliberate reconstruction-plus-child-transmission effort, checking whether the two-factor conjunction predicts successful native-speaker revival better than either factor alone.',
    'If the conjunction model outpredicts single-factor models across cases, the hybrid reading is a genuine structural finding; if it does not generalize beyond the Hebrew case, the reading is closer to a post-hoc reconciliation fitted to one data point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_as_genuine_synthesis_or_reframing_move, empirical, 'Whether the two-factor conjunction is a generalizable finding or a single-case reconciliation.').

omega_variable(
    necessity_claim_contestability,
    'Is liturgical continuity actually necessary for successful vernacular revival, or could a sufficiently documented dead language without liturgical continuity have undergone equivalent reconstruction-plus-transmission revival?',
    'Examine cases of language revival attempted on languages with strong textual documentation but without continuous liturgical/ceremonial use (e.g., some constructed-revival efforts on classical languages), and compare revival success/failure to the Hebrew case.',
    'If revival can succeed without a liturgical substrate given sufficient documentation, the ''necessary enabler'' claim central to this reading weakens, moving the kernel''s center of gravity toward the native_daily_reading; if no counterexample exists, the necessity claim is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_claim_contestability, empirical, 'Whether liturgical continuity is truly necessary or merely one sufficient path to an adequate documentary substrate.').

omega_variable(
    cs_framing_under_determination,
    'Is the correct committer framing here ''three readings of one vitality kernel'' (as declared), or is the hybrid reading better modeled as a meta-level arbitration mechanism sitting ABOVE the two object-level readings rather than as a peer sibling to them?',
    'Compare how each reading is invoked in practice: if scholars treat the hybrid reading as adjudicating between the other two (a higher-order move) rather than as a third candidate answer to the same first-order question, the kernel structure itself may need revision to a two-level model.',
    'If the hybrid reading is meta-level rather than peer-level, its reading_relations to the siblings would be better modeled as ''influences'' in both directions simultaneously rather than as independent sibling claims, and its extremely low ε would be explained structurally (arbitration moves are inherently less extractive than object-level claims) rather than as a contingent fact about this particular synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether the hybrid reading is a peer sibling or a meta-level arbitration move within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t8, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(hebr_tr_t16, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(hebr_tr_t24, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(hebr_tr_t32, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(hebr_be_t8, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 8, 0.05).
narrative_ontology:measurement(hebr_be_t16, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(hebr_be_t24, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 24, 0.06).
narrative_ontology:measurement(hebr_be_t32, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 40, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial 'was Hebrew ever really dead' / 'what made Hebrew revival possible' debate, per the ε-invariance principle. liturgical_reading claims ritual preservation alone constitutes vitality (high continuity credit, low reconstruction credit). native_daily_reading claims only native generation constitutes vitality (low continuity credit, high reconstruction credit, treats liturgical use as inert preservation). This hybrid_continuity_reading claims both are necessary jointly and neither alone is sufficient. All three share near-zero ε and no coercive beneficiary/victim structure, differing instead in causal-credit allocation and in which downstream popular/scholarly narratives they legitimate. Network edges are declared as 'influences' rather than dependency because no reading is temporally or evidentially upstream of the others — they are competing syntheses of the same shared historical record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
