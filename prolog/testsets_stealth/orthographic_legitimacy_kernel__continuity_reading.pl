% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Legitimacy as Preserved Traditional Access (Continuity Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   In 1928 the Republic of Turkey replaced the Arabic-based Ottoman
 *   orthography with a Latin alphabet, and within a generation the state's
 *   entire pre-modern textual inheritance — chronicles, court registers,
 *   poetry, religious commentary, family papers — passed behind a script
 *   barrier that ordinary schooling no longer crosses. This story authors the
 *   continuity reading of that arrangement: the position that a script regime
 *   is legitimate insofar as it preserves its population's access to the
 *   tradition written in the scripts it replaced. Assessed by that reading's
 *   own lights, the standing arrangement is a loss-bearing invariant: the
 *   incompatibility between scripts is a fact no decree can repeal, the harm
 *   falls on every post-reform generation, and no seat collects the destroyed
 *   surplus — what remains is a specialist mediation economy selling partial
 *   restoration. The epsilon referent is the standing Latin-script
 *   arrangement with its severed corpus, never the restored-access
 *   counterfactual this reading endorses. KEY AGENTS (by structural
 *   relationship): - post_reform_generations: primary target
 *   (powerless/constrained) — bear the severed access -
 *   diaspora_record_dependents: secondary target (powerless/trapped) — pay
 *   mediation to reach their own records - ottomanist_specialists: contingent
 *   beneficiary and access administrator (organized/identity_locked) -
 *   republican_state_agencies: agenda setter (institutional/arbitrage) —
 *   could bridge the barrier at any budget cycle -
 *   heritage_translation_publishers: incidental beneficiary (moderate/mobile)
 *   — sell curated access - comparative_literacy_historians: analytical
 *   observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - post_reform_generations: primary target (powerless/constrained) — post-1928-educated public bearing the severed access to the pre-1928 corpus
 *   - diaspora_record_dependents: secondary target (powerless/trapped) — Balkan, Levant, and Anatolian families forced to buy mediation for their own deeds and registers
 *   - ottomanist_specialists: contingent beneficiary and access administrator (organized/identity_locked) — philologists and archivists whose scarce fluency is their standing and whose readership keeps shrinking
 *   - republican_state_agencies: agenda setter (institutional/arbitrage) — ministries and archives that reproduce the single-script public and decline to fund bridges
 *   - heritage_translation_publishers: incidental beneficiary (moderate/mobile) — presses monetizing curated transcription of the classics
 *   - comparative_literacy_historians: analytical observer (analytical/analytical) — external scholarship measuring the transition against other script reforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.26).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.12).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy as Preserved Traditional Access (Continuity Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '9ce46823-8250-4d53-af13-7f6395db9aca').
narrative_ontology:cs_kernel_codification('9ce46823-8250-4d53-af13-7f6395db9aca', distributed).
narrative_ontology:cs_authority_grounding('9ce46823-8250-4d53-af13-7f6395db9aca', lineage).
narrative_ontology:cs_interpretation_layer_present('9ce46823-8250-4d53-af13-7f6395db9aca').
narrative_ontology:cs_reading_relation('9ce46823-8250-4d53-af13-7f6395db9aca', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('9ce46823-8250-4d53-af13-7f6395db9aca', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('9ce46823-8250-4d53-af13-7f6395db9aca', foundational, legitimacy_requires_traditional_access).
narrative_ontology:cs_axiom_status(legitimacy_requires_traditional_access, holdable).
narrative_ontology:cs_axiom_grounding('9ce46823-8250-4d53-af13-7f6395db9aca', legitimacy_requires_traditional_access, deontological).
narrative_ontology:cs_axiom('9ce46823-8250-4d53-af13-7f6395db9aca', secondary, unbridged_script_transition_breaches_intergenerational_trust).
narrative_ontology:cs_axiom_status(unbridged_script_transition_breaches_intergenerational_trust, holdable).
narrative_ontology:cs_axiom_grounding('9ce46823-8250-4d53-af13-7f6395db9aca', unbridged_script_transition_breaches_intergenerational_trust, deontological).
narrative_ontology:cs_reference_frame('9ce46823-8250-4d53-af13-7f6395db9aca', continuous_textual_access).
narrative_ontology:cs_drift_state('9ce46823-8250-4d53-af13-7f6395db9aca', post_1928_latin_regime, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9ce46823-8250-4d53-af13-7f6395db9aca', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, diaspora_record_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, ottomanist_specialists).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, heritage_translation_publishers).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, continuity_of_access_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, intergenerational_textual_trust).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Turks educated after 1928 in the Latin alphabet. They encounter the pre-1928 corpus — Ottoman poetry, chronicles, court records, religious commentary, family letters — only through translation, transcription, or years of elective study. Nothing bars them from learning the older script; evening courses and university seminars exist, but the investment is measured in years and competes with everything else in a life. Most inherit the loss as a closed shelf in the national library.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, biographical, constrained, national).

% Families and successor communities across the former Ottoman lands — the Balkans, the Levant, Anatolia — whose property deeds, waqf endowments, court registers, and correspondence exist only in the pre-1928 script. Courts and land registries periodically force them to hire certified readers and transcribers to prove inheritance or title. They cannot walk away from the records; the records are the claim.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, diaspora_record_dependents, payer,
    powerless, generational, trapped, continental).

% University philologists, archivists, and certified transcribers who retain reading fluency in the pre-1928 script. Their expertise is scarce precisely because the general population lost it; they staff the transliteration projects, certify editions, referee access to manuscript collections, and set transcription conventions. Their professional standing is built on the skill gap: a generation universally fluent in the old script would dissolve their gatekeeping role, yet their discipline also depends on a living readership that keeps shrinking.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottomanist_specialists, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__continuity_reading, ottomanist_specialists, agenda_setter).

% The ministries of education and culture, the state archives, and the language authority. They run the school system that reproduces the single-script public, fund or decline to fund translation and digitization programs, and decide curricular tokens of old-script instruction. They could commission mass transliteration or universal old-script teaching at any budget cycle; successive governments have chosen token provision instead. Nothing structurally prevents them from reversing provision.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, republican_state_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Commercial and foundation presses that sell transcribed, modernized-letter editions of pre-1928 works. The wider the reading public's distance from the original script, the larger their market for mediated classics. They select which titles earn transcription, which shapes what portion of the tradition stays in circulation. If direct access were restored, their niche would close.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, heritage_translation_publishers, beneficiary,
    moderate, biographical, mobile, national).

% Scholars of literacy, script reform, and state formation who study the 1928 transition alongside contemporaneous and later script reforms elsewhere. They hold no stake in Turkish cultural politics; they measure literacy curves, translation output, and archival usage, and compare sequences across countries.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, comparative_literacy_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single shared alphabet lets one school system, one print trade, and one administration address the entire literate public in one code; whatever else it does, the standing arrangement solves the problem of a mass reading public needing a common script.
% TRANSFER_FUNCTION: Moves direct access to the pre-1928 corpus out of general circulation and concentrates it in a trained specialist cadre; moves mediation fees from record-dependent families and curious readers to transcribers, teachers, and presses.
% ABSENT_VOICES: The pre-1928 authors and the first cohort of adults whose literacy was invalidated in 1928-29 are dead and cannot object; the rural populations of the reform era were never consulted and left no organized dissent. In the present, the poorest record-dependent families object through cost — they appear in court files as litigants, not in curriculum debates.
% DISAPPEARANCE_RATIONALE: If the access barrier vanished overnight — every pre-1928 text instantly legible to the Latin-script public — the specialist mediation economy would dissolve, historiography and religious pedagogy would reorganize around direct sources, the literary canon would re-expand backward, and thousands of pending inheritance and title disputes would resolve from the documents themselves.
% FOUNDING_PROBLEM: Mass illiteracy: in the 1920s only a small fraction of the population could read any script, the Arabic-based orthography took years of religious schooling to master, and the state could not communicate with its citizens in print.
% FOUNDING_PROBLEM_CORROBORATION: External statistical series — UNESCO and World Bank literacy records, historical-demography surveys — show Turkish literacy rising from under ten percent to near-universal within two generations; no attestation from inside any cultural faction is needed, and the original reform coalition's own successors no longer defend the arrangement on literacy grounds.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.26 at interval end) because the continuity reading locates harm in loss, not transfer: the standing arrangement destroys access rather than moving a surplus to a collector, and the receipts that do exist — transcription fees, edition sales, specialist salaries — are payments for partial remediation rather than capture of the extracted value. The temporal arc peaks around 1970, when the last broadly bilingual cohort dies and direct access bottoms out, then partially recedes as translation and digitization build mediated substitutes — never to zero, because mediated access is curated access. Suppression is low and decaying: the coercive machinery of the early reform decades (printing and signage bans, enforced alphabet transition) lapsed by the century's end; publishing in the old script is legal today and nothing prevents its study. Theater rises steadily as enforcement decays into commemoration — calligraphy exhibitions, reform anniversaries, ornamental old-script in schoolbooks — until roughly half of what visibly happens around the arrangement is performative. Accessibility collapse is moderate (0.40): understanding the barrier does not close the alternatives, since years-long study and mediated editions remain available, so this lacks a natural law's total closure. Resistance is moderate (0.45): transliteration campaigns, archival-access activism, and recurring curriculum fights meet the arrangement episodically. The claim/metric gap is deliberate: claimed_type is mountain because the binding core — script incompatibility, the impossibility of legislating comprehension — is invariant, while the metrics describe the mitigable arrangement around that core. The engine weighs that divergence; this story does not reconcile it. All three series run on one shared eight-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the post-reform public's seat the arrangement computes as a closed shelf: a heritage priced in years of study they were never asked to spend. From the specialist seat the same structure is professional order — scarcity is standing, certification is livelihood — though the shrinking readership bites back. From the state seat it is settled administrative normality, revisitable at any budget cycle and revisited by none. The engine computes these divergent per-seat types from power, exit, and directionality; the continuity reading's own condemnation of the arrangement is one input among these, not their arbiter.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive the target end: post_reform_generations and diaspora_record_dependents bear the barrier's costs with constrained or trapped exit, placing them near full-target directionality. The specialist cadre is declared beneficiary but carries a directionality override (organized, d=0.30): the automatic derivation from beneficiary status would place them near the subsidy end, yet their benefit is contingent on a scarcity they did not choose, and the same severance hollows their readership and discipline — their net position is dampened, not inverted. State agencies sit near symmetric: they neither collect from the barrier nor suffer it materially, holding arbitrage-grade optionality they decline to exercise. Publishers collect mediation fees but remain mobile and small. The affirmation behind gain_flow='diffuse' is that no named seat captures the destroyed surplus — receipts are service fees at the barrier's edge, not capture of what the barrier removes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass illiteracy under a hard script — is dead by external statistical record, yet the arrangement persists and the world would rearrange if the barrier lifted, so the dead-problem-plus-world-rearranges mismatch flag is expected here. The mandatrophy analysis resolves it against both mislabels: this is not a snare, because no seat captures the arrangement's operation (gain_flow is affirmatively diffuse, and the coercive enforcement that once ran it has decayed to near zero); and it is not a piton of mere theatrical maintenance, because the arrangement's persistence is carried by an invariant — script incompatibility — that no administrator could repeal, surrounded by a mitigation equilibrium of token curricula and market mediation that is cheap to continue and prohibitive to replace. The continuity reading's contribution is to keep the loss visible inside that equilibrium: theatrical commemoration grows precisely as functional access atrophies, and the reading refuses to let the commemoration stand in for the access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame_underdetermination,
    'This file authors the continuity_reading of the orthographic_legitimacy_kernel; would the instrumentalist_reading (literacy and efficiency) or the modernist_reading (Western alignment and rupture) locate a different constraint — different victims, different epsilon — over the same 1928 transition?',
    'Generate the sibling readings as separate stories and compare computed types, victim sets, and epsilon over the identical interval; divergence localizes the disagreement to the legitimacy criterion itself.',
    'Within the continuity frame the arrangement is a loss-bearing invariant with no capturer; under the instrumentalist frame the same transition computes as enforced extraction from the coerced first cohort; under the modernist frame as emancipation. Cross-reading comparison, not this file alone, carries the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_underdetermination, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    naturality_vs_constructed_periphery,
    'Is the access barrier an invariant fact of script incompatibility, or a maintained arrangement whose persistence depends on continued political choices not to bridge it?',
    'Counterfactual costing: price a universal transliteration plus old-script-teaching program and observe whether any government adopts it; if the barrier survives affordable bridges repeatedly declined, the maintained share is constructed.',
    'Pure invariance supports the mountain claim; a large affordably-bridgeable share would push the standing arrangement toward scaffold or piton readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_periphery, empirical, 'Naturality ambiguity: invariant incompatibility versus politically maintained non-bridging.').

omega_variable(
    mediation_substitution_depth,
    'Does translation and digitization genuinely substitute for direct access, or does curated mediation replace the tradition with a specialist-selected fraction of it?',
    'Compare the transcribed-and-published corpus against the full surviving corpus by genre, period, and viewpoint; measure what mediation drops.',
    'Deep substitution validates the declining extraction tail after 1970; shallow substitution means the public inherits a curated tradition and the loss is larger than the scalar records show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediation_substitution_depth, empirical, 'Whether mediated access restores the tradition or replaces it with a selection.').

omega_variable(
    loss_visibility_adaptation,
    'Is the weak contemporary demand for old-script access a structural fact (existing alternatives suffice) or internalized adaptation (generations no longer perceive what was severed)?',
    'Reception studies: track demand spikes when mediated editions appear and when diaspora record disputes surface; persistent latent demand revealed by supply indicates adapted rather than absent need.',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s deepest hold is that its victims cannot name the loss — and classification shifts toward higher suppression weighting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(loss_visibility_adaptation, conceptual, 'Structural versus internalized (adapted) suppression of access demand.').

omega_variable(
    specialist_rent_magnitude,
    'Do the credentialed old-script readers collect meaningful rents from the access barrier, or are their receipts ordinary wages for mediation services?',
    'Income and market analysis of transcription, certification, and edition-preparation work against comparable skilled labor outside the barrier.',
    'Material rents would add a captured-beneficiary structure the current story denies and pull the arrangement toward extraction readings; wage-level receipts confirm the no-capturer picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialist_rent_magnitude, empirical, 'Size of specialist gatekeeping rents over the access barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 1928, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement_basis(orth_tr_t1928, observed).
narrative_ontology:measurement(orth_tr_t1940, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement_basis(orth_tr_t1940, observed).
narrative_ontology:measurement(orth_tr_t1955, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement_basis(orth_tr_t1955, observed).
narrative_ontology:measurement(orth_tr_t1970, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement_basis(orth_tr_t1970, observed).
narrative_ontology:measurement(orth_tr_t1985, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement_basis(orth_tr_t1985, observed).
narrative_ontology:measurement(orth_tr_t2000, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(orth_tr_t2000, observed).
narrative_ontology:measurement(orth_tr_t2015, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2015, 0.47).
narrative_ontology:measurement_basis(orth_tr_t2015, observed).
narrative_ontology:measurement(orth_tr_t2026, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 2026, 0.5).
narrative_ontology:measurement_basis(orth_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1928, 0.24).
narrative_ontology:measurement_basis(orth_be_t1928, observed).
narrative_ontology:measurement(orth_be_t1940, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1940, 0.3).
narrative_ontology:measurement_basis(orth_be_t1940, observed).
narrative_ontology:measurement(orth_be_t1955, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1955, 0.34).
narrative_ontology:measurement_basis(orth_be_t1955, observed).
narrative_ontology:measurement(orth_be_t1970, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1970, 0.36).
narrative_ontology:measurement_basis(orth_be_t1970, observed).
narrative_ontology:measurement(orth_be_t1985, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 1985, 0.34).
narrative_ontology:measurement_basis(orth_be_t1985, observed).
narrative_ontology:measurement(orth_be_t2000, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(orth_be_t2000, observed).
narrative_ontology:measurement(orth_be_t2015, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement_basis(orth_be_t2015, observed).
narrative_ontology:measurement(orth_be_t2026, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 2026, 0.26).
narrative_ontology:measurement_basis(orth_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1928, 0.62).
narrative_ontology:measurement_basis(orth_su_t1928, observed).
narrative_ontology:measurement(orth_su_t1940, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement_basis(orth_su_t1940, observed).
narrative_ontology:measurement(orth_su_t1955, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1955, 0.44).
narrative_ontology:measurement_basis(orth_su_t1955, observed).
narrative_ontology:measurement(orth_su_t1970, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement_basis(orth_su_t1970, observed).
narrative_ontology:measurement(orth_su_t1985, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 1985, 0.21).
narrative_ontology:measurement_basis(orth_su_t1985, observed).
narrative_ontology:measurement(orth_su_t2000, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement_basis(orth_su_t2000, observed).
narrative_ontology:measurement(orth_su_t2015, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2015, 0.13).
narrative_ontology:measurement_basis(orth_su_t2015, observed).
narrative_ontology:measurement(orth_su_t2026, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(orth_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1928 script reform' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints indexed by legitimacy criterion. This file (continuity_reading) authors low epsilon over a loss-bearing invariant with diffuse victims; the instrumentalist_reading authors the transition as enforced extraction from the coerced first cohort; the modernist_reading authors it as emancipatory alignment. The upstream member by current institutional force is the instrumentalist_reading, whose criteria govern the standing arrangement; this continuity reading is the oppositional heritage frame that cites the same transition as evidence of breach. Every family member links the others via network edges; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
