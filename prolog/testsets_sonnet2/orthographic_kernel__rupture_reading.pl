% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: 1928 Turkish Script Reform as Deliberate Civilizational Rupture
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested orthographic kernel: the
 *   Turkish alphabet reform of 1928 read as a deliberate act of
 *   civilizational rupture — a calculated severance of the population's
 *   direct textual access to Ottoman administrative, legal, and Islamic
 *   religious tradition, undertaken to make a new secular-nationalist
 *   identity materially and cognitively plausible. This is not the
 *   modernization_reading (which holds the same event as primarily a genuine
 *   phonetic/technical improvement with continuity preserved through
 *   translation and institutional bridging) nor the continuity_reading (which
 *   holds Arabic script itself as constitutive of legitimate Ottoman-Islamic
 *   civilizational continuity, largely independent of literacy efficiency
 *   arguments). Under the rupture_reading, ε is authored very high: the
 *   standing arrangement under contest is the abrupt, punitively enforced
 *   discontinuity itself, assessed by this reading's own lights, not by the
 *   modernization reading's more benign framing of the same event.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: agenda-setter and structural beneficiary — designs, enforces, and narrates the rupture
 *   - pre_reform_literate_population: primary victim — mass functional illiteracy imposed overnight
 *   - ulema_and_religious_scholars: secondary institutional victim — loses exclusive textual authority
 *   - new_secular_educational_elite: beneficiary class created by the rupture itself
 *   - nationalist_historiography_project: non-agent vindicated narrative made plausible by the archive's inaccessibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.87).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.88).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "1928 Turkish Script Reform as Deliberate Civilizational Rupture").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '7296112b-73f3-4bab-acb8-610a179fbcda').
narrative_ontology:cs_kernel_codification('7296112b-73f3-4bab-acb8-610a179fbcda', formalized).
narrative_ontology:cs_authority_grounding('7296112b-73f3-4bab-acb8-610a179fbcda', extraction).
narrative_ontology:cs_interpretation_layer_present('7296112b-73f3-4bab-acb8-610a179fbcda').
narrative_ontology:cs_reading_relation('7296112b-73f3-4bab-acb8-610a179fbcda', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7296112b-73f3-4bab-acb8-610a179fbcda', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('7296112b-73f3-4bab-acb8-610a179fbcda', foundational, national_identity_requires_severance_from_prior_civilizational_frame).
narrative_ontology:cs_axiom_status(national_identity_requires_severance_from_prior_civilizational_frame, holdable).
narrative_ontology:cs_axiom_grounding('7296112b-73f3-4bab-acb8-610a179fbcda', national_identity_requires_severance_from_prior_civilizational_frame, instrumental).
narrative_ontology:cs_axiom('7296112b-73f3-4bab-acb8-610a179fbcda', secondary, textual_access_control_is_a_legitimate_instrument_of_state_identity_formation).
narrative_ontology:cs_axiom_status(textual_access_control_is_a_legitimate_instrument_of_state_identity_formation, holdable).
narrative_ontology:cs_axiom_grounding('7296112b-73f3-4bab-acb8-610a179fbcda', textual_access_control_is_a_legitimate_instrument_of_state_identity_formation, conventional).
narrative_ontology:cs_reference_frame('7296112b-73f3-4bab-acb8-610a179fbcda', ottoman_islamic_textual_continuity).
narrative_ontology:cs_drift_state('7296112b-73f3-4bab-acb8-610a179fbcda', post_1928_enforcement_completion, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7296112b-73f3-4bab-acb8-610a179fbcda', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_secular_educational_elite).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, nationalist_historiography_project).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ulema_and_religious_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_bureaucratic_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, arabic_script_literate_elderly).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, civilizational_break_from_ottoman_islamic_past_is_necessary_for_national_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Latin alphabet by law in 1928, criminalizes continued institutional use of Arabic script within a short compliance window, and builds the Millet Mektepleri (Nation's Schools) to retrain the adult population. Controls the framing of the change as forward-looking modernization while explicitly targeting the severance of textual access to Ottoman archives, religious jurisprudence, and Islamic scholarly tradition as an object, not a side effect.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, kemalist_state_apparatus, beneficiary).

% Millions of adults who read and wrote fluently in Ottoman Arabic script become functionally illiterate in the new national orthography overnight. They cannot read newspapers, official documents, correspondence, or their own family and religious texts without retraining. Exit does not exist within the nation-state; emigration is not a realistic option for most, and refusal carries social and economic exclusion.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    powerless, biographical, trapped, national).

% Their authority rested substantially on exclusive command of a textual tradition (Quranic Arabic script literacy, Ottoman legal and theological corpora) that the reform renders inaccessible to new generations. Some retain private religious authority; institutional standing and the pipeline of new literate scholars collapse as madrasas are closed and the new script severs students from primary sources without translation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ulema_and_religious_scholars, payer,
    moderate, generational, constrained, national).

% Career bureaucrats whose professional competence was built on Ottoman-script record-keeping, law, and correspondence are made obsolete within the civil service almost immediately. Some retrain and are absorbed into the new state apparatus on the condition of demonstrated loyalty to the new order; others are pushed out.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_bureaucratic_class, payer,
    moderate, biographical, constrained, national).

% Young teachers, journalists, and officials trained natively in the Latin script from the reform's outset gain rapid social and institutional advancement, filling positions vacated by the deskilled Ottoman-literate class. Their careers and social status are built directly on the rupture rather than merely surviving it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_secular_educational_elite, beneficiary,
    organized, generational, mobile, national).

% The new national narrative — Turkish identity as pre-Ottoman/Anatolian/secular rather than continuous with Ottoman-Islamic civilization — is made materially plausible because the population loses direct reading access to the archive that would complicate it. This is a project/narrative, not an acting agent, but its legitimacy is a direct product of the script change.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, nationalist_historiography_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(orthographic_kernel__rupture_reading, nationalist_historiography_project).

% Older adults for whom retraining through the Millet Mektepleri campaign is least feasible are permanently cut off from full civic literacy for the remainder of their lives, dependent on younger literate relatives to read official notices, letters, and religious material.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, arabic_script_literate_elderly, payer,
    powerless, biographical, trapped, local).

% Generations not yet born who will need specialized training merely to read their own state's pre-1928 archives, court records, and literary output in the original. They have no voice in the 1928 decision and inherit an archive rendered foreign by policy design.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, future_turkish_researchers, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a single national orthography aligned with actual Turkish phonology, which genuinely eases literacy acquisition, printing, and administrative uniformity going forward — a real coordination gain independent of the rupture intent.
% TRANSFER_FUNCTION: Moves institutional legitimacy, bureaucratic employability, and interpretive authority over the national past from the Ottoman-Islamic textual tradition and its bearers to the secular Kemalist state apparatus and the newly literate generation it trained; the pre-reform literate population pays in stranded human capital and severed textual access.
% ABSENT_VOICES: The pre-reform literate population and the ulema were the objects of the policy, not participants in designing it; parliamentary debate occurred inside a state apparatus already committed to the rupture framing. Ottoman-era historians and theologians who might have argued for a transitional or dual-script approach were institutionally sidelined before the vote.
% DISAPPEARANCE_RATIONALE: Had the 1928 reform not occurred (or been reversed), the population would have retained continuous textual access to five centuries of Ottoman administrative, legal, and religious material; the ulema's institutional position would likely have persisted longer; the specific secular-nationalist historiography that requires archival distance to sustain itself would have had a harder time establishing itself against a literate, engaged rival tradition.
% FOUNDING_PROBLEM: Ottoman Arabic script was a genuinely poor phonetic fit for Turkish vowel harmony, producing high illiteracy under the old system; the reform's architects also explicitly sought to sever the population's direct access to Ottoman-Islamic textual authority as a deliberate nation-building act.
% FOUNDING_PROBLEM_CORROBORATION: State historiography and successor secular-nationalist scholarship attest the literacy problem was the primary and sufficient justification. Independent linguistic historians (including some sympathetic to the reform's technical merits) and historians of religion outside the Kemalist tradition attest that Atatürk's own recorded statements and the compressed, punitive enforcement timeline indicate the severance of Ottoman-Islamic continuity was a coequal, deliberately pursued goal, not an incidental byproduct of phonetic modernization.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near the ceiling (0.87 by mid-1940s) because the rupture reading holds that the entire pre-reform literate population's accumulated textual capital was rendered worthless by state fiat, for the specific and stated purpose of foreclosing a rival civilizational narrative, not merely to improve literacy going forward. Suppression starts extremely high (0.95) reflecting the criminalization of continued institutional Arabic-script use and the compressed compliance window, then gradually eases as generational replacement does the enforcement work that coercion no longer needs to do — this is why suppression_requirement declines even as extractiveness stabilizes: the mechanism shifts from active coercion to structural fait accompli. Accessibility collapse is authored near-total (0.92): once the reform's enforcement machinery and generational turnover complete, alternatives (dual literacy, gradual transition, translation bridging) are foreclosed as a practical matter, not merely disfavored. Resistance is moderate (0.55) — significant but ultimately unsuccessful opposition from religious and bureaucratic classes, suppressed within roughly a decade.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state_apparatus sits at the full-beneficiary end: it designed the mechanism, controls its enforcement, and collects the primary good (a citizenry cognitively and archivally separated from a rival legitimating tradition). The pre_reform_literate_population, ulema, and ottoman_bureaucratic_class sit at the full-target end: trapped or constrained exit, immediate and often irreversible loss of functional capital with no compensating institutional position offered. The new_secular_educational_elite is a genuine beneficiary distinct from the state apparatus itself — their advancement is a direct transfer product of the rupture, which is why they are listed as beneficiary rather than merely a neutral bystander.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) matters because there is a genuine coordination function present — a phonetically fitted national orthography does ease literacy acquisition and administrative uniformity going forward, and this reading does not deny that technical benefit exists. What makes it tangled rather than a clean rope is that, under this reading, the SAME structure that delivers the coordination benefit was deliberately weaponized to sever an entire population's access to five centuries of textual tradition as an explicit political goal, with active enforcement (criminalization, compressed timelines, closure of alternative institutions) required to make the severance stick rather than allowing gradual, voluntary, bridged transition. Collapsing this into a pure snare would deny the real forward-literacy gain; collapsing it into a pure rope (the modernization_reading's territory) would launder the deliberate rupture intent into incidental technical necessity — that laundering is exactly what the sibling modernization_reading does, and exactly what this reading refuses to do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_incidental_severance,
    'Was the severance of Ottoman-Islamic textual continuity a coequal, deliberately pursued goal of the 1928 reform, or an incidental (if foreseeable) side effect of a primarily phonetic/literacy-driven modernization policy?',
    'Close reading of Atatürk''s private correspondence, Grand National Assembly debate transcripts, and the enforcement design (compliance windows, criminalization scope, absence of transitional bridging institutions) against comparable script reforms elsewhere (e.g. Vietnamese quoc ngu, Soviet Central Asian Latinization) that pursued literacy gains with less punitive discontinuity.',
    'If incidental, this story''s authored ε is too high relative to the historical record and the modernization_reading''s account becomes the better-supported single constraint; if deliberate and coequal, the rupture_reading''s high ε and tangled_rope classification are the historically accurate reading and continuity_reading''s near-snare framing gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_incidental_severance, empirical, 'Whether cultural rupture was a designed goal or a foreseeable byproduct of phonetic reform.').

omega_variable(
    kernel_referent_stability,
    'Is ''the 1928 script reform'' a single event that different observers read differently, or does the rupture_reading''s referent (the punitive, compressed-enforcement arrangement) differ enough from the modernization_reading''s referent (the phonetic standardization arrangement) that they are not, in fact, describing the same standing arrangement at all?',
    'This is the CS-framing under-determination check: compare whether the enforcement mechanism (criminalization, timeline compression, institutional closure) is treated as constitutive of the constraint under each reading or as a separable implementation detail layered onto a shared core reform.',
    'If the enforcement apparatus is separable, all three readings could in principle converge on a shared factual core with only evaluative disagreement (weakening the case for three distinct constraints); if the enforcement apparatus is constitutive to how each reading defines ''the reform,'' the three-way decomposition into separate constraints is structurally required, as done here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_referent_stability, conceptual, 'Whether the three kernel readings share one referent event or differ in what they take the constraint itself to be.').

omega_variable(
    counterfactual_continuity_feasibility,
    'Was a transitional, dual-script, or bridged approach (preserving Ottoman-script literacy for one or two more generations while introducing Latin script for new administration) technically and politically feasible in 1928, or would any script reform under the Kemalist state have required comparable rupture given the regime''s broader secularization goals?',
    'Comparative case study against jurisdictions that ran dual-script transitional periods (e.g. some post-Soviet Central Asian states'' multi-decade Cyrillic-to-Latin transitions) to assess whether feasibility, not merely intent, explains the compressed Turkish timeline.',
    'If a bridged transition was feasible and rejected, this strengthens the deliberate-rupture reading and the high suppression/extraction values; if no feasible bridged alternative existed given the regime''s other commitments, the rupture reading''s victim-harm framing may overstate what was avoidable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_continuity_feasibility, conceptual, 'Whether a less rupturing transition path was genuinely available.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(orth_tr_t1934, orthographic_kernel__rupture_reading, theater_ratio, 1934, 0.15).
narrative_ontology:measurement(orth_tr_t1940, orthographic_kernel__rupture_reading, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(orth_tr_t1946, orthographic_kernel__rupture_reading, theater_ratio, 1946, 0.2).
narrative_ontology:measurement(orth_tr_t1953, orthographic_kernel__rupture_reading, theater_ratio, 1953, 0.2).
narrative_ontology:measurement(orth_tr_t1960, orthographic_kernel__rupture_reading, theater_ratio, 1960, 0.2).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.62).
narrative_ontology:measurement(orth_be_t1934, orthographic_kernel__rupture_reading, base_extractiveness, 1934, 0.78).
narrative_ontology:measurement(orth_be_t1940, orthographic_kernel__rupture_reading, base_extractiveness, 1940, 0.85).
narrative_ontology:measurement(orth_be_t1946, orthographic_kernel__rupture_reading, base_extractiveness, 1946, 0.87).
narrative_ontology:measurement(orth_be_t1953, orthographic_kernel__rupture_reading, base_extractiveness, 1953, 0.87).
narrative_ontology:measurement(orth_be_t1960, orthographic_kernel__rupture_reading, base_extractiveness, 1960, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orth_su_t1934, orthographic_kernel__rupture_reading, suppression_requirement, 1934, 0.9).
narrative_ontology:measurement(orth_su_t1940, orthographic_kernel__rupture_reading, suppression_requirement, 1940, 0.82).
narrative_ontology:measurement(orth_su_t1946, orthographic_kernel__rupture_reading, suppression_requirement, 1946, 0.75).
narrative_ontology:measurement(orth_su_t1953, orthographic_kernel__rupture_reading, suppression_requirement, 1953, 0.68).
narrative_ontology:measurement(orth_su_t1960, orthographic_kernel__rupture_reading, suppression_requirement, 1960, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the 1928 Turkish script reform' per the ε-invariance principle: continuity_reading (near-snare from the Ottoman-Islamic continuity standpoint), modernization_reading (near-rope from the technical-literacy standpoint), and this rupture_reading (tangled_rope, holding both a genuine coordination function and deliberate, enforced extraction of continuity as coequal facts). All three share the same historical event but author structurally different ε, beneficiary/victim sets, and classifications because each reading takes a different arrangement to be the referent under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
