% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Turkish Script Reform as Deliberate Cultural Rupture (Rupture Reading)
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   In November 1928 the Grand National Assembly adopted the Law on Adoption
 *   of Turkish Letters, replacing the Arabic-script Ottoman orthography with
 *   a Latin-based alphabet; within months Arabic-script publication was
 *   barred, the national schools were mobilized, and the state's textual
 *   machinery converted. This file instantiates ONE reading - rupture_reading
 *   - of the contested orthographic_kernel: the claim that the change was
 *   undertaken principally as a deliberate cultural rupture to sever the
 *   Ottoman-Islamic past and constitute a new national identity. The sibling
 *   readings (continuity_reading, modernization_reading) are separate
 *   constraint stories with their own epsilon values, victim sets, and
 *   classifications; per the epsilon-invariance principle nothing about them
 *   is averaged into this file. Epsilon's referent is the standing
 *   arrangement under contest - the mandated Latin regime and its enforcement
 *   history - assessed by this reading's own lights, hence the very high
 *   value: the reading prices the expropriation of an entire literate
 *   civilization's accumulated textual capital. The claimed type and the
 *   metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data. Time mapping: t=0 is the 1928
 *   law, t=30 is approximately 1958, when the transition cohort had aged out
 *   and enforcement had gone vestigial.
 *
 * KEY AGENTS:
 *   - - post_reform_state_apparatus: Agenda-setter and primary beneficiary (institutional/arbitrage) - authors and enforces the mandate, collects interpretive authority over the past
 *   - - kemalist_cultural_elite: Beneficiary (powerful/mobile) - status, platforms, and careers inside the new order
 *   - - ulema_scribal_class: Primary target (moderate/identity_locked) - script-specific cultural capital devalued; exit fused with self-erasure
 *   - - ottoman_literate_general_public: Target (powerless/constrained) - severed from its own written record, retooled under compulsion
 *   - - post_reform_literate_generation: Coordinated payer (powerless/trapped) - inherits the severance completed in its schooling
 *   - - arabic_script_diaspora_press: Excluded voice (moderate/mobile) - keeps the old literacy alive offshore, outside the conversation
 *   - - orthographic_reform_historians: Analytical observer - assembles the record from no seat in the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.86).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.55).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Turkish Script Reform as Deliberate Cultural Rupture (Rupture Reading)").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political linguistics / state formation / commitment systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '2a39c128-b35f-4981-aada-d1f29c486ba5').
narrative_ontology:cs_kernel_codification('2a39c128-b35f-4981-aada-d1f29c486ba5', formalized).
narrative_ontology:cs_authority_grounding('2a39c128-b35f-4981-aada-d1f29c486ba5', extraction).
narrative_ontology:cs_interpretation_layer_present('2a39c128-b35f-4981-aada-d1f29c486ba5').
narrative_ontology:cs_reading_relation('2a39c128-b35f-4981-aada-d1f29c486ba5', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a39c128-b35f-4981-aada-d1f29c486ba5', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('2a39c128-b35f-4981-aada-d1f29c486ba5', foundational, national_identity_requires_past_severance).
narrative_ontology:cs_axiom_status(national_identity_requires_past_severance, holdable).
narrative_ontology:cs_axiom_grounding('2a39c128-b35f-4981-aada-d1f29c486ba5', national_identity_requires_past_severance, instrumental).
narrative_ontology:cs_axiom('2a39c128-b35f-4981-aada-d1f29c486ba5', foundational, script_change_designed_as_civilizational_rupture).
narrative_ontology:cs_axiom_status(script_change_designed_as_civilizational_rupture, holdable).
narrative_ontology:cs_axiom_grounding('2a39c128-b35f-4981-aada-d1f29c486ba5', script_change_designed_as_civilizational_rupture, empirically_contingent).
narrative_ontology:cs_reference_frame('2a39c128-b35f-4981-aada-d1f29c486ba5', completed_civilizational_severance).
narrative_ontology:cs_drift_state('2a39c128-b35f-4981-aada-d1f29c486ba5', contemporary_neo_ottoman_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2a39c128-b35f-4981-aada-d1f29c486ba5', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_cultural_elite).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ulema_scribal_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_literate_generation).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, post_reform_literate_generation).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, kemalist_civilizational_rupture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces the 1928 Alphabet Law, runs the national schools (Millet Mektepleri) that teach the new letters, licenses printing and publishing, and staffs the courts and ministries that operate only in the new script. Its members retrained first and filled the new teaching and administrative posts. Reversing the arrangement would dissolve the credentialing and interpretive machinery it operates; continuing it costs little now that generations have been schooled inside it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_state_apparatus, beneficiary).

% Journalists, language-commission members, and intellectuals who campaigned for the change and went on to staff the new language and history institutions. Their authority, publishing platforms, and careers are bound to the new order's founding acts; they teach, edit, and adjudicate the national language question from inside the winning coalition.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_cultural_elite, beneficiary,
    powerful, biographical, mobile, national).

% Religious scholars, court scribes, calligraphers, and poets formed in the Arabic-script Ottoman tradition. Their schooling, livelihoods, and standing rested on a literacy the state ceased to recognize; the courts, schools, and presses that sustained their trade closed or converted within a few years. Remaining in the old literacy means shrinking into private observance; crossing into the new one means surrendering the authority their learning conferred. Their scholarly self-concept is constituted through transmission of the Arabic-script corpus, so exit carries the cost of self-erasure, not merely retraining.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ulema_scribal_class, payer,
    moderate, generational, identity_locked, national).

% Ordinary literate men and women - merchants, clerks, teachers, households with libraries and correspondence - who within a few seasons could no longer read the books, ledgers, letters, and gravestones of their own past. Most learned the new letters in evening national-school courses in order to keep functioning; the old library stayed on the shelf. There was no institutional channel through which to keep the old literacy alive.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_literate_general_public, payer,
    powerless, biographical, constrained, national).

% Children schooled entirely in the new alphabet from the 1930s onward. They acquire cheap, standardized literacy and full access to the new print sphere, but no path to the written record of their grandparents; the severance is completed in their schooling before they are old enough to hold a view about it, and leaving the new script would mean forfeiting literacy altogether.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_literate_generation, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_literate_generation, beneficiary).

% Publishers and journals operating from Cairo, Damascus, and elsewhere that continued printing Ottoman-script Turkish for emigre and conservative readers. Outside the republic's jurisdiction, they kept the old literacy commercially alive but had no seat in the domestic conversation about the alphabet and no access to the domestic market the law closed.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, arabic_script_diaspora_press, excluded,
    moderate, biographical, mobile, continental).

% Scholars of comparative script reform and Turkish history who assemble the archival record - commission minutes, parliamentary debate, circulation figures, literacy statistics - and can set the Turkish case beside Soviet latinization and other alphabet politics. They bear no costs and collect no benefits from the arrangement.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, orthographic_reform_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single alphabet for schooling, administration, printing, and telegraphy across a population of mixed script traditions, compatible with international typographic and telecommunications standards, together with a mass pipeline of national schools for teaching it.
% TRANSFER_FUNCTION: Moves interpretive authority over the written past from holders of Arabic-script literacy to the state's schools, archives, and certified editions; moves the pre-1928 corpus out of reach of new readers; moves status and employment in the literate professions to those who convert first.
% ABSENT_VOICES: The diaspora Arabic-script press, the surviving Ottoman literati outside the ruling party, the largely illiterate rural majority who would bear the schooling burden, and non-Turkish Muslim language communities subject to the same alphabet politics - none held a seat in the 1928 decision, which passed under single-party discipline.
% DISAPPEARANCE_RATIONALE: If the Latin mandate vanished overnight, Turkish textual life would bifurcate: schools, courts, and presses would face an immediate script decision, religious and conservative networks would revive Arabic-script instruction, the Ottoman corpus would flood back into circulation through reprinting, and the state's monopoly on certified literacy would erode within a decade.
% FOUNDING_PROBLEM: The republic's founders confronted a population whose literate culture anchored loyalty to the Ottoman-Islamic order; the arrangement was built to dissolve that anchor by rendering the old written heritage unreadable to new readers and installing a national identity oriented to the new state.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: contemporaneous foreign diplomatic reporting and press coverage described the change as a planned break with the Islamic-Ottoman past rather than a mere technical substitution; memoirs and testimony of displaced Ottoman literati record the severance from the losing side; the relocation of Arabic-script Turkish publishing to Cairo is corroborated by the Egyptian imprint record. The state's own commission minutes and ministry circulars attest the design but issue from the benefiting seat.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is very high (0.86 at interval end) because the arrangement's core operation was a one-way conversion of cultural capital: the pre-1928 corpus - religious, literary, legal, familial - became inaccessible to new readers, and the loss cannot be recovered by the people who paid it. The loss was front-loaded (series peaks at 0.88 during the enforcement hump) and then persists as standing severance rather than decaying, because each year the arrangement continues is another year the old corpus recedes beyond reach. Suppression (scalar 0.55) is authored as the standing structural closure: active coercion faded after the 1930s, but the alternatives never reopened - no institutional channel exists for Arabic-script Turkish in schooling, publishing, or administration. The suppression_requirement series separately traces enforcement intensity, which humped (0.55 to 0.82 by t=6, driven by publication bans, mandatory national schools, and inspection) and then decayed (to 0.35) as compliance became self-sustaining; the story tracks enforcement-capacity change, so the series is warranted alongside the scalar. Theater is low throughout (ending 0.14): whatever one thinks of the project, the teaching, publishing, and administration were functionally serious; the ceremonial element (the founder's chalk tours, the letter feasts) peaked modestly around t=6 and shrank as routine took over. Accessibility_collapse is 0.72 - alternatives collapsed far more completely than in an ordinary policy fight (dual-track and gradualist proposals were rejected outright, dissident publishing moved offshore) but short of natural-law totality, since diaspora printing, private use, and later academic paleography survived. Resistance is 0.5: real objection existed in parliament and the press, and passive resistance showed as slow uptake among older cohorts, but organized opposition had been dismantled in sequence - the caliphate abolished in 1924 and the lodges closed in 1925 - before the alphabet law arrived, so the potential victim coalition never had an organizational vehicle. The measurement series run on one shared time grid (t = 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute a coordination-heavy type: from inside the apparatus the arrangement is a founding achievement that now runs as settled infrastructure requiring no effort. The payer seats should compute extraction-heavy types: from the ulema and the general public the same structure operated as confiscation of a literacy they owned. The two principal target seats sit at the same nominal powerlessness but differ in exit texture, and the engine should register it: the ulema's capital was script-specific AND identity-fused (exit equals self-erasure, so they sit nearer the full-target end), while merchants and clerks could retool skills without renouncing themselves (constrained, slightly less trapped). The post-reform generation computes nearest the middle - they received real functional literacy while being severed without consent - and the diaspora press, holding its practice outside jurisdiction, sits toward the beneficiary end despite formal exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the state apparatus collects interpretive authority, credentialing monopoly, and staffing of the new literate professions (d near the beneficiary end); the cultural elite collects status and platforms (low d). Victims: the ulema-scribal class and the Ottoman-literate public bear the transfer (high d, amplified by identity-lock and constrained exit respectively). The post-reform generation is genuinely dual - payer through severed inheritance, beneficiary through cheap literacy - and the derivation from its dual role and trapped exit should place it mid-to-high. The diaspora press derives low-to-mid d: it loses the domestic market but keeps its practice. No directionality_overrides are authored: the override mechanism keys on power atom alone, and this story has two powerless seats (general public, post-reform generation) that need DIFFERENT d values - an override at the powerless atom would corrupt one of them. The role-plus-exit derivation separates them correctly without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - active severance of the Ottoman-Islamic anchor - is dead: the rupture was completed by roughly 1950, and no living project maintains it. Yet the arrangement persists and the world remains rearranged around it, so the R5 mismatch signature (status=dead x verdict=world_rearranges) fires, and here it is CORRECT rather than pathological: the arrangement persists not because anyone performs the founding task but because reversal is prohibitive (fixing_cost) and the new textuality is load-bearing for an entire society. The classification prevents mislabeling in both directions: reading the arrangement as pure extraction misses the real coordination delivered (mass literacy, script uniformity, typographic modernity); reading it as pure coordination misses the confiscation that paid for it. The tangled-rope claim holds both truths in one structure. Note also what the low theater ratio excludes: this is not a piton drifting into performance - the arrangement's remaining function (the national orthography itself) is fully operational; what atrophied was only the enforcement project, which atrophied because it succeeded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the essential structure of the 1928 arrangement best captured by the rupture reading, or do the continuity and modernization readings capture it?',
    'Comparative reading-by-reading compilation: generate the sibling stories and test each reading''s structural predictions (who is harmed, when enforcement peaks, what companion measures accompany the change) against the archival record; the reading whose predicted structure best fits earns presumptive status.',
    'Adopting continuity_reading would shrink the victim set to tradition-bearers and reprice epsilon around destroyed continuity; adopting modernization_reading would recast the payers as transition-cost bearers and likely yield a rope-class profile. This file''s very-high-epsilon, mass-victim structure holds only under the rupture reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the orthographic kernel correctly characterizes the arrangement''s essential structure.').

omega_variable(
    victim_set_intergenerational_boundary,
    'Does the victim set extend beyond the pre-reform literate population to their descendants and to generations schooled into the severance without consent?',
    'Boundary analysis of harm transmission: trace whether measurable harms (family-archive inaccessibility, document loss, exclusion from the heritage record) attach to persons born after the transition, versus terminating with the cohorts who personally held the old literacy.',
    'Extending the victim set raises effective extraction for the descendant seat and strengthens the snare-flavored tail of the classification; bounding it confines the high-directionality seats to the transitional cohorts and keeps the post-reform generation mid-scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_intergenerational_boundary, conceptual, 'Whether intergenerational harm counts inside the victim set or terminates with the pre-reform literate population.').

omega_variable(
    rupture_motive_primacy,
    'Was civilizational severance the operative design goal of the reform, or an accepted side-effect of a modernization imperative that nearly any alphabet change would have produced?',
    'Archival sequence analysis: commission minutes, private correspondence, and the timing of companion measures (abolition of calligraphy instruction, dropping of Arabic and Persian courses, restrictions on religious publishing) - a reform motivated only by efficiency would not require the accompanying cultural closures.',
    'If rupture was primary, the coordination share shrinks and the arrangement trends toward pure extraction; if secondary, the coordination component carries more weight and the tangled-rope reading firms up against the snare alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_motive_primacy, empirical, 'Whether the rupture function was the design goal or a side-effect of modernization.').

omega_variable(
    post_enforcement_persistence_basis,
    'After active enforcement decayed (post-1950), does the arrangement persist by settled preference, by irreversibility of invested textuality, or by inertial drift toward performance?',
    'Revealed-preference study: demand for Ottoman-script instruction when offered electively, usage of digitized Ottoman archives, and whether restoration proposals attract organizing coalitions or only antiquarian curiosity.',
    'Inertial persistence would push late-interval classification toward degraded, inertia-maintained territory; preference-based persistence stabilizes the tangled-rope reading; irreversibility-based persistence keeps the extraction historically real but currently unmaintained by anyone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_enforcement_persistence_basis, empirical, 'What sustains the arrangement now that enforcement has gone vestigial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ok_rupture_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ok_rupture_tr_t0, observed).
narrative_ontology:measurement(ok_rupture_tr_t6, orthographic_kernel__rupture_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(ok_rupture_tr_t6, observed).
narrative_ontology:measurement(ok_rupture_tr_t12, orthographic_kernel__rupture_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(ok_rupture_tr_t12, observed).
narrative_ontology:measurement(ok_rupture_tr_t18, orthographic_kernel__rupture_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(ok_rupture_tr_t18, observed).
narrative_ontology:measurement(ok_rupture_tr_t24, orthographic_kernel__rupture_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(ok_rupture_tr_t24, observed).
narrative_ontology:measurement(ok_rupture_tr_t30, orthographic_kernel__rupture_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(ok_rupture_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(ok_rupture_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement_basis(ok_rupture_be_t0, observed).
narrative_ontology:measurement(ok_rupture_be_t6, orthographic_kernel__rupture_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement_basis(ok_rupture_be_t6, observed).
narrative_ontology:measurement(ok_rupture_be_t12, orthographic_kernel__rupture_reading, base_extractiveness, 12, 0.87).
narrative_ontology:measurement_basis(ok_rupture_be_t12, observed).
narrative_ontology:measurement(ok_rupture_be_t18, orthographic_kernel__rupture_reading, base_extractiveness, 18, 0.86).
narrative_ontology:measurement_basis(ok_rupture_be_t18, observed).
narrative_ontology:measurement(ok_rupture_be_t24, orthographic_kernel__rupture_reading, base_extractiveness, 24, 0.86).
narrative_ontology:measurement_basis(ok_rupture_be_t24, observed).
narrative_ontology:measurement(ok_rupture_be_t30, orthographic_kernel__rupture_reading, base_extractiveness, 30, 0.86).
narrative_ontology:measurement_basis(ok_rupture_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(ok_rupture_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(ok_rupture_su_t0, observed).
narrative_ontology:measurement(ok_rupture_su_t6, orthographic_kernel__rupture_reading, suppression_requirement, 6, 0.82).
narrative_ontology:measurement_basis(ok_rupture_su_t6, observed).
narrative_ontology:measurement(ok_rupture_su_t12, orthographic_kernel__rupture_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement_basis(ok_rupture_su_t12, observed).
narrative_ontology:measurement(ok_rupture_su_t18, orthographic_kernel__rupture_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(ok_rupture_su_t18, observed).
narrative_ontology:measurement(ok_rupture_su_t24, orthographic_kernel__rupture_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(ok_rupture_su_t24, observed).
narrative_ontology:measurement(ok_rupture_su_t30, orthographic_kernel__rupture_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement_basis(ok_rupture_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, modernization_reading).

% DUAL FORMULATION NOTE:
% One colloquial label - 'the 1928 Turkish script reform' - covers three structurally distinct readings of a single kernel, decomposed per the epsilon-invariance principle. All three share a referent (the standing arrangement) but assign different epsilon and different victim/beneficiary structures: continuity_reading prices the destroyed continuity of the Arabic-script tradition; modernization_reading nets coordination gains against transition costs; rupture_reading (this file) prices the deliberate expropriation of an entire literate population's textual capital for the benefit of the post-reform state apparatus. The upstream empirical record (what happened, in what sequence, with what enforcement) is common to all three; the downstream classifications diverge on motive and valuation. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
