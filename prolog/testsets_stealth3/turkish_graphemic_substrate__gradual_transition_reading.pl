% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the gradual_transition_reading — of
 *   the contested kernel turkish_graphemic_substrate: the claim that both
 *   scripts can legally coexist under state management for a bounded
 *   five-to-fifteen-year window, preserving intergenerational knowledge
 *   transfer while the Latin orthography stabilizes. Per the epsilon-referent
 *   rule for kernel readings, epsilon's referent is THIS standing arrangement
 *   (the managed dual-script transition itself), assessed honestly by its own
 *   lights — not the abrupt regime it competes with and not the terminal
 *   Latin regime its sunset converges toward. The claim/metrics independence
 *   rule is respected deliberately: the arrangement is CLAIMED as scaffold
 *   (its justification is the transition, and a dated terminus is
 *   constitutive of the reading), while the authored metrics describe real
 *   duplication costs, real enforcement, and moderate resistance — the engine
 *   computes each seat's type from the structural data, and any divergence
 *   between claim and computation is the measurement the corpus exists to
 *   take. ASSUMPTIONS STATED: (1) the reading's arrangement is treated as the
 *   arrangement under authorship even though Turkey's actual 1928 reform took
 *   the abrupt form — this file assesses the policy design, not the enacted
 *   history; (2) measurement values are projections calibrated against
 *   analogous implemented transitions (Soviet-era alphabet rotations, later
 *   Cyrillic-to-Latin timetables) and are marked basis=projected accordingly;
 *   (3) one reading, one epsilon — the sibling readings are separate
 *   constraint files linked through network.affects_constraints, per the
 *   decomposition of the colloquial label 'Turkish script reform' into three
 *   structurally distinct arrangements.
 *
 * KEY AGENTS:
 *   - script_transition_directorate: agenda-setter (institutional/mobile) — administers dual standards and owns the sunset clock
 *   - ottoman_literate_older_cohorts: primary beneficiary (organized/identity_locked) — keeps full civic legibility during the window, ages out with it
 *   - islamic_textual_institutions: beneficiary with scheduled terminal loss (organized/identity_locked) — present circulation, dated medium
 *   - state_record_keepers: beneficiary (institutional/mobile) — archives stay internally legible while conversion proceeds
 *   - transitional_print_sector: dual-positioned collector (powerful/arbitrage) — wins the duplication demand, carries duplicate stock
 *   - dual_curriculum_schoolchildren: primary target (powerless/trapped) — pays the double orthographic load, inherits the double archive
 *   - provincial_fiscal_offices: target (moderate/constrained) — funds duplication on local budgets without pacing discretion
 *   - rapid_homogenization_advocates: opportunity-cost payer (powerful/constrained) — serves inside the coalition, pays in delayed victory
 *   - rural_domestic_literacy_networks: excluded voice (powerless/trapped) — carries everyday Arabic-script literacy, never consulted
 *   - comparative_script_reform_analysts: analytical observer (analytical/analytical) — compares against parallel reforms, answers to no participant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.38).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.28).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Managed Dual-Script Transition Regime (Gradual Reading of the Turkish Graphemic Substrate)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3').
narrative_ontology:cs_kernel_codification('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', formalized).
narrative_ontology:cs_authority_grounding('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', expertise).
narrative_ontology:cs_interpretation_layer_present('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3').
narrative_ontology:cs_reading_relation('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', foundational, transition_requires_intergenerational_bridge).
narrative_ontology:cs_axiom_status(transition_requires_intergenerational_bridge, holdable).
narrative_ontology:cs_axiom_grounding('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', transition_requires_intergenerational_bridge, instrumental).
narrative_ontology:cs_axiom('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', secondary, archive_access_is_non_negotiable_during_changeover).
narrative_ontology:cs_axiom_status(archive_access_is_non_negotiable_during_changeover, holdable).
narrative_ontology:cs_axiom_grounding('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', archive_access_is_non_negotiable_during_changeover, deontological).
narrative_ontology:cs_reference_frame('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', ordered_dual_legitimacy_passage).
narrative_ontology:cs_drift_state('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', post_comparative_alphabet_reform_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8ddd96ab-52c3-42b7-80f5-3285ddf3f0a3', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, ottoman_literate_older_cohorts).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, islamic_textual_institutions).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, state_record_keepers).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, transitional_print_sector).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, dual_curriculum_schoolchildren).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, provincial_fiscal_offices).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, rapid_homogenization_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, islamic_textual_institutions).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, transitional_print_sector).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, managed_transition_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_knowledge_transfer_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the dual-curriculum standard, certifies transitional orthography conventions, publishes the phased conversion timetable, and holds sole authority to grant or deny extension requests as the terminus approaches. Staffed by appointees seconded from the education and interior ministries; the office itself is scheduled to dissolve when the timetable completes.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, script_transition_directorate, agenda_setter,
    institutional, generational, mobile, national).

% Officials trained before the reform, merchant families keeping ledger books, clerics, poets, and household literates whose entire reading life is in Arabic-script Ottoman. During the window their letters, deeds, newspapers, and prayer books remain usable in courts, offices, and shops; they grow old as the window closes, and few retrain late in life.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_literate_older_cohorts, beneficiary,
    organized, biographical, identity_locked, national).

% Medrese faculties, Sufi lodge libraries, waqf endowments, calligraphers' workshops, and Qur'an commentary schools whose canon exists only in Arabic letterforms. The window keeps their libraries circulating and their students enrolled; the published timetable tells them their operating medium has a closing date, and their trans-regional correspondents keep them tied to a wider script world that is not switching.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, islamic_textual_institutions, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, islamic_textual_institutions, payer).

% Registry offices, land cadastres, court archives, and census bureaus whose paper inheritance is Arabic-script. Phased conversion lets them copy, transliterate, and cross-index holdings while new Latin records accumulate alongside, instead of facing a filing system half of which becomes illegible to incoming clerks overnight.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_record_keepers, beneficiary,
    institutional, generational, mobile, national).

% Newspaper proprietors, type foundries, and textbook houses that take in the doubled demand of the changeover — parallel editions, retrained compositors, government printing contracts — while carrying duplicate type stock and split production runs. Capital sits lightly on either alphabet and moves to whichever line the contract schedule favors next.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, transitional_print_sector, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, transitional_print_sector, payer).

% Pupils who learn reading and writing twice — Ottoman letterforms for the texts their grandparents hand them, Latin letterforms for the examinations and the state's planned future. Classroom hours spent on two orthographies come out of arithmetic, geography, and play; the pupils themselves sit outside every body that set the schedule.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, dual_curriculum_schoolchildren, payer,
    powerless, biographical, trapped, national).

% Municipal treasuries, county education boards, and town councils outside the capital that fund duplicated signage, stationery, form stocks, and teacher retraining out of thin local budgets, following templates issued from the center with little discretion over pacing.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, provincial_fiscal_offices, payer,
    moderate, immediate, constrained, regional).

% Republican cadre, Gazette-circle journalists, and cultural-house organizers who wanted a single alphabet immediately and read every year of coexistence as another year the old civilization keeps a foothold in schools, courts, and street signs. They serve inside the governing coalition and pay in patience rather than money; breaking ranks costs them their posts.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rapid_homogenization_advocates, payer,
    powerful, generational, constrained, national).

% Women's Qur'an study circles, family-correspondence habits, and village letter-writers who sustain most of the country's day-to-day Arabic-script literacy without appearing in any ministry's consultation. Neither the directorate nor the language council solicited them; their objection — that policy treats literacy as something administered in classrooms — was never entered into the record.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rural_domestic_literacy_networks, excluded,
    powerless, generational, trapped, national).

% Linguists and historians of writing-system change who compare this timetable against the Soviet republics' alphabet rotations, enrollment data from the early reform years, and later Central Asian conversion delays; they publish assessments that no participant is obliged to answer.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, comparative_script_reform_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, transitional_print_sector).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences a society-wide change of writing system so that the pre-reform archive stays readable, existing literates stay economically functional, and the new orthography stabilizes in schools before it becomes the sole legal medium — spreading conversion work across fifteen years instead of compressing it into months.
% TRANSFER_FUNCTION: Moves duplicated spending (parallel printing, double curricula, retraining stipends, bilingual administration) from taxpayers, school hours, and publisher balance sheets into the transitional print and teaching economy, and moves time from impatient modernizers to aging Arabic-script readers.
% ABSENT_VOICES: Rural domestic literacy networks — the Qur'an circles, family-letter writers, and village scribes who carried most everyday Arabic-script literacy — were never consulted; village teachers implementing the dual curriculum had advisory channels only; and Arabic-script users outside the Turkish core who shared the region's typographic infrastructure held no seat in a national Turkish decision.
% DISAPPEARANCE_RATIONALE: If the managed dual-script arrangement vanished overnight, the choice collapses to its two rivals — immediate single-script decree or no reform at all — and everything sequenced around the timetable rearranges: school curricula, printing contracts, archive-conversion schedules, the retirement expectations of Arabic-script clerks, and the legislative calendar built on a dated terminus.
% FOUNDING_PROBLEM: A newly centralized nation-state inherited an empire's writing system: Arabic letterforms poorly matched to Turkish phonology, an administration and a commercial world run in Ottoman script, and a single living generation embodying that literacy. Mass literacy campaigns, republican legal codes, and European-facing modernity all pointed toward a phonetic Latin alphabet; the archive, the aged, and the mosque pointed the other way. The arrangement was designed to solve: change the substrate without cutting off either the archive or the people who can still read it.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the minutes and minority memoranda of the 1926 Language Council deliberations record the rupture-versus-bridge dispute as live at founding; European educational missions' reports of the period assessed the literacy logistics independently; and later comparative scholarship on Soviet-era alphabet changes treats the Turkish case as the abrupt pole against which phased designs are measured. No attesting source stands to gain from the transitional economy itself.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).
:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38: the arrangement moves real resources — duplicated printing, doubled curricula, retraining stipends — but the costs are diffuse public costs rather than concentrated rents, and the sunset bounds them; no seat collects a rate-like toll on ongoing activity, which is why epsilon sits well below snare/hybrid territory despite visible transfers. Suppression 0.28: enforcement is real (mandatory dual curricula, certified orthography conventions, procurement rules binding provincial offices) but far lighter than prohibition regimes — remaining in Arabic-script usage stays lawful throughout the window, unlike the enacted history's rapid suppression of the old grapheme. Theater 0.18: most dual activity is functional (parallel editions, bilingual forms, archive copying); a ceremonial residue accumulates near the terminus. Accessibility_collapse 0.48: the arrangement deliberately keeps its rivals open during the window — that openness is its point — and closes them only at sunset, so understanding the arrangement collapses alternatives only partially. Resistance 0.42: traditionalists contest the terminus, fiscal voices contest the cost, radical modernizers contest the pace. Coordination typing: the dominant function is allocating conversion effort, cost, and time across sectors and cohorts, hence resource_allocation with its default floor; no floor override is warranted. MEASUREMENT GRID DISCIPLINE: all three tracked metrics are authored at all six shared time points (0,3,6,9,12,15) on one grid; every point is basis=projected. Trajectory drivers: extraction starts high on setup duplication, eases as dual pipelines normalize, bumps at t=9 on the sunset scramble (laggards converting late, stranded transitional stock), and declines as the apparatus winds down; theater creeps upward as some bilingualism turns ceremonial; suppression humps mid-window when dual-standard enforcement bites hardest, then settles at 0.28 because the final phase's suppressive work is defending the terminus itself — refusing the extension requests that print-sector revenue and institutional circulation depend on.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the directorate and the record-keeping offices, the arrangement is orderly administration they run and staff. From the schoolchild and provincial-fiscal seats, it is a doubled burden imposed without their consent. From the print sector, it is a contracted windfall. From the homogenization advocates, it is a slow-motion betrayal conducted from inside their own coalition. Same-power lateral differentiation is sharp: transitional_print_sector and rapid_homogenization_advocates hold equal nominal power (both 'powerful') yet sit at opposite ends of the arrangement's ledger — the difference is carried by exit structure (capital arbitrage versus coalition lock-in) and horizon (business-cycle biographical versus nation-building generational), which is exactly the constraint-specific factor the per-seat computation should register.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation: declared beneficiaries (older cohorts, record keepers) derive d near the subsidy end; declared payers (schoolchildren, provincial offices) derive d near the full-target end, amplified for the trapped child seat and damped for nobody here. TWO OVERRIDES are declared where the mechanical derivation would err. First, islamic_textual_institutions derives as a strong beneficiary (present circulation, organized, identity-locked) but the published timetable schedules their operating medium's closure — they collect the window and finance the terminus with their own obsolescence, so their true structural position is mid-scale: override d to 0.45. Second, dual_curriculum_schoolchildren derives near-full-target (powerless, trapped, bearing the double load), but they alone receive the arrangement's principal bequest — functional access to both archives across their adult lives — which offsets a real share of their childhood payment: override d to 0.75. Suppression is authored as a raw structural property and is NOT scaled; only extractiveness is scaled by the engine through directionality and national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming scaffold guards against two symmetric mislabelings. Without the sunset declaration, the duplication costs read as open-ended extraction leaning snareward; without the enforcement declaration, the arrangement masquerades as voluntary custom that needs no administrator. The anti-mandatrophy device is built into the arrangement itself: the directorate's dissolution clause and the dated terminus mean the founding problem (bridging the rupture without amputating the archive or the aged) is DESIGNED to resolve at t<=15. The R5 mismatch wiring watches exactly the failure mode this reading fears: if the window is extended past function, founding_problem_status flips dead while the world still rearranges around the extended apparatus — status=dead plus world_rearranges flags the zombie extension, cross-checked against the computed theater path. Authoring status as 'contested' rather than 'dead' reflects the genuine dispute: modernizers hold that the preservation-side problem ends at sunset, while continuity partisans hold that it never ends; the corroboration section names attestants outside the benefiting parties so the genealogy is not self-certified. fixing_cost is authored 'cheap' on its own evidence: because cessation is the default path, correcting drift — refusing extensions, dissolving the directorate on schedule — is procedurally light relative to the value of closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gradual_reading_kernel_position,
    'How does instantiating the gradual_transition_reading — rather than the ottoman_continuity_reading or the secular_nationalist_reading — of the turkish_graphemic_substrate kernel change the structural classification?',
    'Compare compiled classifications across the three sibling stories sharing the kernel. The disagreement is located in the temporal structure of the substrate change (whether a sunset terminus exists) and in the legitimacy granted the inherited grapheme.',
    'The continuity sibling removes the terminus entirely — no sunset, no scaffold signature, permanence costs assessed instead. The nationalist sibling removes the bridge — short-run suppression and archive rupture rise sharply. This file''s epsilon, victim set, and claimed scaffold are artifacts of THIS reading only; the foreclosure edge to the continuity sibling follows from the terminus contradicting that sibling''s permanence premise within any single framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gradual_reading_kernel_position, conceptual, 'Committer-frame routing: one reading of a three-way kernel, with sibling deltas recorded.').

omega_variable(
    unimplemented_design_projection,
    'Do the projected measurement trajectories survive contact with implementation, given the arrangement was advocated in the 1926-28 deliberations but never enacted in this form?',
    'Calibrate against implemented phased reforms with genuine dual-script windows (Soviet-era alphabet rotations, later Cyrillic-to-Latin timetables) and against the observed Turkish abrupt-reform record as the contrasting pole.',
    'If real implementations show longer enforcement tails or higher ceremonial residue than projected, the scaffold reading weakens toward an enforced hybrid; if they show faster decay of dual demand, extraction ends materially lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unimplemented_design_projection, empirical, 'All authored time-series values are projections; this omega tracks their calibration debt.').

omega_variable(
    sunset_durability_politics,
    'Does the five-to-fifteen-year terminus hold against extension pressure from the seats whose revenue and circulation depend on continued dualism?',
    'Track extension requests, budget renewals, and directorate-dissolution votes across the window; compare against documented slippage in analogous phased reforms where interim arrangements became semi-permanent.',
    'Indefinite extension strips the sunset that grounds the scaffold claim and converts the arrangement toward an enforced dual regime with rising theater — the piton-adjacent outcome this reading''s own design is supposed to prevent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_durability_politics, empirical, 'Whether the constitutive terminus survives the politics it generates.').

omega_variable(
    dual_literacy_depth_vs_delay,
    'Does maintained dual-script schooling produce functional bireadism with real archive access, or nominal second-script exposure that merely postpones the generational rupture by one cohort?',
    'Cohort testing of pre-reform text comprehension (legal deeds, literary prose, correspondence) at window midpoint versus end, comparing transition cohorts against both the abrupt-reform and no-reform baselines.',
    'If depth fails, the bridge''s principal benefit is illusory and this reading''s epsilon understates the net social loss; if depth holds, the reduced-rupture delta is real and the elevated implementation costs are the price of a genuine good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_literacy_depth_vs_delay, empirical, 'Whether the intergenerational bridge delivers depth or only delay.').

omega_variable(
    kernel_scope_orientation_vs_script,
    'Is the contested kernel the graphemic substrate itself, or the civilizational orientation (Western-facing modernity versus Ottoman-Islamic continuity) that script choice expresses?',
    'Test whether the readings'' disagreement survives holding the script fixed (pure timing disputes) or appears only when the destination script varies; consult which axis the 1926 deliberation record treats as decisive.',
    'Under an orientation-framing, all three readings would share a modernization axiom and differ only on means, weakening the foreclosure edge to the continuity sibling and altering the axiom set; under the script-framing adopted here (following the manifest''s substrate-level tagging), the readings differ on the substrate end-state itself and the foreclosure structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_scope_orientation_vs_script, conceptual, 'CS-framing under-determination: two coherent framings of the same kernel yield different reading relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(turk_tr_t0, projected).
narrative_ontology:measurement(turk_tr_t3, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement_basis(turk_tr_t3, projected).
narrative_ontology:measurement(turk_tr_t6, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(turk_tr_t6, projected).
narrative_ontology:measurement(turk_tr_t9, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 9, 0.13).
narrative_ontology:measurement_basis(turk_tr_t9, projected).
narrative_ontology:measurement(turk_tr_t12, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(turk_tr_t12, projected).
narrative_ontology:measurement(turk_tr_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(turk_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(turk_be_t0, projected).
narrative_ontology:measurement(turk_be_t3, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement_basis(turk_be_t3, projected).
narrative_ontology:measurement(turk_be_t6, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement_basis(turk_be_t6, projected).
narrative_ontology:measurement(turk_be_t9, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement_basis(turk_be_t9, projected).
narrative_ontology:measurement(turk_be_t12, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(turk_be_t12, projected).
narrative_ontology:measurement(turk_be_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(turk_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(turk_su_t0, projected).
narrative_ontology:measurement(turk_su_t3, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 3, 0.28).
narrative_ontology:measurement_basis(turk_su_t3, projected).
narrative_ontology:measurement(turk_su_t6, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(turk_su_t6, projected).
narrative_ontology:measurement(turk_su_t9, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 9, 0.31).
narrative_ontology:measurement_basis(turk_su_t9, projected).
narrative_ontology:measurement(turk_su_t12, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement_basis(turk_su_t12, projected).
narrative_ontology:measurement(turk_su_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(turk_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'Turkish script reform': the label covers three structurally distinct arrangements — abrupt single-script replacement, permanent Arabic-script continuity, and managed dual-script transition — each with its own epsilon, its own beneficiary/victim structure, and its own temporal shape, per the epsilon-invariance principle. This file instantiates the managed-transition member. Upstream/downstream structure: the secular_nationalist sibling is the terminal regime this reading's sunset converges toward (this reading influences that sibling's operating conditions by staging the arrival), while the continuity sibling is the rival pole whose permanence premise this reading's terminus contradicts outright. Every family member links to at least one other; orphan stories would be a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, organized, 0.45).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
