% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Standard Persistence - Naturalization Reading
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the naturalization reading of the QWERTY
 *   persistence kernel: the layout persists because it was, and became,
 *   genuinely adequate, and rival layouts lapsed through fair competition
 *   rather than suppression or rent defense. The referent of epsilon is the
 *   standing arrangement - QWERTY's entrenched default status across
 *   hardware, software, curricula, and hiring - assessed by this reading's
 *   own lights, in which the arrangement charges typists mainly the genuine
 *   cost of acquiring a portable skill. The claim/metric gap is deliberate
 *   and independent: claimed_type is rope because that is this reading's
 *   structural belief (a pure coordination standard, no systematic
 *   beneficiary, no enforcement machinery, alternatives free to compete),
 *   while the metrics are authored as descriptive facts about the actual
 *   arrangement (small but nonzero extraction from minority-choice friction,
 *   near-zero theater, alternatives practically collapsed to niches without
 *   any door being closed). The colloquial label 'why does QWERTY persist'
 *   decomposes, per the epsilon-invariance principle, into this story and its
 *   two linked siblings - see network.dual_formulation_note; this file
 *   handles only the adequacy claim.
 *
 * KEY AGENTS:
 *   - - touch_typists: primary coordination beneficiary (moderate/constrained) - supplies the shared skill the standard coordinates; bears switching cost only as forfeited portability, never as a toll
 *   - - clerical_and_administrative_employers: coordination beneficiary (organized/mobile) - buys labor from the common trained pool; free to retrain internally and occasionally does
 *   - - keyboard_hardware_manufacturers: incidental beneficiary (institutional/arbitrage) - builds to the known spec and ships remappable or alternative layouts on request; per this reading defends nothing
 *   - - alternative_layout_advocates: residual payer seat (moderate/constrained) - voluntarily retrained, bears recurring compatibility friction in mixed environments; their campaigns lost through ordinary competition
 *   - - ansi_iso_standards_committees: agenda_setter (institutional/mobile) - ratifies the de facto default after the fact; enforces nothing and collects no rent
 *   - - economic_historians_and_technology_scholars: analytical observer (analytical/analytical) - produces the contested comparative-trial evidence the adequacy claim stands on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Standard Persistence - Naturalization Reading").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '8ac39a54-ae3e-4fba-b349-e76bb17e654b').
narrative_ontology:cs_kernel_codification('8ac39a54-ae3e-4fba-b349-e76bb17e654b', formalized).
narrative_ontology:cs_authority_grounding('8ac39a54-ae3e-4fba-b349-e76bb17e654b', expertise).
narrative_ontology:cs_interpretation_layer_present('8ac39a54-ae3e-4fba-b349-e76bb17e654b').
narrative_ontology:cs_reading_relation('8ac39a54-ae3e-4fba-b349-e76bb17e654b', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ac39a54-ae3e-4fba-b349-e76bb17e654b', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8ac39a54-ae3e-4fba-b349-e76bb17e654b', foundational, market_selection_tracks_typist_welfare).
narrative_ontology:cs_axiom_status(market_selection_tracks_typist_welfare, holdable).
narrative_ontology:cs_axiom_grounding('8ac39a54-ae3e-4fba-b349-e76bb17e654b', market_selection_tracks_typist_welfare, empirically_contingent).
narrative_ontology:cs_axiom('8ac39a54-ae3e-4fba-b349-e76bb17e654b', foundational, switching_costs_measure_skill_capital_not_barriers).
narrative_ontology:cs_axiom_status(switching_costs_measure_skill_capital_not_barriers, holdable).
narrative_ontology:cs_axiom_grounding('8ac39a54-ae3e-4fba-b349-e76bb17e654b', switching_costs_measure_skill_capital_not_barriers, empirically_contingent).
narrative_ontology:cs_reference_frame('8ac39a54-ae3e-4fba-b349-e76bb17e654b', competitive_market_selection_equilibrium).
narrative_ontology:cs_drift_state('8ac39a54-ae3e-4fba-b349-e76bb17e654b', post_efficiency_critique_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8ac39a54-ae3e-4fba-b349-e76bb17e654b', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, clerical_and_administrative_employers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_hardware_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hundreds of millions of people whose keyboard skill is portable across every machine, employer, and country because everyone learned the same layout. Their training hours bought a durable asset whose value comes from universality. Retraining to another layout is physically feasible and has been done by individuals, but forfeits part of the skill's portability wherever shared equipment is involved.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, touch_typists, beneficiary,
    moderate, biographical, constrained, global).

% Firms and offices that hire from a common pool of already-trained typists instead of running their own training programs. The standard converts recruitment into a commodity transaction. They can retrain staff internally at will and occasionally do for specialized input work; nothing obliges them to keep the default except the cost of diverging from the labor market's skill distribution.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, clerical_and_administrative_employers, beneficiary,
    organized, biographical, mobile, global).

% Build to a known, stable specification that every customer's hands already fit. They print QWERTY legends because that is what buyers' fingers expect, and most operating systems expose one-keystroke remapping to Dvorak or custom layouts, which manufacturers ship without objection. Per this reading they defend nothing: no exclusivity contracts, no suppression of alternative keycaps, no pricing tied to layout loyalty. Their gain is incidental to the standard's universality, not extracted from it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Users who voluntarily retrained to Dvorak or Colemak, typically reporting comfort or speed gains in solo work. They bear recurring friction in mixed environments: unlabeled or mislabeled keycaps, shared terminals that revert to QWERTY, software that assumes the default, colleagues who cannot borrow their machine. They chose this cost themselves; no actor imposes it. Their commercial and advocacy campaigns (1930s onward) repeatedly failed to win market share through ordinary persuasion and product competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_advocates, payer,
    moderate, biographical, constrained, regional).

% Committees that ratified the already-dominant layout as a formal standard and separately published alternative layouts as sanctioned options. Ratification followed the installed base rather than creating it; the committees administer paperwork around a de facto equilibrium and enforce nothing. They could in principle bless a different default tomorrow; the installed base, not the committee, is what would resist.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, ansi_iso_standards_committees, agenda_setter,
    institutional, generational, mobile, global).

% The research community that produced the contested evidence this reading stands on: ergonomic trials of alternative layouts, the wartime Navy studies, Strong's 1956 retraining experiment, and the subsequent efficiency-versus-path-dependence debate. They take no rent from the standard and adjudicate between the adequacy account and its rivals purely through publication and replication.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, economic_historians_and_technology_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__naturalization_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__naturalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single dominant letter layout lets any trained typist operate any machine, lets employers hire from a pre-trained common pool, and lets hardware makers build to a known specification - the interface convention is solved once, socially, instead of per workplace or per device.
% TRANSFER_FUNCTION: Essentially no ongoing material transfer. Individual training hours are converted into a portable skill whose value depends on everyone else holding the same skill; the only thing that moves is the initial learning investment, paid once by each typist to their own future productivity.
% ABSENT_VOICES: Rival-layout designers and speed-typing contenders of the typewriter era, whose inventions were evaluated under vendor-sponsored and wartime conditions they never controlled, and injured typists whose repetitive-strain outcomes motivated ergonomic alternatives. Both groups would press for rigorous comparative trials; they sit outside the standard-setting conversation, which ratifies rather than deliberates.
% DISAPPEARANCE_RATIONALE: Muscle memory in hundreds of millions of hands, printed keycap legends, touch-typing curricula, hiring screens, and keyboard hardware tooling all presuppose the layout. Overnight removal would force simultaneous retraining and relabeling worldwide until coordination converged on a successor; the disruption is enormous even though nobody enforces the arrangement.
% FOUNDING_PROBLEM: Sholes's 1870s typebar jamming: adjacent frequently-struck bars collided and locked during fast sequences, so separating common letter pairs reduced collisions in mechanical typewriters.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: engineering histories establish that electric typewriters and direct-print mechanisms eliminated typebar collision entirely decades ago, so no living party faces the founding problem; economic historians (the Liebowitz-Margolis line) and the ergonomic-measurement literature independently attest that the layout's persistence now rests on skill-coordination value rather than jam avoidance. No beneficiary party continues to assert the jam problem is live.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low and nearly flat across the interval (0.13-0.20 band) because the standing arrangement's charge to its users is dominated by the one-time, genuine cost of acquiring a portable skill, not an ongoing toll; the mild late-interval rise models deepening common-pool dependence as global labor markets grew, capped by the viability of niche layouts (free OS remapping since the mid-1980s) which prevents further accumulation. Suppression is low (0.12): alternatives were never barred - Dvorak was commercially marketed from the 1930s, standards bodies published it as a sanctioned option, and modern systems expose free remapping - they simply lost repeated fair contests. Theater is near-zero throughout (rising 0.04 to 0.10) because almost nothing performs maintenance of the standard: no enforcement rituals exist, only after-the-fact ratifications; the small rise tracks ritualized efficiency-discourse (periodic 'your keyboard is suboptimal' commentary that changes nothing). Accessibility_collapse is moderate-high (0.62): once the installed base formed, practical alternatives collapsed to hobbyist remappings - not because any actor closed doors, but because the coordination value drained out of minority layouts. Resistance is low-moderate (0.25), reflecting episodic advocacy waves rather than sustained opposition. The measurement series run on one shared eight-point grid with every tracked metric authored at every point; a suppression_requirement series is deliberately omitted because the enforcement picture is static across the whole interval (nothing enforces; ratification follows practice) and the scalar captures it. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats experience the constraint as invisible infrastructure - typists hold a skill they would want under any layout, employers face a labor market that arrives pre-trained, manufacturers build to a spec that predates their product decisions. The advocate seat experiences recurring friction: shared terminals reverting to the default, unlabeled keycaps, interview assumptions. The observer seat sees the same arrangement as the canonical test case separating efficient-selection from path-dependence paradigms. Same phenomenon, three different lived structures; the engine computes the divergence from the structural data rather than this prose adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the subsidy end of directionality: touch_typists, clerical_and_administrative_employers, and keyboard_hardware_manufacturers all derive net benefit from universality, with manufacturers additionally holding arbitrage-grade exit (they ship any layout the market demands). The advocate seat is the only meaningful target: alternative_layout_advocates bear the compatibility costs of minority choice, giving them elevated d - but the costs are self-chosen and no actor enforces them, so effective extraction lands near the floor. Receipt surface affirmatively checked: no named seat captures the arrangement's gains - benefits diffuse across all users of the standard, and no extraction stream exists to capture - hence gain_flow 'diffuse'. Fixing is prohibitive relative to benefit: replacing the default would cost hundreds of millions of retrainings to secure an ergonomic edge whose size is itself contested (see dvorak_superiority_magnitude omega), so whoever could fix it (manufacturers, standards bodies) rationally does not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - mechanical typebar jamming - died with electric and direct-print mechanisms generations ago, yet the world still rearranges without the layout. The R5 mismatch (status dead x verdict world_rearranges) therefore fires as designed signal, and this reading explains it without embarrassment: the constraint converted from a jam-avoidance device into general skill-coordination infrastructure, and the conversion happened through competitive selection rather than theatrical upkeep. That conversion is why the theater_ratio stays low and the classification is rope rather than piton: the piton signature requires an administrator who could change the arrangement but bears less cost than fixing would cost him, plus no concentrated beneficiary. Here administrators are passive ratifiers, beneficiaries are numerous (if diffuse), and the cost-asymmetry runs through genuine skill capital rather than neglect. The classification prevents mislabeling in both directions: it stops the advocate seat's real friction from being read as snare-grade extraction (nobody imposes it), and it stops the low measured extraction from being read as proof of benignity by fiat (the dvorak_superiority_magnitude and switching_cost_decomposition omegas keep the adequacy claim answerable to evidence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_superiority_magnitude,
    'How large is the true typing-performance difference between QWERTY and Dvorak-class alternatives under properly controlled conditions?',
    'Pre-registered randomized longitudinal trials with blinded assessment, replicating and correcting the known confounds of the wartime Navy studies (instructor enthusiasm, volunteer selection, vendor sponsorship) and Strong''s 1956 retraining experiment (attrition, motivation effects, short duration).',
    'A large, robust advantage undermines the adequacy axiom, pushes this reading toward the lock_in sibling''s coordination-failure account, and would justify reclassifying the constraint away from rope; a negligible or unreproducible advantage confirms the naturalization account and stabilizes the low-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_magnitude, empirical, 'Contested magnitude of the Dvorak advantage - the empirical hinge of the naturalization reading.').

omega_variable(
    switching_cost_decomposition,
    'Are observed switching frictions genuine skill-investment costs, or are they socially manufactured barriers (employer refusal to accommodate retrained staff, unavailable labeled hardware, software defaults)?',
    'Audit employer accommodation policies for self-retrained workers, hardware remapping and keycap availability, and operating-system layout support; compare realized switching friction for individuals versus institutions over the interval.',
    'If a substantial fraction of the friction is manufactured, suppression is understated and the constraint drifts toward the lock_in or beneficiary_extraction accounts with rising effective extraction; if the friction tracks real human-capital investment, the rope profile and low epsilon stand as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_decomposition, empirical, 'Whether the measured switching costs are authentic skill capital or constructed obstacles.').

omega_variable(
    kernel_committer_structure,
    'This story instantiates the naturalization reading of kernel qwerty_persistence_mechanism. Which structural element - the existence of a systematic beneficiary, the reality of technical inferiority, or active maintenance behavior by incumbents - separates it from the lock_in and beneficiary_extraction sibling readings?',
    'Forensic business history of typewriter and keyboard manufacturers (pricing structure, exclusivity arrangements, marketing of alternatives, remapping support), combined with the comparative-trials program in the dvorak_superiority_magnitude omega; the readings disagree specifically on whether defenders existed and whether superiority was real, not on the fact of persistence.',
    'Resolution toward the beneficiary_extraction sibling raises epsilon, adds victim seats, and moves classification toward snare or tangled_rope; resolution toward the lock_in sibling keeps epsilon low but recasts persistence as coordination failure rather than adequacy, changing the normative reading while leaving the metrics nearly intact; while unresolved, all three readings coexist as rival explanations held by different scholarly factions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-frame omega locating the structural deltas between this reading and its two siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1873, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qpm_nat_tr_t1873, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1873, 0.04).
narrative_ontology:measurement(qpm_nat_tr_t1890, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1890, 0.04).
narrative_ontology:measurement(qpm_nat_tr_t1920, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(qpm_nat_tr_t1945, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1945, 0.06).
narrative_ontology:measurement(qpm_nat_tr_t1965, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(qpm_nat_tr_t1985, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(qpm_nat_tr_t2000, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(qpm_nat_tr_t2026, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2026, 0.1).

% Extraction over time
narrative_ontology:measurement(qpm_nat_be_t1873, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1873, 0.2).
narrative_ontology:measurement(qpm_nat_be_t1890, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1890, 0.16).
narrative_ontology:measurement(qpm_nat_be_t1920, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1920, 0.14).
narrative_ontology:measurement(qpm_nat_be_t1945, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(qpm_nat_be_t1965, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1965, 0.13).
narrative_ontology:measurement(qpm_nat_be_t1985, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1985, 0.17).
narrative_ontology:measurement(qpm_nat_be_t2000, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(qpm_nat_be_t2026, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2026, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'why QWERTY persists' (epsilon-invariance principle). The label covers three structurally distinct claims with different epsilon values and different beneficiary structures: (1) THIS FILE, the naturalization reading - QWERTY is genuinely adequate and alternatives lapsed through fair competition; low stable epsilon, no systematic beneficiary, no victims, claimed rope. (2) qwerty_persistence_mechanism__lock_in_reading - QWERTY persists through path-dependent coordination failure despite technical inferiority; epsilon similarly low but the persistence is framed as failure, not merit, with different omega structure (superiority assumed rather than contested). (3) qwerty_persistence_mechanism__beneficiary_extraction_reading - incumbents actively maintained the layout to protect training investments and market position; high epsilon, named victim seats, snare/tangled_rope territory. The naturalization reading functions as the null hypothesis of the family: the sibling stories are attempts to show the low measured extraction conceals either failure or rent. Each story carries its own stable epsilon over the same standing arrangement; averaging them into one story would fabricate an observable-dependent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
