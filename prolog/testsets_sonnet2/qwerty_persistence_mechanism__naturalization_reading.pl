% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Adequacy/Fair-Competition Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested QWERTY-persistence
 *   kernel: the naturalization reading, under which QWERTY persists because
 *   it became and remains genuinely adequate, and alternative layouts
 *   (principally Dvorak) failed to displace it through open, fair competition
 *   rather than through suppression or captured incumbency. Under this
 *   reading, switching costs are read as real skill-investment costs borne by
 *   individual typists (not manufactured exit barriers), and the Dvorak
 *   speed-advantage claim is treated as empirically contested rather than
 *   established — following the Liebowitz & Margolis critique of the original
 *   David (1985) lock-in narrative. This is a low-extraction, low-suppression
 *   reading precisely because it denies the beneficiary-extraction and
 *   inefficient-lock-in premises that the sibling readings assert. The
 *   sibling readings (lock_in_reading, beneficiary_extraction_reading) are
 *   NOT part of this file; they are separate constraints with their own ε
 *   values, authored elsewhere and linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - typists_with_existing_skill: primary beneficiary of the coordination standard (organized/mobile) — bears retraining cost only if they choose to switch
 *   - keyboard_manufacturers: institutional actor supplying to demonstrated demand, not shaping it against alternatives (institutional/arbitrage)
 *   - software_and_hardware_ecosystem: infrastructural beneficiary of a common default with low switching friction (institutional/mobile)
 *   - dvorak_and_alternative_layout_advocates: present but unpersuasive voice, not an excluded/suppressed one under this reading (moderate/mobile)
 *   - path_dependence_researchers: analytical observer weighing the lock-in narrative against its empirical rebuttal (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout Persistence (Adequacy/Fair-Competition Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '68a44b2b-aa27-4b6b-b92b-0e83d2eb2968').
narrative_ontology:cs_kernel_codification('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', distributed).
narrative_ontology:cs_authority_grounding('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', practice).
narrative_ontology:cs_interpretation_layer_present('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968').
narrative_ontology:cs_reading_relation('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', foundational, qwerty_functional_adequacy_thesis).
narrative_ontology:cs_axiom_status(qwerty_functional_adequacy_thesis, holdable).
narrative_ontology:cs_axiom_grounding('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', qwerty_functional_adequacy_thesis, empirically_contingent).
narrative_ontology:cs_axiom('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', foundational, fair_competition_selection_thesis).
narrative_ontology:cs_axiom_status(fair_competition_selection_thesis, holdable).
narrative_ontology:cs_axiom_grounding('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', fair_competition_selection_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', competitive_market_adequacy_standard).
narrative_ontology:cs_drift_state('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', contemporary_digital_keyboard_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68a44b2b-aa27-4b6b-b92b-0e83d2eb2968', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typists_with_existing_skill).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, software_and_hardware_ecosystem).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, qwerty_functional_adequacy_thesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, fair_competition_selection_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learned QWERTY as their touch-typing standard; their trained skill transfers across virtually every device and workplace because the layout is near-universal. Retraining to an alternative layout would cost them time and temporary productivity loss, but nothing prevents them from switching if an alternative offered a clear enough advantage — most simply judge the switching cost not worth a marginal or contested gain.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typists_with_existing_skill, beneficiary,
    organized, biographical, mobile, global).

% Manufacture QWERTY-layout keyboards because that is what the installed base of trained typists demands; they also freely manufacture Dvorak, Colemak, and other layouts as software-configurable options on modern hardware. They are not shown here to lobby against alternatives or restrict their availability — production follows demonstrated demand rather than shaping it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Operating systems, input software, and physical keyboards all support layout switching at negligible marginal cost today. The ecosystem benefits from a common default that minimizes support burden and cross-device friction, while still accommodating individual choice for the minority who prefer alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, software_and_hardware_ecosystem, beneficiary,
    institutional, generational, mobile, global).

% Argue that alternative layouts offer typing-speed or ergonomic advantages and are underused. Under this reading, they had — and continue to have — full technical freedom to adopt and promote alternatives; their limited traction reflects an empirically contested efficiency case and low realized demand rather than exclusion from the conversation. Their advocacy is heard in ergonomics and typing communities but has not produced evidence sufficient to shift the equilibrium.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, dvorak_and_alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Study why QWERTY persisted after typewriter-era jamming constraints disappeared. Under this reading they evaluate the David (1985) lock-in narrative against the Liebowitz & Margolis (1990) rebuttal showing Dvorak's claimed speed advantage rests on weak, non-independently-replicated evidence — concluding the persistence outcome is consistent with genuine competitive adequacy, not market failure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, path_dependence_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single near-universal keyboard layout lets typing skill, muscle memory, touch-typing instruction, and device design all coordinate around one standard, eliminating the need to relearn or redesign for every new device or workplace.
% TRANSFER_FUNCTION: Under this reading, nothing systematic transfers from a victim class to a beneficiary class — training investment stays with the trained typist as a portable skill, and manufacturers respond to that demand rather than extracting from it.
% ABSENT_VOICES: Alternative-layout advocates are present in the record (ergonomics literature, typing-speed contests, hobbyist communities) but have not produced evidence strong enough to shift majority behavior; their voice is heard, not excluded, under this reading — its influence is limited by contested evidence, not by suppression.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, existing typists would face a genuine retraining cost proportional to their skill investment — that much rearranges. Whether the world would then converge on a superior alternative or simply re-settle on another arbitrary-but-adequate standard is exactly what this reading and its siblings dispute; under the naturalization reading, no clearly superior alternative is waiting to be unlocked, so the honest verdict is contested rather than a clean world_rearranges.
% FOUNDING_PROBLEM: Early typewriters needed a key arrangement that reduced mechanical jamming from adjacent frequently-paired letters striking in sequence; QWERTY solved that mechanical problem for its era.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers and typewriter historians outside the keyboard industry attest the jamming problem disappeared with electric and electronic keyboards decades ago. Under this reading, that the original problem is dead does not imply the layout became a rent-extraction vehicle — Liebowitz & Margolis's independent economic analysis (unaffiliated with any keyboard manufacturer) is offered as outside corroboration that the layout's continued use reflects contested-but-real adequacy rather than captured demand.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.12) because this reading denies that any party captures rents from QWERTY's persistence — the metric reflects the reading's own claim that switching costs are genuine skill-investment costs, not manufactured exit barriers. Suppression is authored very low (0.08) because alternative layouts are freely available on virtually all modern hardware/software at zero marginal cost — nothing structurally blocks adoption. Accessibility_collapse is moderate (0.6), not low, because in practice almost no one switches once trained — but under this reading that collapse is attributed to rational cost-benefit calculation given contested benefits, not to suppression of alternatives. Resistance is low (0.2): there is a small, persistent advocacy community for alternative layouts, but it does not constitute active resistance to an extractive arrangement, because this reading holds there is no extraction to resist.
 *
 * PERSPECTIVAL GAP:
 *   The engine may compute this reading's stakeholders differently than the sibling readings' stakeholders, even though some agent names could plausibly overlap (e.g., a manufacturer) — that divergence is exactly the point of the kernel decomposition. Under this reading, keyboard_manufacturers sit near the beneficiary end because they are read as demand-responsive rather than lock-in-preserving; the beneficiary_extraction_reading would place a differently-drawn manufacturer actor much closer to the target/extractor pole. These are not the same stakeholder computed twice; they are structurally distinct claims about the same real-world actor, authored in separate files.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim group is declared in this reading — the schema's tangled_rope/snare gates do not apply because this reading's claimed_type is rope, and rope requires no victims. Beneficiaries (typists, manufacturers, ecosystem) all sit near the low-d end because the reading holds the arrangement to be a positive-sum coordination outcome: typists get a portable skill and universal compatibility, manufacturers get a stable production standard, and the ecosystem gets minimized support burden — with no corresponding extraction from a subordinated party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typewriter jamming) is dead, which could suggest mandatrophy — a constraint outliving its function. This reading resists that inference: it holds that QWERTY's justification migrated from 'prevents jamming' to 'is the near-universal, adequately-performing standard that coordinates a global installed base of skill and hardware,' and that this migrated justification is independently sound rather than a post-hoc cover story. The founding_problem_status is authored 'dead' honestly (the mechanical problem is gone), while the disappearance_verdict is authored 'contested' rather than 'world_unchanged' — the reading does not claim QWERTY is inconsequential, only that its consequence is genuine adequacy rather than captured extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_efficiency_evidence_status,
    'Does Dvorak (or another alternative layout) confer a real, replicable typing-speed or ergonomic advantage over QWERTY sufficient to justify retraining costs at scale?',
    'Independently replicated, randomized controlled typing studies with adequate sample sizes and blinded evaluation, free of sponsorship by any layout''s advocates or original 1940s Dvorak-funded studies.',
    'If a robust advantage were established and QWERTY still persisted despite it, this reading would collapse into the lock_in_reading (coordination failure) or beneficiary_extraction_reading (active suppression) — the naturalization reading depends on the advantage remaining genuinely contested or negligible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_efficiency_evidence_status, empirical, 'Whether Dvorak''s claimed efficiency advantage is empirically real or an artifact of weak original studies.').

omega_variable(
    which_reading_the_evidence_favors,
    'Among the three kernel readings (naturalization, lock_in, beneficiary_extraction), which best fits the historical and economic record of QWERTY''s persistence?',
    'Comparative historical-economic analysis synthesizing: (a) the original David (1985) network-externality lock-in model, (b) the Liebowitz & Margolis (1990) rebuttal and evidence review, (c) any documented manufacturer lobbying or standard-setting behavior actively disfavoring alternative layouts.',
    'This is the committer-level question the kernel decomposition exists to hold open: this file assumes the naturalization reading for its own internal consistency, but does not adjudicate which reading is correct. Resolution in favor of a sibling reading would not falsify this file (each reading is a self-consistent constraint) but would shift which reading best describes the actual world.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_evidence_favors, conceptual, 'Which of the three kernel readings the totality of evidence actually favors — the question this decomposition is structured to leave open rather than pre-resolve.').

omega_variable(
    switching_cost_attribution,
    'Are QWERTY switching costs best characterized as genuine, portable human-capital investment (this reading''s claim) or as an artificially inflated barrier maintained by ecosystem design choices (a sibling reading''s claim)?',
    'Comparative analysis of switching friction across countries/eras with different keyboard-layout defaults and education systems, controlling for ecosystem design choices versus pure retraining time.',
    'If switching costs are shown to be substantially inflated by design choices rather than pure retraining time, this reading''s low suppression/extraction scores would need revision toward the beneficiary_extraction_reading''s higher values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_attribution, empirical, 'Whether switching costs are natural skill-investment costs or partly manufactured friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the qwerty_persistence_mechanism kernel, each authored as a separate file per the ε-invariance principle: naturalization_reading (this file, claimed rope, ε≈0.12 — adequacy/fair-competition), lock_in_reading (path-dependent coordination failure despite technical inferiority — expected higher accessibility_collapse with no identified extractor), and beneficiary_extraction_reading (active incumbent maintenance to protect training investments — expected tangled_rope/snare with manufacturer beneficiaries and typist/alternative-layout victims). The three share the same underlying historical episode but instantiate structurally distinct claims about mechanism and beneficiary structure; they must not be averaged or reconciled into one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
