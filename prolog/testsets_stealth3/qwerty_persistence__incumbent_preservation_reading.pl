% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Standard Persistence — Incumbent-Preservation Regime
 *   domain: technology history / industrial standards / path dependence
 *
 * SUMMARY:
 *   A single letter arrangement, patented in the 1870s for a mechanical
 *   problem that no longer exists, governs nearly every keyboard in use. This
 *   story authors that standing arrangement as it appears under one specific
 *   account of why it endures: it persists because identifiable parties
 *   actively defend it. On this account the arrangement is a jointly produced
 *   coordination good wrapped around a defended incumbent position —
 *   manufacturers ship only the defended layout and decline to stock rivals;
 *   the instruction industry teaches only it; employers screen for it; and
 *   every would-be migrant pays retraining costs alone, without network
 *   support, while defenders bear no comparable exposure. The arrangement
 *   solves a real problem — skill-and-equipment fungibility across a vast
 *   labor market — and simultaneously taxes those who would trade it for
 *   something better. Epsilon is assessed by this reading's own lights on the
 *   standing arrangement as found, with the defensive apparatus counted
 *   inside the arrangement's cost structure; it is not averaged with, or
 *   hedged against, any rival account of the same history. Claimed type and
 *   metrics are authored independently: the claim states what this reading
 *   takes the structure to be; the metrics state what its operation looks
 *   like descriptively. KEY AGENTS (by structural relationship): -
 *   keyboard_hardware_manufacturers: Agenda setter (institutional/mobile,
 *   global) — decides which letter arrangements ship in hardware and
 *   firmware; the arrangement protects decades of tooling, molds, and
 *   product-line plans - typing_instruction_industry: Beneficiary
 *   (organized/constrained, national) — sells courses, texts, and speed
 *   certifications built exclusively on the majority layout -
 *   trained_qwerty_typists: Beneficiary with payer exposure
 *   (moderate/identity_locked, global) — marketable skill fused to the
 *   majority layout; portability gained, retraining exposure carried -
 *   alternative_layout_adopters: Payer (powerless/trapped, global) — switched
 *   deliberately and now absorbs reverse lock-in: slower collaboration,
 *   incompatible shared hardware, permanent explanation overhead -
 *   efficiency_seeking_typists: Payer (moderate/constrained, global) —
 *   evaluated the redesign evidence, priced the solo retraining, mostly
 *   walked away - alternative_layout_developers: Excluded (moderate/trapped,
 *   national) — produced improved designs with no seat where shipping
 *   decisions were made - economic_historians: Observer
 *   (analytical/analytical, global) — reconstructs the record all camps argue
 *   from
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.52).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Standard Persistence — Incumbent-Preservation Regime").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology history / industrial standards / path dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '8fed1716-0c5e-4c3a-a755-3d32a53c9647').
narrative_ontology:cs_kernel_codification('8fed1716-0c5e-4c3a-a755-3d32a53c9647', formalized).
narrative_ontology:cs_authority_grounding('8fed1716-0c5e-4c3a-a755-3d32a53c9647', extraction).
narrative_ontology:cs_interpretation_layer_present('8fed1716-0c5e-4c3a-a755-3d32a53c9647').
narrative_ontology:cs_reading_relation('8fed1716-0c5e-4c3a-a755-3d32a53c9647', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('8fed1716-0c5e-4c3a-a755-3d32a53c9647', foundational, beneficiary_defense_is_load_bearing).
narrative_ontology:cs_axiom_status(beneficiary_defense_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('8fed1716-0c5e-4c3a-a755-3d32a53c9647', beneficiary_defense_is_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('8fed1716-0c5e-4c3a-a755-3d32a53c9647', secondary, incumbent_capital_specificity_forecloses_migration).
narrative_ontology:cs_axiom_status(incumbent_capital_specificity_forecloses_migration, holdable).
narrative_ontology:cs_axiom_grounding('8fed1716-0c5e-4c3a-a755-3d32a53c9647', incumbent_capital_specificity_forecloses_migration, empirically_contingent).
narrative_ontology:cs_reference_frame('8fed1716-0c5e-4c3a-a755-3d32a53c9647', actively_defended_incumbent_standard).
narrative_ontology:cs_drift_state('8fed1716-0c5e-4c3a-a755-3d32a53c9647', contemporary_post_efficiency_critique_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8fed1716-0c5e-4c3a-a755-3d32a53c9647', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_instruction_industry).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, path_dependence_network_effects_doctrine).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, first_mover_advantage_irreversibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell typewriters and keyboards carrying the majority letter arrangement, and decide which layouts ship as built-in options and which remain unavailable without third-party modification. Decades of tooling, molds, firmware defaults, and product-line planning are tied to shipping what they already build, which gives them a standing reason to keep the line unchanged. Because their factories could produce any layout with retooling, their practical exit from the arrangement is comparatively easy should demand ever move — the asymmetry is that they control whether demand is allowed to form.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_hardware_manufacturers, agenda_setter,
    institutional, generational, mobile, global).

% Business colleges, vocational programs, and commercial training providers sell courses, textbooks, and speed certifications built around the majority layout. Curricula, testing instruments, and instructor pipelines all presume students learn the prevailing arrangement. Teaching anything else would mean rewriting materials and issuing credentials employers do not recognize, so their offerings track whatever hiring managers expect, and enrollment follows the layout's ubiquity.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_instruction_industry, beneficiary,
    organized, biographical, constrained, national).

% Workers whose marketable skill is fluency in the majority arrangement. Because nearly every employer expects it, their proficiency travels with them between jobs, machines, and countries. Learning a different arrangement would mean months of slowed output while a certified, resume-ready skill depreciates, with almost no workplace able to use the new skill afterward. Their professional self-concept — speed certificates, typing-test scores, decades of practice — is bound up in the layout they already command.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists, payer).

% Individuals and small offices that deliberately switched to a redesigned arrangement after weighing the evidence. After switching they type slower than before during retraining, cannot borrow a colleague's keyboard or cover shared reception duties, and answer questions about their unusual setup indefinitely. Returning to the majority arrangement costs a second round of relearning, so the decision is hard to undo in either direction.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    powerless, biographical, trapped, global).

% Typists and office managers who examined the redesign evidence and considered switching for speed or comfort. Each weighs weeks of degraded output against uncertain lifetime gains, finds no employer willing to fund the transition for a single workstation, and mostly abandons the attempt; the few who persist bear the cost alone, without peer support, shared hardware, or employer recognition of the new skill.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists, payer,
    moderate, biographical, constrained, global).

% Researchers and inventors — most prominently the university team behind the 1930s rearrangement — who designed improved layouts and sought manufacturers willing to build them. Their proposals required factory commitments and distribution channels they never controlled; design work without a production partner went nowhere, and they held no seat where shipping or curriculum decisions were made. Their advocacy continues in journals and enthusiast communities far from procurement.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_developers, excluded,
    moderate, biographical, trapped, national).

% Scholars who reconstruct why the arrangement spread and endured, working from shipping decisions, advertising, patent records, and the documented failures of every migration attempt. They take no operational side and bear none of the arrangement's costs; their competing reconstructions are the terrain on which the other seats argue.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_hardware_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real interoperability problem: one common letter arrangement lets any trained operator use any machine, lets employers hire interchangeably and cover absences, lets schools teach one curriculum with universal payoff, and lets equipment makers share components and buyer expectations. The coordinated good is skill-and-equipment fungibility across a very large labor market.
% TRANSFER_FUNCTION: Moves adaptation costs onto whoever departs from the arrangement — retraining hours, temporary output loss, and compatibility friction land on switchers and would-be switchers — while continuing demand for compatible equipment and instruction flows to the makers and training providers whose products the arrangement keeps indispensable.
% ABSENT_VOICES: Redesign advocates and the researchers behind improved layouts had no seat where shipping or curriculum decisions were made; ordinary typists were consulted by no one — the arrangement was chosen by equipment designers and defended by suppliers and employers. Would-be switchers inside companies lack representation anywhere: no budget line, no advocate in procurement, no counterparty to hear the case.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, hundreds of millions of people could not type until retrained; keyboards, firmware defaults, hiring tests, certification exams, and course catalogs would all need replacement or rewrite. Shared-device workplaces, employment screening, and the entire training industry would reorganize around whatever replaced it — the arrangement is load-bearing infrastructure for the modern written-work economy.
% FOUNDING_PROBLEM: Early typewriters jammed when adjacent typebars were struck in rapid succession; the layout was arranged to separate frequently paired letters and reduce collisions, and to support brisk sales demonstrations.
% FOUNDING_PROBLEM_CORROBORATION: Typewriter engineering histories and surviving machine designs document the jamming rationale and its extinction with typeball elements and electronic printing; economic historians on opposing sides of the persistence debate agree the original mechanical problem vanished generations ago. Corroboration comes entirely from outside the beneficiary set — notably, the beneficiary seats no longer cite jam prevention at all.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 composes three streams this reading counts as costs of the standing arrangement: retraining barriers borne by solo migrants, foregone-efficiency drag across the enormous installed base, and the defensive apparatus itself (withheld built-in options, uniform curricula, hiring screens) — the reading defines defensive costs as part of the arrangement, so they belong in epsilon. Suppression 0.52 is today's residual enforcement — default settings, procurement specifications, hiring norms — and is authored as a raw structural property, unscaled by power or scope; the engine scales only extractiveness. Theater 0.25 reflects the growing rhetorical layer of the defense: contested mid-century efficiency studies still circulate as proof, standards bodies convene periodic reviews that change nothing, while the material enforcement that once consisted of refusing to build alternatives has thinned. Accessibility collapse 0.70: alternatives are one setting away on modern operating systems, yet shared hardware, hiring expectations, and tutorial ecosystems collapse the practical alternative space for anyone whose typing is embedded in a workplace. Resistance 0.45: recurring advocacy waves — the 1930s redesign campaign, ergonomic movements, corporate pilots — met real organizing energy and consistently failed to reach critical mass, which is precisely the phenomenon this reading exists to explain. Measurements run on one shared grid (seven points spanning 1950–2022 at twelve-year steps): extractiveness peaks at the personal-computer transition, when migration was cheapest and was nonetheless not taken; the suppression series is authored because enforcement capacity is the dynamic this story traces — it declines monotonically as software decoupled layout from hardware and enforcement shifted from physical exclusivity to defaults and norms; theater rises through the era when the defense became mostly argument. The series are arcs, not cycles — no oscillation mechanism is claimed, and the end-state values match the authored scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the manufacturers' seat the arrangement is an asset they maintain: nothing about it reads as imposed, because they are the ones doing the imposing. From the training industry's seat it is the syllabus — the property that makes their product universally sellable. From the trained typist's seat it is simultaneously the foundation of their livelihood and a ceiling they have been taught is immovable; that seat is the pivot where the arrangement's coordination face and its cost face meet in one person. From the adopters' and would-be switchers' seats the same arrangement is a wall with a tollbooth: visible, quantified, and individually irrational to climb. The engine derives each seat's classification from power, exit, and directional position; the gap between the asset-view and the wall-view of one identical structure is the perspectival divergence this corpus exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly onto the d-axis. keyboard_hardware_manufacturers are declared beneficiaries and hold the agenda: the arrangement subsidizes them (protected tooling, undisturbed demand), placing them near the beneficiary end. typing_instruction_industry collects enrollment that presupposes the layout — near-full beneficiary. trained_qwerty_typists are dual-declared: portability income flows in, retraining exposure and skill-depreciation risk flow out; with identity_locked exit their effective position sits near symmetry with a tilt toward target, because the arrangement's persistence is exactly what makes their sunk skill unrefundable. alternative_layout_adopters and efficiency_seeking_typists are declared victims with trapped and constrained exits respectively — near-full targets, the adopters more so because their reverse lock-in is total. alternative_layout_developers, excluded from every decision surface, are structurally pure targets. No directionality overrides are used: beneficiary/victim declarations plus exit atoms already produce the correct spread, and the one genuinely mixed seat is handled by its dual declaration rather than by an override that would also sweep unrelated agents sharing its power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both halves of this arrangement visible. Read as pure coordination, the arrangement looks like a benign standard and the seventy-year failure of every migration attempt reads as market wisdom; read as pure extraction, the real interoperability benefits vanish and the puzzle becomes why no coalition ever formed against an obvious imposition. The hybrid reading says both facts are the mechanism: the coordination good recruits defenders cheaply (every trained typist defends it for free, because their skill rides on it), and the defended position is what forecloses alternatives at near-zero defender cost (withholding a built-in option is free; building it invites cannibalization). The genealogy interview sharpens the picture: the founding problem — typebar jamming — has been dead for two generations, and the founding-problem-status/disappearance-verdict pair (dead, world_rearranges) is an honest mismatch: the arrangement no longer does what it was built for, yet the world genuinely would scramble without it, because a newer coordination good grew on top of the old one. That mismatch is the zombie signature, and the theater series corroborates it only moderately — the arrangement is not yet mostly performance; its defense still buys real exclusivity. Whether the residue is a maintained asset or a gilded relic is exactly the question this file leaves open for the engine and for the sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qwerty_kernel_reading_scoping,
    'This constraint is the incumbent_preservation_reading of the qwerty_persistence kernel — does scoping persistence as beneficiary-defended (rather than coordination-sustained) correctly attribute the mechanism?',
    'Cross-reading comparison: compile the sibling lapsed_alternatives_reading and compare computed classifications, epsilon composition, and victim-set behavior against the same historical record; divergent classifications localize the disputed causal weight.',
    'If coordination value alone accounts for persistence, this reading''s defensive-suppression costs are residual and epsilon drops toward the sibling''s profile; if defense is load-bearing, the sibling understates suppression and loses the victim seats this reading declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_kernel_reading_scoping, conceptual, 'Committer-frame scoping: one of two live readings of the QWERTY persistence kernel; the disagreement sits at the causal locus of persistence.').

omega_variable(
    defense_evidence_attribution,
    'Are the documented defense behaviors — withheld alternative layouts, uniform curricula, hiring-screen norms — load-bearing causes of persistence, or background texture around a self-sustaining coordination equilibrium?',
    'Archival work: manufacturer product-planning memos, licensing correspondence concerning the 1930s redesign proposals, procurement specifications recording layout as an explicit requirement.',
    'Load-bearing defense supports the tangled_rope profile with enforcement-gated persistence; noise-level defense collapses this constraint toward the sibling reading''s rope-like shape with epsilon near coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_evidence_attribution, empirical, 'Whether defensive conduct is causal or epiphenomenal to the standard''s endurance.').

omega_variable(
    alternative_superiority_uncertainty,
    'Do redesigned arrangements deliver efficiency or comfort gains large enough that foregone adoption constitutes real harm to efficiency-seekers?',
    'Controlled longitudinal switching studies with honest retraining-cost accounting, correcting the documented flaws of the mid-century naval studies; modern keystroke-level productivity modeling.',
    'A substantial verified advantage keeps efficiency-seekers in the victim set and epsilon high; a negligible advantage removes them, shrinks epsilon toward pure inertia, and effectively awards the kernel to the sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_superiority_uncertainty, empirical, 'The empirical crux separating this reading from the lapsed-alternatives sibling: the size of the foregone prize.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression holding would-be switchers in place structural (defaults, hiring screens, shared hardware) or internalized (typists'' conviction that switching is impractical, professional skill identity fused to the majority layout)?',
    'Natural experiments where structural barriers were removed at organizational scale: track whether retrained cohorts retain an alternative layout once defaults stop enforcing it, or revert.',
    'If substantially internalized, effective suppression outlasts structural removal — the measured scalar understates persistence force, and identity lock among trained typists is doing more work than the structural data shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression force between external barriers and fused skill identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t12, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t24, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t24, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t36, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t36, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t48, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 48, 0.32).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t48, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t60, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_tr_t72, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 72, 0.25).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(qwerty_incumbent_reading_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t12, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t24, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t24, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t36, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t36, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t48, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 48, 0.66).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t48, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t60, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_be_t72, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 72, 0.62).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_incumbent_reading_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t12, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t24, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t24, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t36, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 36, 0.71).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t36, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t48, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 48, 0.63).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t48, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t60, observed).
narrative_ontology:measurement(qwerty_incumbent_reading_su_t72, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 72, 0.52).
narrative_ontology:measurement_basis(qwerty_incumbent_reading_su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why QWERTY persisted' decomposes per epsilon-invariance into two structurally distinct claims, forming a constraint family. This file authors persistence-via-beneficiary-defense: epsilon includes defensive suppression costs, the victim set comprises alternative-layout adopters and efficiency-seekers, and the claimed type is tangled_rope. The sibling, qwerty_persistence__lapsed_alternatives_reading, authors persistence-via-coordination-value with alternatives lapsing below critical mass: lower epsilon, thinner victim set, rope-side expectation. The evidentiary relationship runs in both directions — this reading reads the same archival record as evidence of active suppression; the sibling reads the identical record as evidence of sufficient coordination value — so the edge is declared bidirectionally aware via affects_constraints rather than claiming clean upstream/downstream order. Neither file averages across the divide.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
