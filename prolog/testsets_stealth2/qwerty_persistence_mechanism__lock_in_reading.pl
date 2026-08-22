% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Layout Persistence via Path-Dependent Coordination Failure (Lock-In Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   The QWERTY layout, designed in the 1870s for mechanical typewriters and
 *   cemented by Remington's commercial success and the first generations of
 *   trained typists, remains the near-universal text-input standard more than
 *   half a century after its founding rationale — mechanical typebar jamming
 *   — ceased to exist. Superior layouts (Dvorak 1936, Colemak 2006) are
 *   freely available, built into every major operating system, and
 *   consistently fail to displace the incumbent. This story instantiates the
 *   LOCK-IN READING of why: persistence through path-dependent coordination
 *   failure. The arrangement transfers nothing to anyone; its cost is a
 *   diffuse deadweight loss spread across every text-producing worker, and
 *   its persistence requires no enforcer because each individual's rational
 *   attachment (protecting an enormously valuable skill asset) reproduces the
 *   collective trap. The colloquial label 'QWERTY inefficiency' decomposes,
 *   per the epsilon-invariance principle, into three structurally distinct
 *   constraint stories — adequacy (naturalization_reading), active
 *   maintenance (beneficiary_extraction_reading), and coordination failure
 *   (this file) — linked through the network block; each carries its own
 *   beneficiary structure, its own epsilon, and its own classification. KEY
 *   AGENTS (by structural relationship): - keyboard_standards_bodies: Formal
 *   agenda-setter (institutional/constrained) — ratifies the layout standard;
 *   revision is procedurally possible but imposes costs far beyond the body's
 *   mandate - os_and_device_vendors: Operative agenda-setter
 *   (institutional/constrained) — set the defaults that reproduce the
 *   standard on every new device - qwerty_trained_workforce: Diffuse
 *   beneficiary and bearer (powerless/constrained) — hundreds of millions
 *   whose skill asset is protected by the same persistence that taxes their
 *   throughput - keyboard_hardware_incumbents: Incidental beneficiary
 *   (powerful/mobile) — avoid retooling risk under continuity; capture no
 *   transfer - high_volume_text_workers: Principal bearers
 *   (moderate/constrained) — data entry, transcription, programming;
 *   accumulate the largest share of the efficiency differential -
 *   alternative_layout_developers: Stranded innovators (moderate/mobile) —
 *   personally escaped to superior layouts; their innovation cannot reach
 *   scale - typing_education_institutions: Curriculum beneficiaries
 *   (organized/constrained) — materials and certifications presuppose the
 *   incumbent layout - path_dependence_economists: Analytical observers
 *   (analytical/analytical) — see the full attractor structure across all
 *   seats
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.16).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.2).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Layout Persistence via Path-Dependent Coordination Failure (Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '04894876-e54b-4671-9583-85dfa1ca15bf').
narrative_ontology:cs_kernel_codification('04894876-e54b-4671-9583-85dfa1ca15bf', distributed).
narrative_ontology:cs_authority_grounding('04894876-e54b-4671-9583-85dfa1ca15bf', distributed).
narrative_ontology:cs_reading_relation('04894876-e54b-4671-9583-85dfa1ca15bf', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('04894876-e54b-4671-9583-85dfa1ca15bf', qwerty_persistence_mechanism__beneficiary_extraction_reading, influences).
narrative_ontology:cs_axiom('04894876-e54b-4671-9583-85dfa1ca15bf', foundational, switching_costs_can_trap_inferior_standards).
narrative_ontology:cs_axiom_status(switching_costs_can_trap_inferior_standards, holdable).
narrative_ontology:cs_axiom_grounding('04894876-e54b-4671-9583-85dfa1ca15bf', switching_costs_can_trap_inferior_standards, empirically_contingent).
narrative_ontology:cs_axiom('04894876-e54b-4671-9583-85dfa1ca15bf', foundational, no_rational_individual_actor_can_cure_the_lock).
narrative_ontology:cs_axiom_status(no_rational_individual_actor_can_cure_the_lock, holdable).
narrative_ontology:cs_axiom_grounding('04894876-e54b-4671-9583-85dfa1ca15bf', no_rational_individual_actor_can_cure_the_lock, empirically_contingent).
narrative_ontology:cs_reference_frame('04894876-e54b-4671-9583-85dfa1ca15bf', path_dependence_attractor_framework).
narrative_ontology:cs_drift_state('04894876-e54b-4671-9583-85dfa1ca15bf', post_liebowitz_margolis_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('04894876-e54b-4671-9583-85dfa1ca15bf', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_hardware_incumbents).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, os_and_device_vendors).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typing_education_institutions).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and ratify the keyboard layout standards (ANSI INCITS, ISO/IEC 9995) that codify the incumbent arrangement. Revision is procedurally available at any cycle, but any committee that proposed a mandated migration would impose retraining and retooling costs on every member constituency at once, far exceeding the body's budget, staff, and enforcement reach. The committees therefore ratify the existing layout by default, cycle after cycle, and treat layout substitution proposals as out of scope.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Decide which layout ships as the default on every new computer, phone, and terminal — the operative act that reproduces the standard on each hardware generation. Changing the default would expose them to enterprise incompatibility complaints, support-ticket spikes, and user backlash, while keeping it costs nothing and matches what their customers already know. They collect no fee from the layout itself; their stake is continuity and support-burden avoidance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, os_and_device_vendors, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, os_and_device_vendors, beneficiary).

% Hundreds of millions of people whose touch-typing skill is anchored to the incumbent layout. The standard's persistence protects the value of an asset many spent months acquiring and use daily for decades; a wholesale switch would strand that asset overnight. The same persistence imposes a small perpetual throughput and accuracy cost relative to optimized layouts. Individually each person could retrain, but retraining means months of reduced productivity followed by re-entry into a world that still runs on the old layout — an exchange no individual rationally makes.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce, payer).

% Manufacturers and peripheral vendors whose product lines, legends, and firmware assume the incumbent layout. Continuity spares them retooling lines, reprinting keycaps, and re-validating designs; a migration would be a costly disruption with no compensating revenue, since customers would not pay a premium for the new layout. They receive no income from the arrangement — their benefit is avoided cost, and they would happily produce any layout the market demanded.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_hardware_incumbents, beneficiary,
    powerful, biographical, mobile, global).

% Data-entry clerks, transcriptionists, court reporters, medical coders, and programmers who produce text for a living and therefore accumulate the largest share of the efficiency differential — extra keystrokes, error-correction time, and strain-hours compounded over careers. Their skill certifications and employer toolchains presuppose the incumbent layout, so retraining individually buys them a superior layout and an incompatible resume. Like the broader workforce, they also hold protected skill assets, which offsets part of their loss position.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_workers, beneficiary).

% The designers and advocate communities behind Dvorak, Colemak, and successor layouts. They have personally adopted their layouts — their own exit is complete — but their innovation cannot reach scale: every prospective adopter faces the same individually-irrational exchange, and standards processes, procurement specifications, and education curricula give their proposals no seat. Decades of documented efficiency arguments have moved the default nowhere.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers, payer,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers, excluded).

% Schools, vocational programs, and certification bodies whose curricula, drills, timed tests, and credentials are built around the incumbent layout. Continuity spares them rewriting materials and retraining instructors; their graduates arrive pre-adapted to employer expectations. Teaching a different layout would degrade the market value of their certificates until employers switched — a coordination step no single institution can take.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_education_institutions, beneficiary,
    organized, generational, constrained, national).

% Researchers in the David-Arthur tradition and their critics (the Liebowitz-Margolis line) who study the arrangement as the canonical case of increasing-returns lock-in. They take no position inside the arrangement, publish the competing causal accounts, and supply the empirical estimates — of the efficiency differential and of switching costs — on which any resolution of the kernel contest depends.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, path_dependence_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single universal text-input standard: any trained typist can sit at any keyboard anywhere and work; employers hire against a known skill; hardware legends, firmware, software defaults, and education curricula all assume one layout. The coordination problem solved is skill-hardware-software interoperability at civilizational scale — a problem any single standard solves, whichever one it is.
% TRANSFER_FUNCTION: Moves nothing to anyone. No fee, rent, or tribute flows from any seat to any other; the arrangement's cost is a diffuse deadweight loss — cumulative typing-time, error-correction, and strain overhead relative to optimized layouts — borne by every text-producing worker and collected by no one. Its benefit side is likewise non-transferable: skill-asset protection and retooling avoidance are avoided losses, not receipts.
% ABSENT_VOICES: Alternative-layout designers and ergonomics researchers would object that a superior, freely available layout is being perpetuated by pure coordination failure; they hold no seat in standards committees, procurement specification, or curriculum boards. Future typists not yet trained are also absent: each generation's institutions renew the default on behalf of people who will inherit the installed base without ever having been consulted about it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if the incumbent layout suddenly lost its standard status — the world would rearrange violently before settling higher: mass retraining across the installed-base workforce, hardware legend and firmware retooling, a multi-year productivity trough as muscle memory rebuilt, transitional chaos in shared equipment and legacy interfaces, followed by a permanent throughput and accuracy gain for the text-producing economy. Arrangements demonstrably depend on it; that is precisely the trap.
% FOUNDING_PROBLEM: Mechanical typebar jamming in 1860s-70s typewriters: adjacent frequently-struck typebars collided at the platen, so the layout was arranged (per the traditional account) to separate common letter pairs and, partly, to pace operators below the jam threshold. Remington's commercial dominance then certified the layout for the first mass generation of trained typists, creating the training network that outlived the machines.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party: typewriter historiography and patent records (Smithsonian collections, Sholes correspondence) document the jamming problem and the design response; electrical-engineering and office-technology histories document its extinction with the electric typewriter in the mid-twentieth century. No party disputes that mechanical jamming no longer constrains keyboard design. The live dispute — whether the layout itself remains adequate — belongs to the sibling readings' contest, not to the founding problem's status, which is settled.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.16) because the lock-in reading's defining claim is that nothing is collected: the arrangement produces a deadweight efficiency loss, not a transfer. Suppression is low-to-moderate (0.20) and — critically — DECLINING across the interval: the layout was once physically bound into hardware (leaving meant buying different machines), while modern software makes remapping trivial. Adoption stayed near zero anyway, which is the lock-in reading's central evidence: the binding mechanism is the network externality, not coercion. Theater is low (0.18): the arrangement persists silently, without ceremonial defense; the modest recent rise reflects marketing rhetoric ('industry standard,' 'proven ergonomics') rather than functional activity. Accessibility_collapse is moderate (0.60): alternatives remain purchasable and legal, but once a typist understands the externality, the alternative collapses as an INDIVIDUAL option — a Dvorak-trained worker is stranded on every shared machine, timed assessment, and employer assumption. Resistance (0.35) recurs in waves — ergonomic campaigns, corporate pilots, open-source layout projects — and is repeatedly deflated by the very coordination failure under study. The claimed type is PITON: the founding function (jam avoidance) is dead, the arrangement persists through institutional inertia, no seat profits enough to maintain it, no seat hurts enough to fix it, and the fix is collectively unreachable. I author theater honestly as low even though piton cases often show high theater — the guidance is explicit that theatricality is a symptom, not the test; the test is the cost-asymmetry, which this arrangement exhibits in pure form. All three measurement series run on one shared time grid (1936/1955/1975/1990/2005/2024) so every metric is authored at every examined point; the suppression_requirement series is included because the story specifically tracks the erosion of the structural barrier (hardware-bound layout to software-remappable layout), not merely a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. From the standards-body and vendor seats, the arrangement is a neutral default being administered: nothing flows to them, and touching it is pure downside. From the workforce seat, the same arrangement is simultaneously a subsidy (skill-asset protection) and a diffuse tax (throughput differential) — a genuinely dual-positioned seat that neither a pure-beneficiary nor a pure-target derivation captures. From the alternative-layout seat, it is a wall: the innovation exists, works, and cannot propagate. Same-level lateral divergence: two typists of identical skill and identical organizational rank experience different constraints depending solely on tool control — a freelance programmer who owns her machines and can remap everything sits near the mobile end, while a corporate data-entry clerk on a managed fleet with standardized images sits at the constrained end. Power is equal; exit differs by a constraint-specific factor (control over the tool chain), which is exactly the same-level differentiation the structural data should register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the trained workforce and hardware incumbents sit toward the beneficiary end; high-volume text workers and alternative-layout developers toward the target end. Two overrides correct derivations the structural arrays cannot express. First, the trained workforce is declared a beneficiary (its skill value is protected), which would derive a low d — but the same population bears the entire deadweight cost, making its true position symmetric (override: powerless -> 0.50). Second, high-volume text workers are declared victims, deriving a high d — but their QWERTY skill asset is also protected by persistence, offsetting part of their loss position (override: moderate -> 0.65). The overrides apply cleanly at the power-atom level because each atom hosts a single coherent seat in this story. No override is needed for hardware incumbents (genuinely incidental beneficiaries, mobile exit, near the beneficiary end) or for the standards and vendor seats (agenda-setters whose costs and benefits are both negligible — near-symmetric administrators).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical typebar jamming — died with the electric typewriter, and the arrangement's mandate has accordingly outlived its function (mandatrophy_resolved: true; founding_problem_status: dead). The R5 mismatch consumer will flag this story: dead founding problem combined with a world_rearranges disappearance verdict is the zombie signature. The receipt surface resolves the flag's meaning: gain_flow is affirmatively diffuse — after checking every named seat, none captures a transfer, because there is no transfer — so this is a zombie without a parasite. That distinction is exactly what the classification apparatus exists to protect. Without the piton category, the lock-in phenomenon would be forced into one of two mislabels: the snare/tangled_rope reading would have to invent a culprit (the extraction sibling's move — positing incumbents defending rents the record does not show them collecting), or the rope reading would have to deny the suboptimality (the naturalization sibling's move — asserting the outcome is adequate and closing the question). Piton names the actual structure: a former rope whose founding function atrophied, persisting through inertia, costing everyone a little, benefiting no one enough to maintain it, hurting no one enough to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint is one reading (lock_in_reading) of the qwerty_persistence_mechanism kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling stories: the naturalization_reading removes the victim structure entirely (alternatives lapsed fairly, no suboptimality to explain, epsilon approaches zero); the beneficiary_extraction_reading adds concentrated beneficiaries and active enforcement (incumbents defending training investments, requiring_active_enforcement true). The disagreement is located in the causal mechanism of persistence — adequacy versus inertia versus active defense — and consequently in whether any seat captures gains.',
    'If the naturalization reading is correct, this story''s victim declarations dissolve and the arrangement certifies as ordinary coordination; if the extraction reading is correct, gain_flow shifts from diffuse to a named capturer and the type moves toward tangled_rope or snare. This story''s own classification is valid only under the lock-in mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: one of three rival readings of the QWERTY persistence kernel, with the structural deltas each sibling would introduce.').

omega_variable(
    dvorak_differential_magnitude,
    'How large is the actual efficiency and error-rate differential between QWERTY and optimized alternatives such as Dvorak or Colemak?',
    'Controlled typing trials with matched participant pools, meta-analysis of the Dvorak seminar studies (including re-examination of the Strong and Healy critiques of early Navy-sponsored research), and keystroke-dynamics modeling on large corpora.',
    'If the differential is negligible, the lock-in reading loses its object — persistence of an adequate layout is not a coordination failure — and the story collapses toward the naturalization sibling. If the differential is substantial, the collective-suboptimality claim stands and the piton characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_differential_magnitude, empirical, 'Magnitude of the technical inferiority that the lock-in mechanism is supposed to be trapping.').

omega_variable(
    migration_cost_benefit_counterfactual,
    'What would a coordinated global migration to a superior layout actually cost, relative to the discounted stream of aggregate efficiency gains?',
    'Economic modeling of simultaneous-transition scenarios: retraining hours across the installed-base workforce, hardware and firmware retooling, transitional error costs, against long-run throughput gains; calibrated on partial natural experiments (single-firm or single-agency voluntary switches).',
    'If migration is net-positive but only under simultaneity, the prohibitive-fixing-cost characterization and the piton classification are confirmed as a genuine coordination failure. If migration is net-negative even when coordinated, the arrangement is not suboptimal at all and the naturalization sibling absorbs the phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migration_cost_benefit_counterfactual, empirical, 'Whether the arrangement is a trapped improvement (fix worth having, collectively unreachable) or an adequate equilibrium.').

omega_variable(
    passive_perpetuation_vs_active_defense,
    'Where is the boundary between passive perpetuation (shipping QWERTY as the default because changing defaults is risky) and active defense (maintaining QWERTY to protect a competitive position)?',
    'Internal vendor documentation and procurement records: do layout-default decisions weigh competitor positioning and training-investment moats, or only user-disruption risk and support burden? Cross-vendor comparison of default-change deliberations.',
    'If vendor behavior constitutes strategic defense of a rent position, the story drifts toward the beneficiary_extraction sibling — gain_flow stops being diffuse and enforcement becomes active. If defaults are set by inertia and risk-aversion alone, the lock-in reading holds and no seat captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_perpetuation_vs_active_defense, conceptual, 'Boundary between this reading and the extraction sibling: whether default-setting is inertial or strategic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_tr_t1936, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement(qwerty_lockin_tr_t1955, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1955, 0.11).
narrative_ontology:measurement(qwerty_lockin_tr_t1975, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(qwerty_lockin_tr_t1990, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(qwerty_lockin_tr_t2005, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(qwerty_lockin_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_be_t1936, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1936, 0.13).
narrative_ontology:measurement(qwerty_lockin_be_t1955, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1955, 0.14).
narrative_ontology:measurement(qwerty_lockin_be_t1975, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(qwerty_lockin_be_t1990, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(qwerty_lockin_be_t2005, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(qwerty_lockin_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lockin_su_t1936, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1936, 0.48).
narrative_ontology:measurement(qwerty_lockin_su_t1955, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1955, 0.44).
narrative_ontology:measurement(qwerty_lockin_su_t1975, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(qwerty_lockin_su_t1990, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(qwerty_lockin_su_t2005, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement(qwerty_lockin_su_t2024, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'QWERTY inefficiency.' The label conflates three structurally distinct claims about why the layout persists: (1) naturalization_reading — the layout is adequate and selection was fair (epsilon near zero, no victims); (2) beneficiary_extraction_reading — incumbents actively defend a rent position (concentrated beneficiaries, active enforcement, tangled_rope/snare territory); (3) lock_in_reading (this file) — coordination failure without extraction (diffuse costs, no capturer, piton). Each story carries its own epsilon, beneficiary/victim structure, and claimed type; they are linked here because the readings compete over the same empirical record and cite one another — the lock-in mechanism is the substrate the extraction reading builds on, and the naturalization reading is the null hypothesis both rivals must defeat. Upstream/downstream: this reading's switching-cost structure influences the extraction sibling's intelligibility; the naturalization sibling coexists as the live adequacy claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__lock_in_reading, powerless, 0.5).
constraint_indexing:directionality_override(qwerty_persistence_mechanism__lock_in_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
