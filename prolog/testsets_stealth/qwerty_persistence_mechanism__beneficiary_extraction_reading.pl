% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Incumbent Defense (Beneficiary-Extraction Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   The constraint under authorship is the active-maintenance arrangement
 *   behind QWERTY's persistence: from Remington's early standardization
 *   through the Union Typewriter combination (1893), the typing-school
 *   curriculum economy, mid-century procurement ratification, and the refusal
 *   channel that kept alternative layouts (Dvorak 1936 onward) out of mass
 *   production. This file instantiates ONE reading of the contested kernel
 *   qwerty_persistence_mechanism — the beneficiary-extraction reading, on
 *   which identifiable incumbents actively defended the layout to protect
 *   training investments and market position, and artificial switching costs
 *   transferred surplus to them. The sibling readings (lock-in,
 *   naturalization) are separate constraint files with their own epsilon and
 *   beneficiary structures; per the epsilon-invariance principle they are
 *   linked, not merged. Epsilon's referent here is the standing arrangement
 *   under contest — QWERTY-as-actively-defended — assessed by this reading's
 *   own lights, never the competing-layout regime this reading treats as the
 *   counterfactual. KEY AGENTS (by structural relationship): -
 *   remington_union_trust: Agenda-setting beneficiary
 *   (institutional/arbitrage) — administers the standard, collects the
 *   protected pricing - incumbent_typing_schools: Secondary beneficiary
 *   (organized/identity_locked) — collects tuition on the fixed curriculum -
 *   professional_typists: Primary payer (organized/constrained) — bears the
 *   layout's daily costs with muscle-memory-bound exit - office_employers:
 *   Payer with offsetting coordination gains (powerful/mobile) — buyer power,
 *   collective-action-bound - alternative_layout_inventors: Excluded
 *   innovator (moderate/trapped) — closed channels to market -
 *   keyboard_hardware_ecosystem: Residual beneficiary (organized/mobile) —
 *   collects from the installed base - standards_and_procurement_bodies:
 *   Analytical observer (institutional/analytical) — ratified, did not
 *   originate
 *
 * KEY AGENTS:
 *   - remington_union_trust: agenda-setting beneficiary (institutional/arbitrage) — set and enforced the standard, collected the protected pricing
 *   - incumbent_typing_schools: secondary beneficiary (organized/identity_locked) — tuition economy fused to the fixed curriculum
 *   - professional_typists: primary payer (organized/constrained) — bore the layout's costs; exit bound by muscle memory
 *   - office_employers: payer with offsetting gains (powerful/mobile) — buyer power neutralized by collective-action limits
 *   - alternative_layout_inventors: excluded innovator (moderate/trapped) — no manufacturing or teaching channel
 *   - keyboard_hardware_ecosystem: residual beneficiary (organized/mobile) — accessory sales off the installed base
 *   - standards_and_procurement_bodies: analytical observer (institutional/analytical) — codified the practice mid-century
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.3).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.18).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Incumbent Defense (Beneficiary-Extraction Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'bff76627-4440-4e2f-9abd-b06b3c2b3a88').
narrative_ontology:cs_kernel_codification('bff76627-4440-4e2f-9abd-b06b3c2b3a88', distributed).
narrative_ontology:cs_authority_grounding('bff76627-4440-4e2f-9abd-b06b3c2b3a88', expertise).
narrative_ontology:cs_interpretation_layer_present('bff76627-4440-4e2f-9abd-b06b3c2b3a88').
narrative_ontology:cs_reading_relation('bff76627-4440-4e2f-9abd-b06b3c2b3a88', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('bff76627-4440-4e2f-9abd-b06b3c2b3a88', qwerty_persistence_mechanism__naturalization_reading, influences).
narrative_ontology:cs_axiom('bff76627-4440-4e2f-9abd-b06b3c2b3a88', foundational, persistence_requires_active_defense).
narrative_ontology:cs_axiom_status(persistence_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('bff76627-4440-4e2f-9abd-b06b3c2b3a88', persistence_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('bff76627-4440-4e2f-9abd-b06b3c2b3a88', foundational, artificial_switching_costs_capture_incumbent_surplus).
narrative_ontology:cs_axiom_status(artificial_switching_costs_capture_incumbent_surplus, holdable).
narrative_ontology:cs_axiom_grounding('bff76627-4440-4e2f-9abd-b06b3c2b3a88', artificial_switching_costs_capture_incumbent_surplus, empirically_contingent).
narrative_ontology:cs_reference_frame('bff76627-4440-4e2f-9abd-b06b3c2b3a88', beneficiary_defended_standard).
narrative_ontology:cs_drift_state('bff76627-4440-4e2f-9abd-b06b3c2b3a88', post_revisionist_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bff76627-4440-4e2f-9abd-b06b3c2b3a88', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_trust).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, keyboard_hardware_ecosystem).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, professional_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, path_dependence_first_mover_advantage_doctrine).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, installed_base_switching_cost_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consolidated typewriter patents, tooling, and dealer networks under the Union Typewriter combination from 1893; standardized the QWERTY layout across member brands; declined offers to manufacture or license competing layouts; successor firms inherited the installed base and continued the product strategy. Revenue flowed from machine and supply sales in a market where buyers faced few layout alternatives, and the firm could diversify product lines without abandoning its position.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_trust, agenda_setter,
    institutional, generational, arbitrage, global).

% Built curricula, textbooks, timing drills, and placement services around QWERTY touch-typing; tuition income depended on teaching the layout employers demanded; instructors' certifications and teaching materials were specific to it. Changing course meant rewriting every textbook and retraining every instructor, so the schools defended the existing curriculum and lobbied against alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, identity_locked, national).

% Operated keyboards all day under the fixed layout; their speed and accuracy lived in QWERTY-specific muscle memory, so personally switching layouts meant weeks of lost wages and awkward transition. The common layout also let them change jobs or machines without relearning, and they were never asked to vote on the standard their hands lived under.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, professional_typists, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, professional_typists, beneficiary).

% Purchased machines and hired certified operators; absorbed output losses attributable to the layout and would have borne retraining and downtime costs to move offices to a different one. Gained a fungible labor pool because every school taught the same keys. Large buyers occasionally trialed alternatives but rarely alone, since no single employer could move the training ecosystem by itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers, beneficiary).

% Patented alternative layouts, ran efficiency studies, and sought licensing deals with manufacturers and adoption by schools. Manufacturers with QWERTY tooling declined, and schools tied to QWERTY curricula declined; without a manufacturing channel or a teaching channel, the designs remained demonstrations and their promoters bore the cost of failed commercialization.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors, payer,
    moderate, biographical, trapped, national).

% Makes keycaps, replacement parts, labels, and accessories whose product lines assume the standard legend; inventory and molds are laid out for it. Serves other layouts only as niche items. Collects sales from the enormous installed base without setting any rules and without depending on any enforcement activity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, keyboard_hardware_ecosystem, beneficiary,
    organized, biographical, mobile, global).

% Ratified the layout in national and international standards and wrote it into government procurement specifications mid-century; reviewed efficiency testimony when doing so. Codified an existing practice rather than choosing among alternatives, and takes an analytical distance from the layout question today.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, standards_and_procurement_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_trust).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single keyboard layout lets any trained operator work any machine, lets employers hire from a common certified pool, and lets schools teach one curriculum — the industry-wide compatibility problem is solved once instead of firm by firm.
% TRANSFER_FUNCTION: Moves pricing protection and curriculum revenue to incumbent manufacturers and typing schools — buyers paid prices insulated from layout competition, and students paid for training whose resale value depended on the layout staying fixed — while the costs of the layout's inefficiency and of foregone alternatives fell on typists, employers, and the inventors of competing layouts.
% ABSENT_VOICES: Alternative-layout inventors and independent efficiency researchers stood outside every decision that mattered — manufacturer product committees, school curriculum boards, procurement specification panels. Working typists, the population living with the layout daily, were represented nowhere: the standard was settled by sellers and buyers of equipment, not by the hands that used it.
% DISAPPEARANCE_RATIONALE: Had the defense arrangement vanished at any point in its operative life — say 1936 — alternative-layout machines would have reached dealers, schools would have competed on curricula, and layout choice would have become a live purchase decision rather than a settled fact; the typewriter economy would have reorganized around competing standards. By 2024 the counterfactual converges toward 'unchanged': the enforcement machinery is already gone and the layout persists through installed-base momentum, which is precisely the dead-mandate signal this story documents.
% FOUNDING_PROBLEM: Protecting sunk investments — Remington's production tooling, the combination's patent pool and dealer network, the schools' QWERTY curricula — against displacement by rival layouts, by converting first-mover standardization into durable market position.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: antitrust proceedings against the Union Typewriter combination document the consolidation the arrangement protected; contemporaneous correspondence and Dvorak's published accounts document manufacturer refusals to license competing layouts; economic historians (David 1985 assembling the maintenance narrative; Liebowitz and Margolis 1990/1994 contesting its weight) attest both the existence and the contested status of the defense record. Every beneficiary is defunct, and no party attests that the founding problem is still live.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).
:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All series run on one shared eight-point grid (1873-2024), each tracked metric authored at every point. Base extractiveness traces an inverted U: near zero at adoption (0.15), rising through combination consolidation (0.40 by 1893) and the school-curriculum economy (0.55 by 1913), peaking in the Dvorak-suppression era (0.68-0.70, 1936-1955) when switching costs were most artificial and alternatives most actively excluded, then declining as electronic keyboards made remapping trivial (0.42 by 1995) to a residual 0.30 — ecosystem rents and muscle-memory externalities riding on inertia. Suppression requirement tracks enforcement intensity with the same shape (peak 0.65 mid-century; 0.18 today, when major operating systems ship alternative layouts freely). Theater ratio rises monotonically (0.05 to 0.58): as functional enforcement decayed, the share of QWERTY-defense activity that is rhetorical — origin myths, efficiency rationalizations, habit appeals — grew to majority. Base properties report the end state (2024), consistent with the final measurements. Claim/metric independence: claimed_type is tangled_rope because the arrangement's operative life had all three canonical components — a genuine coordination function (industry-wide layout compatibility), asymmetric incidence (manufacturers and schools collected; typists, employers, and inventors paid), and active enforcement (patent pooling, dealer exclusivity, curriculum lock-step, procurement specification). The end-state metrics show decay toward an inertial profile; that divergence between the claim and the terminal metrics is the lifecycle datum, not an inconsistency to reconcile. Accessibility_collapse is 0.35: alternatives never fully collapsed — Dvorak remained learnable, purchasable, and eventually pre-installed — but adoption friction stayed high throughout. Resistance 0.45: continuous advocacy, studies, and periodic corporate trials met the arrangement without ever displacing it. The trajectory is an inverted U, not cyclical; no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (remington_union_trust) experienced the arrangement as its own asset: a compatibility solution it built and a market position it defended — from that seat the structure presents as legitimate coordination. The payer seats compute otherwise: professional_typists lived the layout's costs with constrained exit, alternative_layout_inventors faced closed channels, and office_employers bore inefficiency and retraining exposure despite holding buyer power. Same-nominal-level actors diverge on asset specificity: employers' capital was generic (mobile exit), typing schools' capital was entirely QWERTY-specific (identity-locked exit), which is why two market actors of comparable standing sit at opposite ends of the resistance profile. Inter-institutionally, manufacturers (institutional, arbitrage) could diversify product lines without abandoning the standard; schools could not pivot curricula without writing off their human capital — the same constraint, different exits, different computed types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (remington_union_trust, incumbent_typing_schools, keyboard_hardware_ecosystem) drive those seats toward the beneficiary end of d; victim declarations (professional_typists, alternative_layout_inventors, office_employers) drive theirs toward the target end, amplified by constrained and trapped exit respectively. One override: office_employers are declared payers, but the structural derivation from that declaration alone would overshoot their target-ward position — they held buyer power, gained a fungible labor pool from the common curriculum, and faced a collective-action problem rather than a barrier; d is overridden to 0.45 (near-symmetric). professional_typists carry a secondary beneficiary role (job mobility across machines) that tempers but does not overturn their target-ward derivation. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the constraint's global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defending mechanical-era manufacturing and training investments — died with the technology it protected, yet the arrangement's residue persists as rhetoric and ecosystem rents. Classifying the operative life as tangled_rope prevents two opposite mislabels: reading it as pure coordination (the naturalization error — ignoring who collected and who was excluded) or as pure snare (ignoring the real compatibility function the defense rode on). The R5 interview records the mismatch directly: founding_problem_status is dead while the arrangement demonstrably rearranged the world during its operative life — the dead-problem-plus-persistence signature. The terminal high-theater, low-enforcement profile is documented rather than tuned away; whether the decayed tail warrants a separate inertial-phase story is carried as the terminal_phase_status omega rather than forced into this file's classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates the beneficiary_extraction_reading of kernel qwerty_persistence_mechanism; do the sibling readings (lock_in_reading, naturalization_reading) instantiate structurally different constraints with different epsilon and different beneficiary/victim sets?',
    'Comparative classification across the three sibling files; the disagreement localizes in causal attribution of persistence (deliberate defense vs. emergent coordination failure vs. adequacy under fair competition) and in whether identifiable beneficiaries captured surplus.',
    'If the readings converge on identical structure, the kernel decomposition is redundant; if they diverge as expected, cross-reading comparison measures how much of QWERTY''s persistence each causal story carries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame routing: this file is one reading of a three-reading kernel, not the kernel itself.').

omega_variable(
    maintenance_evidence_weight,
    'Does the archival record show deliberate, coordinated defense of QWERTY by identifiable beneficiaries (combination governance, licensing refusals, curriculum coordination, procurement lobbying), or only diffuse commercial inertia that the extraction narrative retrospectively organizes?',
    'Systematic archival review: Union Typewriter combination records, manufacturer correspondence regarding Dvorak licensing approaches, typing-school association minutes, government procurement specification files.',
    'Strong documentary defense supports this reading''s structure and its authored suppression series; weak evidence collapses the reading toward the lock_in_reading and lowers effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_evidence_weight, empirical, 'Whether active maintenance is documented or merely inferred from outcomes.').

omega_variable(
    dvorak_superiority_counterfactual,
    'Were the suppressed alternative layouts actually superior by enough that maintaining QWERTY destroyed real surplus for users?',
    'Controlled typing studies and modern ergonomic meta-analyses comparing layouts on speed, error rate, and strain, adjusting for the teacher-effect and study-design confounds identified in the revisionist critique.',
    'If alternatives were not materially superior, the payer seats lose their foregone-surplus claim and this reading''s extraction component shrinks toward ordinary standardization cost; if superior, extraction via artificial switching costs is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_counterfactual, empirical, 'Superiority of the suppressed alternative determines whether the payer seats bear real losses.').

omega_variable(
    terminal_phase_status,
    'Is the post-1990 residue of this arrangement — high theater ratio, near-zero enforcement, ecosystem sales riding on installed-base inertia — a late phase of this same constraint, or a structurally distinct inertial constraint deserving its own story?',
    'Lifecycle drift analysis on the authored series; if the terminal segment classifies differently from the operative segment, split the story at the enforcement-decay inflection circa 1975-1995.',
    'Splitting would assign the terminal residue an inertial-side classification and leave this file covering the operative defense era with a materially higher end-state epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_phase_status, conceptual, 'Whether the arrangement''s decayed tail is the same constraint or a successor one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1873, observed).
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1893, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t1893, observed).
narrative_ontology:measurement(qwer_tr_t1913, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1913, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t1913, observed).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement_basis(qwer_tr_t1936, observed).
narrative_ontology:measurement(qwer_tr_t1955, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement_basis(qwer_tr_t1955, observed).
narrative_ontology:measurement(qwer_tr_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement_basis(qwer_tr_t1975, observed).
narrative_ontology:measurement(qwer_tr_t1995, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(qwer_tr_t1995, observed).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(qwer_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1873, observed).
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1893, 0.4).
narrative_ontology:measurement_basis(qwer_be_t1893, observed).
narrative_ontology:measurement(qwer_be_t1913, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1913, 0.55).
narrative_ontology:measurement_basis(qwer_be_t1913, observed).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1936, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1936, observed).
narrative_ontology:measurement(qwer_be_t1955, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1955, 0.7).
narrative_ontology:measurement_basis(qwer_be_t1955, observed).
narrative_ontology:measurement(qwer_be_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement_basis(qwer_be_t1975, observed).
narrative_ontology:measurement(qwer_be_t1995, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(qwer_be_t1995, observed).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 2024, 0.3).
narrative_ontology:measurement_basis(qwer_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement_basis(qwer_su_t1873, observed).
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1893, 0.35).
narrative_ontology:measurement_basis(qwer_su_t1893, observed).
narrative_ontology:measurement(qwer_su_t1913, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1913, 0.5).
narrative_ontology:measurement_basis(qwer_su_t1913, observed).
narrative_ontology:measurement(qwer_su_t1936, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1936, 0.6).
narrative_ontology:measurement_basis(qwer_su_t1936, observed).
narrative_ontology:measurement(qwer_su_t1955, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1955, 0.65).
narrative_ontology:measurement_basis(qwer_su_t1955, observed).
narrative_ontology:measurement(qwer_su_t1975, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement_basis(qwer_su_t1975, observed).
narrative_ontology:measurement(qwer_su_t1995, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement_basis(qwer_su_t1995, observed).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 2024, 0.18).
narrative_ontology:measurement_basis(qwer_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'QWERTY persistence' into three epsilon-invariant readings of one kernel: this file (beneficiary_extraction_reading — active defense, identifiable beneficiaries, artificial switching costs), qwerty_persistence_mechanism__lock_in_reading (emergent path-dependent coordination failure, no necessary beneficiary), and qwerty_persistence_mechanism__naturalization_reading (adequate design prevailing under fair competition, no victims). The readings differ in epsilon, beneficiary structure, and enforcement profile, so they are separate stories linked by affects_constraints per the BGS family pattern, not one story with a measurement parameter. Evidentiary findings in this file (documented defense or its absence) shift the explanatory share the sibling readings must carry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
