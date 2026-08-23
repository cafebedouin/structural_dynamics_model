% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto (Reading)
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the minoritarian_veto_reading of the
 *   supermajority_threshold kernel. The kernel is the constitutional text
 *   specifying a supermajority (typically 2/3 or 3/4) for amendment. Three
 *   readings contest it: consensus_safeguard_reading (threshold ensures deep
 *   consensus), adaptive_gradient_reading (threshold should be
 *   evidence-tuned), and this reading — minoritarian_veto_reading — which
 *   holds that the threshold's actual operation is converting historical
 *   privilege into permanent veto against majoritarian reform. The constraint
 *   is NOT the text itself but the standing arrangement under contest: a veto
 *   geometry that lets a shrinking minority bind an expanding majority.
 *   Beneficiaries are identifiable (entrenched elites, status quo
 *   institutions, historical privilege holders); victims are contemporary and
 *   future majorities blocked from necessary adaptation. The coordination
 *   story (deep consensus filter) is cover; the extraction is structural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.82).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.88).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto (Reading)").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '68a8c000-a9b5-4063-8e14-3cafa9576687').
narrative_ontology:cs_kernel_codification('68a8c000-a9b5-4063-8e14-3cafa9576687', formalized).
narrative_ontology:cs_authority_grounding('68a8c000-a9b5-4063-8e14-3cafa9576687', lineage).
narrative_ontology:cs_interpretation_layer_present('68a8c000-a9b5-4063-8e14-3cafa9576687').
narrative_ontology:cs_reading_relation('68a8c000-a9b5-4063-8e14-3cafa9576687', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('68a8c000-a9b5-4063-8e14-3cafa9576687', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('68a8c000-a9b5-4063-8e14-3cafa9576687', foundational, supermajority_threshold_systematically_entrenches_minority_veto).
narrative_ontology:cs_axiom_status(supermajority_threshold_systematically_entrenches_minority_veto, holdable).
narrative_ontology:cs_axiom_grounding('68a8c000-a9b5-4063-8e14-3cafa9576687', supermajority_threshold_systematically_entrenches_minority_veto, empirically_contingent).
narrative_ontology:cs_axiom('68a8c000-a9b5-4063-8e14-3cafa9576687', secondary, consensus_safeguard_narrative_is_ideological_cover).
narrative_ontology:cs_axiom_status(consensus_safeguard_narrative_is_ideological_cover, holdable).
narrative_ontology:cs_axiom_grounding('68a8c000-a9b5-4063-8e14-3cafa9576687', consensus_safeguard_narrative_is_ideological_cover, empirically_contingent).
narrative_ontology:cs_reference_frame('68a8c000-a9b5-4063-8e14-3cafa9576687', original_framing_anti_majoritarian_safeguard).
narrative_ontology:cs_drift_state('68a8c000-a9b5-4063-8e14-3cafa9576687', contemporary_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('68a8c000-a9b5-4063-8e14-3cafa9576687', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, historical_privilege_holders).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_seeking_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, future_generations).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, minority_veto_as_constitutional_essence).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, originalist_entrenchment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold disproportionate influence over the blocking minority position; their wealth, institutional access, and network centrality let them shape which amendments reach the threshold and which are strangled. They do not administer the threshold but capture its veto yield.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Institutions, industries, and professional guilds whose regulatory privileges, tax advantages, or market protections survive only because the supermajority barrier makes repeal structurally impossible. They mobilize to defend the threshold as 'stability' while it functions as their moat.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, constrained, national).

% Groups whose constitutional standing was baked into the founding settlement (e.g., malapportioned legislative chambers, special jurisdictional carve-outs). The threshold converts their historical accident into a permanent veto; their identity fuses with the arrangement so exit is inconceivable.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, historical_privilege_holders, beneficiary,
    moderate, generational, identity_locked, national).

% Electoral majorities that consistently support reforms (voting rights restoration, campaign finance limits, climate legislation, healthcare expansion) but watch them die at the supermajority wall. Their exit option is constitutional amendment — which the threshold makes mathematically implausible.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    moderate, biographical, constrained, national).

% Social movements, advocacy coalitions, and legislative majorities that build supermajority-scale support only to discover the threshold's real geography: it counts not votes but veto points. They are payers (investment lost) and excluded (their victories are nullified by the rule they cannot change).
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_seeking_majorities, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, reform_seeking_majorities, excluded).

% Bear the compounding cost of blocked adaptation (climate, inequality, democratic decay) with zero voice in the threshold's calibration. They would object to a rule that lets a shrinking minority bind an expanding majority forever — but they are not in the room.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Groups whose rights require constitutional expansion (disenfranchised populations, territories without representation, communities targeted by entrenched majoritarian structures). The threshold that 'protects minorities' in the consensus_safeguard_reading is the same threshold that blocks their inclusion.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, marginalized_groups, excluded,
    powerless, generational, trapped, national).

% Adjudicate the threshold's scope: what counts as an 'amendment' vs. 'revision', whether popular initiatives can bypass it, whether legislative supermajorities suffice. Their jurisprudence has gradually narrowed the amendment path, hardening the veto.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislatures, conventions, or popular initiative processes that formally propose amendments. They operate the machinery but the threshold sets the bar; their agenda-setting power is real but bounded by the veto geometry they did not design and cannot alone lower.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, amendment_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% Study the threshold's empirical effects: veto frequency, amendment failure rates, policy drift, democratic legitimacy erosion. Their findings consistently show the threshold blocks reform more than it protects rights — but the arrangement persists.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, political_scientists, observer,
    analytical, civilizational, analytical, global).

% Produce the rival readings (consensus_safeguard, adaptive_gradient, minoritarian_veto). The dispute among them IS the constraint's ideological superstructure; no reading has displaced the others in the academy, and the threshold survives the stalemate.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to coordinate deep democratic consensus by filtering transient passions; this reading finds no evidence it performs that function — the supermajority threshold does not distinguish deep from shallow majorities, it simply empowers any blocking minority that meets the count.
% TRANSFER_FUNCTION: Transfers effective constitutional agency from contemporary majorities (who must assemble supermajorities to reform) to blocking minorities (who need only hold one-third-plus-one to veto). The resource transferred is the power to bind the future; the transfer is structural, not voluntary.
% ABSENT_VOICES: Future generations and marginalized groups who need constitutional adaptation are structurally excluded — they cannot vote, lobby, or litigate the threshold itself. Their absence is not accidental; the threshold's geometry ensures they never reach the veto point.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, constitutional reform would revert to majoritarian pathways: voting rights acts, climate legislation, campaign finance reform, and democratic structural repairs would pass legislatures and survive judicial review. The veto geometry that currently blocks them would collapse; the policy landscape would reorganize around contemporary majorities within 1-2 electoral cycles.
% FOUNDING_PROBLEM: The founding problem was fear of majority tyranny: the framers believed unchecked majorities would expropriate minorities, destabilize property, and oscillate wildly. The supermajority threshold was designed as a cooling saucer — a structural brake on passionate but shallow majorities.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (majority tyranny) is attested as still live by the consensus_safeguard_reading's proponents and by judicial opinions citing 'protection of minority rights.' It is attested as dead or weaponized by political scientists documenting that the threshold now blocks majoritarian reforms that *protect* minorities (voting rights, anti-gerrymandering, DC statehood) while protecting entrenched *majorities* (malapportionment, filibuster, electoral college). Historical analysis from outside the beneficiary set (Ackerman, Levinson, Lutz) corroborates the drift from safeguard to veto.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.82: the threshold transfers constitutive power from majorities to minorities without compensation — the resource is democratic agency itself. Suppression 0.88: alternatives (reform via amendment) are not merely discouraged but mathematically foreclosed by the veto count; the constraint's persistence depends on this foreclosure, not on participant buy-in. Theater 0.45: the consensus-safeguard narrative is real and sincerely held by many, but a growing share of the threshold's operation serves veto entrenchment, not consensus-filtering. Accessibility_collapse 0.78: once the veto geometry is understood, the alternative (majoritarian amendment) collapses — you cannot 'choose' a lower threshold. Resistance 0.62: reform movements resist but their resistance is channeled into the very supermajority process that blocks them, creating a recursive trap.
 *
 * PERSPECTIVAL GAP:
 *   From the consensus_safeguard seat, the constraint is a rope (genuine coordination, minimal coercion). From the minoritarian_veto seat, it is a snare (pure extraction, high suppression). From the adaptive_gradient seat, it is a tangled_rope (coordination function exists but is miscalibrated, requiring active tuning). The engine computes these seat divergences from the structural data — the authored claim (snare) reflects this reading's assessment; the engine will verify whether the metrics and structure support it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (entrenched_elites, status_quo_beneficiaries, historical_privilege_holders) sit at d ≈ 0.1-0.2: they collect veto rents, have arbitrage-grade exit (they can work the system or leave it), and the constraint subsidizes their position. Victims (contemporary_majorities, reform_seeking_majorities, future_generations) sit at d ≈ 0.85-0.95: they bear the full cost of blocked reform, have constrained or trapped exit (amendment is the only path and the threshold blocks it), and the constraint extracts their agency. Constitutional_courts and amendment_bodies (agenda_setters) sit near d ≈ 0.5: they administer the constraint and gain institutional authority from it, but are also constrained by its geometry — they cannot unilaterally lower the bar. Excluded voices (future_generations, marginalized_groups) sit at d ≈ 1.0: pure extraction with zero voice. Observers sit at d = 0.5 (analytical symmetry).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (majority tyranny) is contested: live for some threats, dead for others, weaponized for the threshold itself. The arrangement persists not because the problem is live but because the beneficiaries of the veto (entrenched elites, status quo institutions) control the amendment pathway that could fix it. Mandatrophy is unresolved: the threshold's mandate has outlived its coordination function (if it ever had one) and now serves as a lock-in mechanism. The mandate is not formally resolved — it is structurally immune to resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the supermajority threshold perform ANY genuine coordination function (filtering transient majorities), or is its entire operation extraction masquerading as coordination?',
    'Counterfactual amendment history: simulate which failed amendments would have passed under majoritarian rules and assess whether they represented ''transient passions'' or ''durable majorities blocked by veto geometry.'' Compare amendment failure rates across policy domains.',
    'If genuine coordination exists, the constraint is tangled_rope (coordination + extraction). If zero coordination, it is pure snare. The theater_ratio trajectory (rising) suggests coordination is decaying or was never the dominant function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the constraint has a non-zero coordination core.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the threshold''s suppression structural (the veto count itself) or internalized (reform actors self-censor, build supermajority coalitions that still fail, treat the threshold as natural law)?',
    'Measure reform proposal volume before and after threshold salience events (e.g., failed amendment campaigns, court rulings). If proposals drop without formal suppression, internalized suppression is significant.',
    'If internalized, effective suppression exceeds the structural measure — the target carries the veto inside them. This would raise the constraint''s effective classification severity for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in constitutional veto geometries.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the supermajority_threshold kernel. How does the minoritarian_veto_reading''s structural claim (snare) relate to the sibling readings'' claims (rope/tangled_rope)?',
    'The kernel contest is resolved not by evidence alone but by which reading''s axioms gain institutional uptake. Track citation networks in constitutional jurisprudence and legislative practice: which reading''s normative framework guides actual amendment politics?',
    'If minoritarian_veto_reading''s axioms displace consensus_safeguard_reading in institutional practice, the kernel''s effective classification shifts toward snare across seats. If consensus_safeguard_reading retains institutional authority, the constraint remains contested with seat-divergent classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel reading contestation and its structural consequences.').

omega_variable(
    historical_privilege_conversion_mechanism,
    'By what specific mechanism does the threshold convert historical privilege into permanent veto? Is it the interaction of malapportionment + supermajority, or the threshold alone?',
    'Compare veto outcomes in jurisdictions with supermajority thresholds but without malapportionment vs. those with both. Isolate the threshold''s independent contribution to minority veto power.',
    'If the threshold alone generates veto entrenchment, the snare classification holds universally. If it requires malapportionment as force multiplier, the constraint family includes a coupled malapportionment story (affects_constraints link).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_privilege_conversion_mechanism, empirical, 'Mechanism of historical privilege conversion to permanent veto.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smt_mvr_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(smt_mvr_tr_t47, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 47, 0.22).
narrative_ontology:measurement(smt_mvr_tr_t94, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 94, 0.31).
narrative_ontology:measurement(smt_mvr_tr_t141, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 141, 0.38).
narrative_ontology:measurement(smt_mvr_tr_t188, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 188, 0.42).
narrative_ontology:measurement(smt_mvr_tr_t235, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 235, 0.45).

% Extraction over time
narrative_ontology:measurement(smt_mvr_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smt_mvr_be_t47, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 47, 0.42).
narrative_ontology:measurement(smt_mvr_be_t94, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 94, 0.51).
narrative_ontology:measurement(smt_mvr_be_t141, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 141, 0.63).
narrative_ontology:measurement(smt_mvr_be_t188, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 188, 0.74).
narrative_ontology:measurement(smt_mvr_be_t235, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 235, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(smt_mvr_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(smt_mvr_su_t47, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 47, 0.71).
narrative_ontology:measurement(smt_mvr_su_t94, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 94, 0.76).
narrative_ontology:measurement(smt_mvr_su_t141, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 141, 0.81).
narrative_ontology:measurement(smt_mvr_su_t188, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 188, 0.85).
narrative_ontology:measurement(smt_mvr_su_t235, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 235, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, malapportionment_veto_coupling).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, judicial_review_entrenchment).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, federalism_veto_points).

% DUAL FORMULATION NOTE:
% This story is the minoritarian_veto_reading of the supermajority_threshold kernel. The consensus_safeguard_reading claims rope (genuine coordination); the adaptive_gradient_reading claims tangled_rope (miscalibrated coordination requiring tuning). This reading claims snare (pure extraction). The three stories form a constraint family linked by affects_constraints. The ε values diverge: consensus_safeguard ε ≈ 0.15, adaptive_gradient ε ≈ 0.35, minoritarian_veto ε = 0.82 (this story). The divergence is the measurement — the kernel's label conceals structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
