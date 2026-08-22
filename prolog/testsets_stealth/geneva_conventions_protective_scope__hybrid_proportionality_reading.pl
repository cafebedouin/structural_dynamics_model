% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Classification-Scaled Geneva Protections (Hybrid Proportionality Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   The Geneva protective regime scales its protections by conflict
 *   classification: full Convention and AP I standards attach to conflicts
 *   classified international; Common Article 3 and the narrowly-applicable AP
 *   II attach to conflicts classified non-international; and
 *   proportionality-style legal analysis determines which tier governs a
 *   given detention, targeting, or internment decision. This file authors ONE
 *   reading of the contested geneva_conventions_protective_scope kernel — the
 *   hybrid_proportionality_reading, the operating doctrine of practicing
 *   states and tribunals. Two sibling readings are separate constraints with
 *   their own epsilon, beneficiaries, and victims: state_centric_reading
 *   (scope gated on Article 4 combatant criteria) and
 *   universal_rights_reading (CA3 plus human rights law as a universal
 *   floor). The three readings assign different epsilon to the same treaty
 *   texts because they locate the extraction differently; the
 *   epsilon-invariance principle requires separate stories rather than one
 *   story with a measurement parameter. KEY AGENTS (by structural
 *   relationship): militarily_dominant_state_parties — agenda-setter
 *   (institutional/arbitrage), produces classification and proportionality
 *   determinations and collects the interpretive leverage ambiguity affords;
 *   ihl_interpretive_professionals — beneficiary (moderate/identity_locked),
 *   careers constituted by the framework's interpretive complexity;
 *   neutral_humanitarian_intermediaries — beneficiary/observer
 *   (institutional/constrained), collects mandate and access from the tiered
 *   architecture while paying real operational costs;
 *   nongovernmental_armed_groups — payer (powerless/trapped), status
 *   determined by adversaries' classifications;
 *   civilians_in_noninternational_conflicts — payer (powerless/trapped), hold
 *   the thinner tier's protections; detained_unprivileged_belligerents —
 *   payer (powerless/trapped), fall between protection regimes;
 *   weak_state_parties — payer/beneficiary (moderate/constrained), accept
 *   classifications made by better-resourced parties;
 *   human_rights_treaty_bodies — excluded (organized/constrained), argue the
 *   universal floor from outside the apparatus;
 *   international_criminal_tribunals — observer (institutional/analytical),
 *   adjudicate tier questions after the fact.
 *
 * KEY AGENTS:
 *   - militarily_dominant_state_parties: Agenda-setter (institutional/arbitrage) — runs classification and proportionality determinations; collects the interpretive leverage that ambiguity affords
 *   - ihl_interpretive_professionals: Beneficiary (moderate/identity_locked) — military lawyers, academics, and NGO advisers whose livelihoods depend on the framework's complexity
 *   - neutral_humanitarian_intermediaries: Beneficiary/observer (institutional/constrained) — ICRC-style mandate holders collecting access rights while delivering services and bearing casualties
 *   - nongovernmental_armed_groups: Payer (powerless/trapped) — members' status fixed by classifications they cannot make or contest
 *   - civilians_in_noninternational_conflicts: Payer (powerless/trapped) — receive the thinner tier's protections
 *   - detained_unprivileged_belligerents: Payer (powerless/trapped) — held between POW and full civilian regimes
 *   - weak_state_parties: Payer/beneficiary (moderate/constrained) — ratify the full package but inherit classifications made by others
 *   - human_rights_treaty_bodies: Excluded (organized/constrained) — assert concurrent-application floor without a seat in the classification apparatus
 *   - international_criminal_tribunals: Observer (institutional/analytical) — resolve tier questions retrospectively on party-assembled records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.66).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Classification-Scaled Geneva Protections (Hybrid Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '9a4faf19-45c7-48b9-86ce-569614632be2').
narrative_ontology:cs_kernel_codification('9a4faf19-45c7-48b9-86ce-569614632be2', fixed_text).
narrative_ontology:cs_authority_grounding('9a4faf19-45c7-48b9-86ce-569614632be2', distributed).
narrative_ontology:cs_reading_relation('9a4faf19-45c7-48b9-86ce-569614632be2', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a4faf19-45c7-48b9-86ce-569614632be2', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_axiom('9a4faf19-45c7-48b9-86ce-569614632be2', foundational, protections_scale_with_conflict_typology).
narrative_ontology:cs_axiom_status(protections_scale_with_conflict_typology, holdable).
narrative_ontology:cs_axiom_grounding('9a4faf19-45c7-48b9-86ce-569614632be2', protections_scale_with_conflict_typology, conventional).
narrative_ontology:cs_axiom('9a4faf19-45c7-48b9-86ce-569614632be2', foundational, proportionality_balancing_determines_application).
narrative_ontology:cs_axiom_status(proportionality_balancing_determines_application, holdable).
narrative_ontology:cs_axiom_grounding('9a4faf19-45c7-48b9-86ce-569614632be2', proportionality_balancing_determines_application, instrumental).
narrative_ontology:cs_reference_frame('9a4faf19-45c7-48b9-86ce-569614632be2', typological_graduated_protection).
narrative_ontology:cs_drift_state('9a4faf19-45c7-48b9-86ce-569614632be2', contemporary_classification_contest, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a4faf19-45c7-48b9-86ce-569614632be2', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_state_parties).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, ihl_interpretive_professionals).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, neutral_humanitarian_intermediaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, nongovernmental_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_noninternational_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, detained_unprivileged_belligerents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, weak_state_parties).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weak_state_parties).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_balancing_method).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, graduated_obligation_ratification_strategy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Conventions and Protocols and staff the legal-adviser offices that produce classification determinations for each operation. When their forces detain, target, or intern, their lawyers select the applicable tier — international or non-international — and run the proportionality assessments that determine what protections attach. Ambiguity in both steps lets them argue for the lighter tier without ever formally repudiating the treaties; reservation, reinterpretation, and withdrawal clauses remain open doors. They fund and host the diplomatic conferences where scope language is negotiated.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_state_parties, agenda_setter,
    institutional, generational, arbitrage, global).

% Military lawyers, academy professors, NGO legal advisers, and treaty-body experts whose careers consist of arguing classification and proportionality questions. The tiered structure generates the interpretive problems that employ them; a flat universal rule would shrink the field. Their professional standing is bound to the framework's complexity — leaving it means leaving the profession.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, ihl_interpretive_professionals, beneficiary,
    moderate, biographical, identity_locked, global).

% The ICRC and comparable neutral intermediaries hold treaty-defined mandates — visiting detainees, tracing the missing, proposing custody arrangements — that exist only because the Conventions assign them roles tier by tier. They deliver real protective services and pay operational costs, including delegate casualties, while collecting institutional standing and access rights from the same architecture. Their neutrality commitment bars them from advocating replacement of the framework they operate inside.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, neutral_humanitarian_intermediaries, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, neutral_humanitarian_intermediaries, observer).

% Fight in conflicts whose classification is determined largely by the state opponent and external patrons. Their members' status — privileged fighter, unprivileged belligerent, detainee with or without procedural rights — follows from a determination they cannot make or contest. Common Article 3 binds them in theory, but its thresholds and consequences arrive pre-interpreted by better-resourced adversaries.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, nongovernmental_armed_groups, payer,
    powerless, immediate, trapped, regional).

% Live under the thinner tier: when a conflict is classified non-international, the detailed civilian protections of the fourth Convention and AP I do not attach, and they hold whatever Common Article 3 and AP II — with its high applicability threshold — provide. They did not choose the classification and have no forum in which to appeal it; displacement, siege, and detention conditions follow from determinations made elsewhere.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_noninternational_conflicts, payer,
    powerless, immediate, trapped, regional).

% Held in facilities whose regime depends on how the detaining power classified the conflict and their status. Where the classification is non-international or their status unprivileged, they fall between POW and full civilian protections — no POW-status tribunal process, reduced procedural guarantees, open-ended internment arguments. Their access to review is mediated by the same legal machinery that determined their status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, detained_unprivileged_belligerents, payer,
    powerless, immediate, trapped, national).

% Ratified the full treaty package but lack the legal-adviser capacity to run classification and proportionality analyses at scale. When conflicts touch their territory, classifications proposed by stronger parties or tribunals tend to stick; they receive the Common Article 3 floor as protection but bear compliance scrutiny priced for better-resourced states.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weak_state_parties, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, weak_state_parties, beneficiary).

% Monitor human rights conventions that they read as applying concurrently in armed conflict, producing a universal floor that would flatten the tiered structure. They publish conclusions and hear petitions from people inside classified conflicts, but hold no seat in the Geneva classification apparatus; their concurrence doctrine is acknowledged, resisted, and never incorporated by the treaty parties who run the tiers.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, human_rights_treaty_bodies, excluded,
    organized, generational, constrained, global).

% Adjudicate war-crimes charges that require deciding which tier governed — the Tadić jurisdictional analysis is the leading instance. Their classifications bind the accused before them and radiate persuasive force outward, but they review classifications after the fact, on records assembled by the parties whose conduct is at issue.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, militarily_dominant_state_parties).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the participation problem in limiting warfare: graduated obligations let states with divergent security interests and internal-affairs sensitivities accept binding limits — near-universal ratification followed where earlier uniform-standard efforts failed — and the classification step tells every party which rulebook governs a given fight, giving operations predictable legal parameters.
% TRANSFER_FUNCTION: Moves protective status and legal exposure between classes of persons according to conflict classification: full detention and targeting protections flow to those inside classified international conflicts; reduced Common Article 3 floors to those inside classified non-international ones; and the classification and proportionality determinations themselves concentrate interpretive authority over life-and-death status in the hands of whichever party fields the stronger legal machinery.
% ABSENT_VOICES: Persons whose status the classification fixes — detainees, civilians under the thinner tier, non-state fighters — have no seat in any classification proceeding; human rights treaty bodies argue the concurrent-application floor from outside the apparatus and are acknowledged but never seated; affected populations cannot appeal the determination that sets their protection level.
% DISAPPEARANCE_RATIONALE: Overnight removal would leave every party facing an all-or-nothing choice between full Convention obligations and no treaty limits; the web of status determinations, ICRC access mandates, tribunal jurisdictional rulings, and detention-regime practices built on the tiers would lose its organizing frame, and states that accepted limits only because the tiers made acceptance affordable would confront ratification politics anew.
% FOUNDING_PROBLEM: After uniform-standard efforts in 1864, 1906, and 1929 failed to reach internal conflicts, the 1949 drafters faced a participation problem: major powers would not accept maximal obligations in civil wars or colonial contexts, yet some limit was urgently needed; protections were therefore scaled by conflict type to buy near-universal ratification.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 Diplomatic Conference records corroborate the historical account — small-state delegations and ICRC interventions pressed for broader coverage while major powers insisted on typological limits — and independent academic IHL scholarship confirms the ratification logic. But no source outside the benefiting parties attests that scaling remains necessary today: human rights treaty bodies and the ICRC customary-IHL study explicitly dispute it, treating the floor as universal in substance; the parties who benefit from classification discretion are the only voices asserting continued necessity.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 at interval end: the tiered structure delivers real protection — the Common Article 3 floor was a genuine advance over the pre-1949 law-free zone in civil wars — while attaching the full standard only where classification places it, and both the classification step and the proportionality step are run by the party with the strongest legal machinery. Suppression is 0.58: the constraint does not physically bar alternatives, but a detainee cannot opt into the fuller tier, and the consent-based treaty architecture resists replacement — suppression here is structural (treaty lock-in, jurisdictional gatekeeping) rather than violent. Theater ratio 0.38: periodic compliance reporting, anniversary conferences, and mutual-observation rituals perform commitment while classification practice continues underneath; ICRC visits and tribunal prosecutions remain functional, keeping theater below half. Accessibility collapse 0.45: workable alternatives persist — the concurrent human rights floor, the customary-IHL project, universal-rights advocacy — but none has displaced the tiered apparatus. Resistance 0.57: sustained doctrinal resistance from human rights bodies and scholarship, met by periodic state pushback against expansion. The measurement series run on one shared grid (1949-2025, eight points) so every tracked metric is authored at every examined time point; trajectories rise monotonically rather than cyclically, tracking the intensification of classification contests (decolonization wars, AP II's deliberately narrow 1977 applicability threshold, the post-2001 detention-classification disputes, hybrid conflicts in Syria and Ukraine). Suppression_requirement is tracked because this story specifically traces enforcement-machinery growth: compliance inquiries, tribunal jurisdiction, and access negotiations hardened over the interval as classification disputes multiplied. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the tiers are a hard-won compromise that made limiting warfare ratifiable at all — the arrangement looks like its own best justification. From the payer seats the same tiers are a lottery run on other people's status: whether a detainee receives POW process depends on a classification produced by the detaining power's own lawyers, and whether civilians under siege hold AP I protections depends on a characterization announced by a foreign ministry. The engine computes this divergence from the structural data; the divergence is the finding, not noise to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily dominant state parties sit nearest the beneficiary pole: they collect classification leverage and pay little they did not themselves agree to, with arbitrage-grade exit (reservation, reinterpretation, withdrawal clauses). Interpretive professionals and humanitarian intermediaries also sit beneficiary-side — the former collect careers from complexity, the latter collect mandate and access while bearing real operational costs, which keeps them off the extreme pole. The three payer groups sit near the target pole: their protection level is set by determinations they cannot make, contest, or exit, and trapped exit pushes them toward full-target directionality. Weak state parties sit mid-range — they receive the Common Article 3 floor as incidental beneficiaries but bear compliance scrutiny and classification outcomes set by better-resourced actors. Larger spatial scope amplifies effective extraction for the payer seats because verification of tier-compliance across active conflict zones is hardest exactly where they stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the regime as pure snare misses why nearly every state accepted any limits: the graduated structure solved a real participation problem that uniform-standard efforts repeatedly failed to solve, and the Common Article 3 floor is invoked daily in operational practice. Reading it as pure rope misses the extraction: the same classification step that made ratification affordable lets the strongest party select the lighter tier precisely when its conduct would fail the heavier one, and proportionality indeterminacy converts a balancing method into a post-hoc justification surface. The founding-problem interview locates the tension honestly: the founding problem (buying participation) is historically corroborated by sources outside the benefiting parties, but its continued necessity is disputed by every such source today, while the world demonstrably rearranges around the tiers — the mismatch pattern the battery watches for sits at 'contested', not resolved, and the story declines to launder that contest into either a clean rope or a clean snare verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the hybrid_proportionality_reading of the geneva_conventions_protective_scope kernel; how would the victim set and extraction profile change under the sibling readings?',
    'Generate the sibling stories (state_centric_reading, universal_rights_reading) and compare victim sets, epsilon, and classification structure across the three files; the disagreement is located in whether conflict classification legitimately gates protective scope.',
    'Under state_centric_reading the protected class shrinks further (unprivileged belligerents fall out entirely) and extraction concentrates on status exclusions; under universal_rights_reading the classification gate disappears, epsilon collapses toward the coordination-cost residual, and the type moves toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the protective-scope kernel; sibling readings alter the victim set and epsilon.').

omega_variable(
    proportionality_indeterminacy_share,
    'How much of the measured extraction flows from proportionality analysis''s genuine balancing necessity versus its exploitable indeterminacy?',
    'Code classification and proportionality determinations across tribunals and state practice for consistency, reversal rates, and correlation with the determining party''s operational interests.',
    'If indeterminacy dominates, effective extraction rises and the type drifts snare-ward; if necessity dominates, a larger share of epsilon is coordination cost inherent to calibrated application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_indeterminacy_share, empirical, 'Splitting epsilon between balancing necessity and exploitable indeterminacy.').

omega_variable(
    classification_gate_necessity,
    'Was conflict-type scaling structurally necessary to achieve treaty participation, or would uniform standards have ratified comparably?',
    'Counterfactual analysis of the 1864/1906/1929 ratification failures against 1949''s success, plus the natural experiment of human rights law''s concurrent application achieving floor-level coverage without tiering.',
    'If scaling was unnecessary, the tiering functions as cover for selective interpretation and the snare share grows; if necessary, the coordination leg of the tangled rope is load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_gate_necessity, empirical, 'Whether the tiered structure''s coordination function is genuine or retrospective cover.').

omega_variable(
    victim_set_fluctuation_status,
    'Does the victim set''s variation by conflict classification reflect a morally relevant difference between conflict types, or an artifact of the stronger party''s interpretive leverage?',
    'Compare protection outcomes for structurally similar persons (detainees, urban civilians under siege) across conflicts classified differently, controlling for battlefield conditions.',
    'If outcomes diverge mainly with classification rather than conditions, epsilon should be assessed per-classification and the asymmetry attributed to interpretive leverage; if conditions drive outcomes, the scaling tracks real differences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_fluctuation_status, conceptual, 'Whether classification-correlated protection gaps track moral relevance or leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcps_hybrid_tr_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1949, 0.16).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t1949, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t1960, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1960, 0.19).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t1960, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.23).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t1977, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t1990, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t1999, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1999, 0.3).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t1999, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t2005, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t2005, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t2014, observed).
narrative_ontology:measurement(gcps_hybrid_tr_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(gcps_hybrid_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gcps_hybrid_be_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1949, 0.44).
narrative_ontology:measurement_basis(gcps_hybrid_be_t1949, observed).
narrative_ontology:measurement(gcps_hybrid_be_t1960, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1960, 0.47).
narrative_ontology:measurement_basis(gcps_hybrid_be_t1960, observed).
narrative_ontology:measurement(gcps_hybrid_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.51).
narrative_ontology:measurement_basis(gcps_hybrid_be_t1977, observed).
narrative_ontology:measurement(gcps_hybrid_be_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(gcps_hybrid_be_t1990, observed).
narrative_ontology:measurement(gcps_hybrid_be_t1999, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1999, 0.58).
narrative_ontology:measurement_basis(gcps_hybrid_be_t1999, observed).
narrative_ontology:measurement(gcps_hybrid_be_t2005, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(gcps_hybrid_be_t2005, observed).
narrative_ontology:measurement(gcps_hybrid_be_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement_basis(gcps_hybrid_be_t2014, observed).
narrative_ontology:measurement(gcps_hybrid_be_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(gcps_hybrid_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gcps_hybrid_su_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement_basis(gcps_hybrid_su_t1949, observed).
narrative_ontology:measurement(gcps_hybrid_su_t1960, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1960, 0.43).
narrative_ontology:measurement_basis(gcps_hybrid_su_t1960, observed).
narrative_ontology:measurement(gcps_hybrid_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.47).
narrative_ontology:measurement_basis(gcps_hybrid_su_t1977, observed).
narrative_ontology:measurement(gcps_hybrid_su_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(gcps_hybrid_su_t1990, observed).
narrative_ontology:measurement(gcps_hybrid_su_t1999, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1999, 0.53).
narrative_ontology:measurement_basis(gcps_hybrid_su_t1999, observed).
narrative_ontology:measurement(gcps_hybrid_su_t2005, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement_basis(gcps_hybrid_su_t2005, observed).
narrative_ontology:measurement(gcps_hybrid_su_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement_basis(gcps_hybrid_su_t2014, observed).
narrative_ontology:measurement(gcps_hybrid_su_t2025, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(gcps_hybrid_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Geneva protections' decomposes into three readings of one kernel with distinct epsilon. This file (hybrid_proportionality_reading) authors the classification-scaled account; state_centric_reading authors the status-gated account; universal_rights_reading authors the floor-universal account. The shared upstream fact (treaty ratification history) feeds all three; the universal reading exerts reverse pressure on this one through concurrent-application doctrine. Each story carries its own epsilon, beneficiaries, and victims; links here propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
