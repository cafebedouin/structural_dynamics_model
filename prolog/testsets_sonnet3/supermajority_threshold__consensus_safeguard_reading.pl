% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Amendment Threshold — Consensus Safeguard Reading
 *   domain: constitutional_theory/political_economy
 *
 * SUMMARY:
 *   This story instantiates the consensus_safeguard_reading of the
 *   supermajority_threshold kernel: the claim that a high amendment bar
 *   exists to filter constitutional change for depth and durability of
 *   agreement, protecting the polity from decisions made in moments of
 *   transient majoritarian enthusiasm. On this reading the beneficiary set is
 *   diffuse (the whole polity, future generations, minority communities
 *   protected from majoritarian override) and there is no specific identified
 *   victim class in ordinary operation — a would-be reform majority that
 *   cannot clear the bar is not a 'victim' on this reading, since the whole
 *   point is that unproven consensus should not translate into permanent
 *   constitutional change. This is a deliberately different constraint from
 *   the minoritarian_veto_reading (which locates the same threshold's
 *   function in entrenching a blocking minority's historical privilege) and
 *   from the adaptive_gradient_reading (which treats the threshold as a
 *   tunable instrument requiring empirical calibration rather than a fixed
 *   quality filter). Each reading has its own epsilon and its own stakeholder
 *   structure; they are linked only through the network layer, never merged
 *   into one classification.
 *
 * KEY AGENTS:
 *   - constitutional_order_beneficiaries: diffuse citizenry (moderate/constrained) — benefits from institutional predictability
 *   - future_generations: unborn/unenfranchised (powerless/trapped) — inherits the locked-in framework
 *   - minority_constitutional_communities: regional/religious/linguistic minorities (moderate/constrained) — protected from majoritarian override
 *   - legislative_supermajority_coalition_builders: agenda_setter (organized/constrained) — must build durable cross-partisan coalitions
 *   - transient_reform_majorities: excluded (organized/constrained) — cannot prevail through ordinary majoritarian channels
 *   - constitutional_scholars: analytical observer — studies the empirical fit between threshold and durability claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.22).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.35).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Amendment Threshold — Consensus Safeguard Reading").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '724a4dd3-e994-461a-a2e1-b0b6cd344d4f').
narrative_ontology:cs_kernel_codification('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', formalized).
narrative_ontology:cs_authority_grounding('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', lineage).
narrative_ontology:cs_interpretation_layer_present('724a4dd3-e994-461a-a2e1-b0b6cd344d4f').
narrative_ontology:cs_reading_relation('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', foundational, durable_supermajority_agreement_signals_legitimate_deep_consensus).
narrative_ontology:cs_axiom_status(durable_supermajority_agreement_signals_legitimate_deep_consensus, holdable).
narrative_ontology:cs_axiom_grounding('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', durable_supermajority_agreement_signals_legitimate_deep_consensus, conventional).
narrative_ontology:cs_axiom('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', foundational, transient_majority_preference_is_insufficient_warrant_for_constitutional_change).
narrative_ontology:cs_axiom_status(transient_majority_preference_is_insufficient_warrant_for_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', transient_majority_preference_is_insufficient_warrant_for_constitutional_change, deontological).
narrative_ontology:cs_reference_frame('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', framers_deliberative_consensus_design).
narrative_ontology:cs_drift_state('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('724a4dd3-e994-461a-a2e1-b0b6cd344d4f', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_order_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_generations).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, minority_constitutional_communities).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, deep_consensus_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, counter_majoritarian_stability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The general citizenry living under a constitutional order whose fundamental terms are not subject to rapid reversal by transient legislative majorities. They benefit from predictable rules of the political game, protection of entrenched rights against momentary passions, and confidence that basic commitments (federal structure, rights guarantees, institutional design) will hold across electoral cycles. Their exit from this arrangement means living under a constitution that could be rewritten by whatever coalition currently holds a bare majority.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_order_beneficiaries, beneficiary,
    moderate, civilizational, constrained, national).

% Not yet born or not yet enfranchised, they cannot participate in the current constitutional bargain but inherit whatever framework the supermajority threshold locks in place. The high bar filters out amendments driven by short-lived political enthusiasms that a current majority might regret imposing on people who never consented, on this reading protecting them from decisions made in moments of passion rather than durable reflection.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, national).

% Regional, religious, linguistic, or political minorities whose foundational protections (federalism guarantees, rights clauses, representation formulas) would be vulnerable to erosion by a simple majority coalition. The threshold requires broad cross-cutting agreement before their protections can be altered, which on this reading is precisely the point: it forces proponents of change to build consensus that includes minority buy-in rather than simply outvoting them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, minority_constitutional_communities, beneficiary,
    moderate, generational, constrained, national).

% Political actors who must assemble the broad coalitions needed to clear the threshold. They administer the amendment process by structuring proposals, negotiating concessions, and building the durable cross-partisan support the mechanism demands. Their path to constitutional change runs only through sustained coalition-building, not through a single election cycle's momentum.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, legislative_supermajority_coalition_builders, agenda_setter,
    organized, biographical, constrained, national).

% A political majority that has won recent elections on a reform mandate but cannot muster the supermajority needed to entrench its preferred change constitutionally. From their vantage the threshold looks like an obstacle to democratic will, but on the consensus-safeguard reading their exclusion is exactly the mechanism working as designed — they are asked to persuade rather than simply prevail, and if the reform is genuinely durable it should be able to build the broader coalition over time.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, transient_reform_majorities, excluded,
    organized, immediate, constrained, national).

% Legal and political theorists who study amendment rates, coalition durability, and the empirical relationship between threshold height and constitutional legitimacy. They evaluate whether the threshold actually filters for durable consensus or merely filters for well-organized minorities, without themselves being subject to the constraint.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the timing and durability of fundamental legal change: it prevents the constitutional text from being rewritten by whichever coalition happens to hold a bare majority in a given electoral cycle, requiring instead that changes command support broad and durable enough to be credibly called a settled consensus rather than a passing preference.
% TRANSFER_FUNCTION: On this reading, the arrangement does not transfer resources from an identifiable victim to an identifiable beneficiary in ordinary operation — it moves the cost of constitutional change from 'whoever holds a simple majority' to 'whoever can build the broader coalition,' diffusing the burden of persuasion across all who would alter foundational law and diffusing the benefit of stability across all who live under it.
% ABSENT_VOICES: Transient reform majorities who believe their mandate is being obstructed are structurally present in the debate but structurally incapable of prevailing through ordinary majoritarian channels; genuinely disenfranchised groups who might benefit from rapid constitutional change (and who are captured more fully in the minoritarian_veto_reading) are largely absent from this reading's frame, which treats the current constitutional settlement as the presumptively legitimate baseline.
% DISAPPEARANCE_RATIONALE: If the threshold vanished overnight, ordinary legislative majorities could rewrite the constitution alongside ordinary statutes. On the consensus-safeguard reading the world rearranges significantly and for the worse: constitutional protections become as volatile as regular legislation, undermining the settled expectations that structure long-term investment, minority protection, and institutional design. Whether this is a rearrangement or merely a return to ordinary democratic responsiveness is exactly what separates this reading from its siblings, hence 'contested' rather than a clean verdict.
% FOUNDING_PROBLEM: Founders and framers across many constitutional systems observed that simple-majority amendment processes let short-term political passions permanently alter foundational rules, producing instability, opportunistic rule-changing by temporary majorities, and erosion of minority protections that were supposed to be insulated from ordinary politics.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists studying constitutional durability across many jurisdictions (outside any single constitution's beneficiary class) have documented correlations between very low amendment barriers and both constitutional instability and majority-driven erosion of minority protections; comparative constitutional law scholarship treats the passion-versus-consensus problem as a genuinely live design question, not merely a claim asserted by those who benefit from current entrenchment.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, contested).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).
:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, on this reading, the threshold's ordinary operation does not extract value from an identifiable victim class — it withholds a change until broader consensus forms, which is a cost borne diffusely by anyone seeking rapid reform, not a rent captured by a specific extracting party. Suppression is moderate (0.35): the mechanism does actively suppress the ability of a bare majority to enact constitutional change, and this suppression is structural and enforced (hence requires_active_enforcement: true), but it suppresses a procedural path rather than substantive alternatives — reformers can still pursue ordinary legislation, build coalitions over time, or amend statutes. Accessibility collapse is moderate (0.4): the supermajority path genuinely closes off the 'simple majority rewrites the constitution' alternative, but multiple other avenues for gradual influence remain open. Resistance is moderate-low (0.3), reflecting real but bounded friction from reform coalitions that view the bar as obstructive.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting coalition-builders and the excluded transient-majority seat would compute this constraint very differently even under this reading's own metrics: coalition-builders experience the threshold as a demanding but legitimate discipline they must satisfy, while transient majorities experience the identical structural fact (inability to enact change) as an obstruction. The consensus-safeguard reading's classification holds only from the vantage that treats the current constitutional baseline as presumptively legitimate; the engine computes the payer-adjacent excluded seat's perception separately from the beneficiary seats' perception, and that divergence is exactly the structural fact the kernel's sibling readings are built to capture differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (the general citizenry, future generations, protected minority communities) are declared with low structural extraction directed at them — the constraint subsidizes their interest in stability and protection. No victims are declared for this reading because the consensus-safeguard framing does not identify a party from whom value is extracted in ordinary operation; the closest candidate, transient reform majorities, is modeled as 'excluded' rather than 'payer' because on this reading their inability to prevail is the mechanism functioning correctly, not an extraction from them. This is the central structural delta from the minoritarian_veto_reading, which would declare that same coalition (or a subset of it) as victims of an entrenched blocking minority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (guarding against rewriting foundational law on transient passion) remains live by most comparative-constitutional accounts, which is why founding_problem_status is authored 'live' rather than 'dead.' This blocks a mandatrophy misreading in one direction: it would be an error to treat the persistence of a rarely-used amendment mechanism as evidence of pure inertia (a piton), since the mechanism's low usage rate is itself consistent with it doing exactly what a consensus filter should do — screening out proposals that never achieve durable consensus, not proving the filter is defunct. But the same low-extraction, high-legitimacy story is engine-testable: if temporal or comparative data later showed the threshold primarily blocking, not filtering, consensus formation, the true structural fact would look more like the minoritarian_veto_reading, and that divergence is what the sibling story exists to register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_filter_vs_privilege_entrenchment,
    'Does the supermajority threshold actually filter for durable, cross-cutting democratic consensus, or does it function primarily to entrench the preferences of a historically advantaged blocking minority regardless of the depth of broader consensus?',
    'Comparative analysis of amendment attempts across jurisdictions with varying threshold heights: track how often blocked amendments later achieved broad popular support (evidence for the veto reading) versus how often blocked amendments reflected genuinely transient or regionally concentrated preferences that failed to durably spread (evidence for the consensus-safeguard reading).',
    'If blocked amendments systematically show later-achieved broad consensus that the threshold nonetheless continued to block, this reading''s classification (rope, diffuse beneficiaries, no victim class) would be undermined and the minoritarian_veto_reading''s classification would better describe the constraint''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_filter_vs_privilege_entrenchment, empirical, 'Whether the threshold measures durable consensus or entrenches minority privilege — the central contest between this reading and its sibling.').

omega_variable(
    beneficiary_diffuseness_stability,
    'Is ''constitutional continuity'' a genuine diffuse public good benefiting the whole polity, or does it disproportionately benefit those already advantaged by the status quo constitutional arrangement (property holders, established political parties, incumbent institutional actors)?',
    'Distributional analysis of who has historically invoked the threshold to block proposed changes, and whether the invoking parties overlap systematically with groups favored by the pre-existing constitutional settlement.',
    'If invocation is concentrated among status-quo-advantaged actors rather than diffuse across the polity, the declared beneficiary set (constitutional_order_beneficiaries, future_generations, minority_constitutional_communities) would need narrowing, and extractiveness would likely be revised upward toward the tangled_rope or snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_diffuseness_stability, empirical, 'Whether the claimed diffuse beneficiary set genuinely captures who benefits from stability, or masks a narrower advantaged class.').

omega_variable(
    threshold_calibration_arbitrariness,
    'Is the specific numerical threshold (e.g. two-thirds, three-fourths) itself derivable from any principled theory of ''depth of consensus,'' or is it a historically contingent number that this reading retroactively justifies as a quality filter?',
    'Historical and comparative constitutional research into the origin of specific threshold numbers and whether framers offered a principled derivation versus a negotiated political compromise.',
    'If the threshold number is shown to be an arbitrary negotiated artifact rather than a principled consensus-depth measure, this reading''s legitimating story weakens relative to the adaptive_gradient_reading, which explicitly treats the threshold as requiring empirical tuning rather than fixed principled justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_calibration_arbitrariness, conceptual, 'Whether the specific threshold height has a principled basis or is a contingent artifact dressed as principled design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 60, 0.2).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(supermajority_threshold__consensus_safeguard_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the supermajority_threshold kernel. consensus_safeguard_reading (this file) authors low extraction and diffuse beneficiaries, classifying the threshold as coordination (rope). minoritarian_veto_reading authors the same procedural kernel with a specific victim set (reform majorities and their constituents) and active enforcement of blocking-minority privilege, classifying toward tangled_rope or snare. adaptive_gradient_reading treats the threshold as an instrumental variable whose legitimacy is conditional on empirical calibration, which if uncalibrated would drift toward scaffold-without-sunset or piton. All three share the identical textual/procedural artifact (the numerical amendment threshold) but diverge sharply in epsilon, beneficiary/victim structure, and claimed type — exactly the decomposition the epsilon-invariance principle requires rather than a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
