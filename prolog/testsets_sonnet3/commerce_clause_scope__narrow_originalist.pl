% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Narrow-Originalist Reading of Commerce Clause Scope
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the narrow-originalist reading of the Commerce
 *   Clause: 'commerce among the several states' means trade that literally
 *   crosses state lines, 'regulate' means make regular/facilitate rather than
 *   restrict or prohibit, and federal power is limited to removing
 *   state-imposed barriers to interstate trade and ensuring uniform
 *   commercial rules for genuinely interstate transactions. On this reading,
 *   the coordination function (preventing interstate trade wars, ensuring a
 *   uniform commercial floor for goods actually crossing state lines) is real
 *   and narrow, and the extraction is low in absolute terms — but the reading
 *   systematically walls off federal reach from intrastate labor,
 *   environmental, and civil-rights regulation, transferring the cost of that
 *   walling-off onto people the federal government would otherwise protect.
 *   Two sibling readings of the same kernel (broad_effects_test,
 *   intermediate_channels) are NOT part of this file; they are separate
 *   constraints with their own ε and their own stakeholder sets, linked
 *   structurally rather than blended into this one.
 *
 * KEY AGENTS:
 *   - state_governments: primary beneficiary (institutional/arbitrage) — gains exclusive regulatory authority over intrastate economic activity
 *   - local_businesses: secondary beneficiary (moderate/constrained) — escapes federal compliance burden
 *   - civil_rights_claimants_in_recalcitrant_states: primary victim (powerless/trapped) — loses federal cause of action absent an interstate nexus
 *   - workers_in_intrastate_industries: secondary victim (powerless/constrained) — loses federal wage/safety floor
 *   - federal_courts: agenda_setter (institutional/analytical) — administers and could revise the interstate-line test
 *   - congress: excluded (institutional/constrained) — statutory preferences for broader reach are foreclosed by the doctrine, not part of its interpretive conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Narrow-Originalist Reading of Commerce Clause Scope").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/political").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '3242297c-b745-4238-8f84-dae096d4ad7f').
narrative_ontology:cs_kernel_codification('3242297c-b745-4238-8f84-dae096d4ad7f', fixed_text).
narrative_ontology:cs_authority_grounding('3242297c-b745-4238-8f84-dae096d4ad7f', lineage).
narrative_ontology:cs_interpretation_layer_present('3242297c-b745-4238-8f84-dae096d4ad7f').
narrative_ontology:cs_reading_relation('3242297c-b745-4238-8f84-dae096d4ad7f', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('3242297c-b745-4238-8f84-dae096d4ad7f', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('3242297c-b745-4238-8f84-dae096d4ad7f', foundational, regulate_means_facilitate_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('3242297c-b745-4238-8f84-dae096d4ad7f', regulate_means_facilitate_not_restrict, conventional).
narrative_ontology:cs_axiom('3242297c-b745-4238-8f84-dae096d4ad7f', foundational, commerce_requires_literal_interstate_transaction).
narrative_ontology:cs_axiom_status(commerce_requires_literal_interstate_transaction, holdable).
narrative_ontology:cs_axiom_grounding('3242297c-b745-4238-8f84-dae096d4ad7f', commerce_requires_literal_interstate_transaction, conventional).
narrative_ontology:cs_axiom('3242297c-b745-4238-8f84-dae096d4ad7f', secondary, federal_power_categorically_excludes_intrastate_activity).
narrative_ontology:cs_axiom_status(federal_power_categorically_excludes_intrastate_activity, holdable).
narrative_ontology:cs_axiom_grounding('3242297c-b745-4238-8f84-dae096d4ad7f', federal_power_categorically_excludes_intrastate_activity, deontological).
narrative_ontology:cs_reference_frame('3242297c-b745-4238-8f84-dae096d4ad7f', founding_era_enumerated_powers_framework).
narrative_ontology:cs_drift_state('3242297c-b745-4238-8f84-dae096d4ad7f', post_new_deal_administrative_state, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3242297c-b745-4238-8f84-dae096d4ad7f', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_beneficiaries).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, workers_in_intrastate_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary regulatory authority over intrastate economic activity — labor conditions, local environmental permitting, in-state business licensing, and civil rights enforcement within their borders. Under this reading, federal statutes reaching non-interstate-transactional conduct are void as applied to them, so a state can decline to adopt federal floors and courts will strike federal preemption attempts. Exit from federal oversight is close to arbitrage-grade: a state hostile to a federal labor or civil-rights standard need not lobby for a waiver, it can simply assert the activity is not interstate commerce and litigate the point favorably under this doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, state_governments, agenda_setter).

% Wholly intrastate firms — a local manufacturer selling only within one state, a family restaurant, an in-state agricultural operation — escape federal wage, safety, and environmental regulation that would otherwise apply if commerce were read broadly. They benefit from lighter compliance burdens and from being regulated (if at all) only by state law, which they can more easily influence given proximity and lower lobbying cost.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, constrained, local).

% Not a single actor but the abstract policy value of federalism-as-laboratory: under this reading, fifty jurisdictions can trial different regulatory regimes for intrastate activity without a single federal floor overriding the experiment. Listed for completeness; collects no rents itself.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters).

% Individuals facing discrimination by intrastate businesses (a lunch counter serving only local customers, a locally-owned hotel not part of an interstate chain) lose federal civil-rights protection under this reading unless the specific transaction can be tied to interstate travel or goods. Where a state legislature or courts are hostile to protecting them, the only recourse is the state's own (possibly nonexistent or weakly enforced) civil rights law. Relocating out of a recalcitrant state is often not a realistic option given economic and family ties — exit is closer to trapped than constrained.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states, payer,
    powerless, biographical, trapped, local).

% Represents the class of actors — interstate businesses seeking one compliance regime instead of fifty, workers seeking a uniform wage/hour floor, environmental interests seeking a national baseline — whose interest in predictable, single national standards is defeated when the same activity is regulated (or not) differently depending on whether it is characterized as interstate. Listed for completeness as a non-agent interest bearing the cost of fragmentation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_beneficiaries, payer,
    moderate, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_beneficiaries).

% Employees of firms characterized as purely intrastate (a local mine, an in-state textile mill, in-home domestic labor) lose access to federal minimum-wage, overtime, and workplace-safety protections that would apply if their employer's conduct were read as interstate commerce. They can seek employment elsewhere but often face the same intrastate-classification problem region-wide, and relocation carries real costs — exit is constrained rather than trapped, but meaningfully bounded.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, workers_in_intrastate_industries, payer,
    powerless, biographical, constrained, local).

% Federal judges applying this reading determine, case by case, whether a given statute's target activity crosses the interstate line as originally understood. They administer the boundary and could, by adopting a different interpretive method, widen or narrow it; the doctrine's persistence depends on continued judicial commitment to originalist method over competing interpretive theories.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Would prefer, in many enacted statutes, to reach intrastate activity with cumulative national effects (environmental contamination, labor standards, civil rights) but under this reading lacks the constitutional authority to do so directly — it must find an alternative enumerated power or abandon the regulation. Congress's preference for broader reach is structurally present in the statutes it passes but is not part of the interpretive conversation this reading permits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents states from unilaterally interfering with the flow of goods and services across state lines and gives interstate commercial actors a predictable, uniform floor of federal commercial rules — genuinely solving the collective-action problem of state-level trade barriers (tariffs, discriminatory taxation, protectionist licensing) that a purely state-by-state regime would produce.
% TRANSFER_FUNCTION: Moves regulatory authority over intrastate economic, labor, environmental, and civil-rights matters away from federal statute and toward state legislatures and courts; in states hostile to federal-style protections, this transfers real costs from would-be regulatory beneficiaries (workers, civil rights claimants, environmental interests) to the businesses and state governments that would otherwise have borne compliance costs.
% ABSENT_VOICES: Civil rights claimants and workers in states unwilling to legislate protections are structurally absent from the doctrinal conversation, which is conducted among judges, litigants with resources to bring test cases, and states asserting sovereignty interests — the people actually left without protection rarely have standing or resources to be heard in the cases that set the boundary.
% DISAPPEARANCE_RATIONALE: If this reading's boundary disappeared and a broader reading took over, federal statutes currently held inapplicable to intrastate labor, environmental, and civil-rights matters would immediately reach conduct they cannot reach today — states would lose exclusive regulatory authority over large categories of local economic activity, and civil rights claimants in recalcitrant states would gain a federal cause of action currently unavailable to them.
% FOUNDING_PROBLEM: The Constitutional Convention needed to solve interstate trade wars under the Articles of Confederation — states erecting tariffs and discriminatory regulations against each other's goods — without creating a federal government with plenary power over all economic life within the states.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and state governments attest the founding problem (interstate trade-war prevention plus enumerated, limited federal power) remains the correct frame and is still live in its original scope. Legal historians outside the originalist camp, along with civil rights litigators and labor historians, attest that the founding problem as originally scoped has been substantially superseded by a national integrated economy in which almost no activity is truly local in economic effect, making the narrow reading's persistence a matter of judicial-philosophical commitment rather than continued fit to the original problem.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because the reading's own coordination function — removing state trade barriers and unifying rules for genuinely interstate transactions — is real and the doctrine does not itself extract resources from a broad population; its cost is concentrated and structural (denial of federal protection) rather than a diffuse rent. Suppression is moderate (0.42) because the doctrine is actively enforced through judicial review striking down federal statutes reaching intrastate conduct, and that enforcement forecloses political-branch alternatives (Congress cannot simply legislate around it without a different enumerated power). Resistance is high (0.68) reflecting sustained academic, congressional, and civil-rights-litigation pushback against this reading since its modern articulation. Accessibility collapse is moderate (0.4): unlike a mountain, real alternatives (the two sibling readings) remain live and are argued in courts continuously — the reading has not achieved anything like natural-law-level closure.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses sit near the beneficiary end: the doctrine removes constraints that would otherwise bind them, and they have institutional or economic leverage over intrastate rulemaking they retain. Civil rights claimants and intrastate workers sit near the target end: they are powerless, their exit is trapped or constrained, and the doctrine's operation directly removes protection they would otherwise have. Federal courts are the agenda-setter — they administer the interstate-line test and could shift it through a different interpretive method, which is exactly the axis on which the sibling readings compete.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (interstate trade-war prevention via limited enumerated federal power) is authored as contested rather than flatly dead: from the originalist tradition's own lights the problem persists in its original scope and the doctrine is not mandatrophic. From outside that tradition, the doctrine is read as having outlived its fit to an integrated national economy, making its persistence a matter of interpretive commitment rather than continued necessity. Declaring this contested rather than resolving it in either direction is the correct move for a kernel-reading story: this file authors the narrow-originalist reading's own self-understanding while the corroboration field routes the competing genealogy claim through sourced attestation rather than adjudicating it here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_meaning_recovery_reliability,
    'Does the narrow reading of ''commerce'' and ''regulate'' accurately recover the founding-era semantic and legal meaning of those terms, or does it import a later libertarian gloss onto ambiguous historical usage?',
    'Corpus linguistics analysis of founding-era usage of ''commerce'' and ''regulate'' across ratification debates, contemporaneous dictionaries, and early Congressional and judicial practice (e.g., Gibbons v. Ogden''s own historical claims); comparison against the intermediate and broad readings'' competing historical accounts.',
    'If the historical recovery is unreliable or contested, the reading''s claim to be the ''original'' meaning is weakened, and its persistence looks more like continued preference for decentralization dressed as textual fidelity — pushing the classification toward a more clearly extractive tangled_rope or snare framing on the victim side. If the recovery is robust, the reading''s coordination-function claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_meaning_recovery_reliability, empirical, 'Whether the narrow reading''s historical semantic claims withstand corpus-linguistic and historical scrutiny.').

omega_variable(
    beneficiary_vs_vindicated_proposition_boundary,
    'Is ''decentralized_regulatory_experimentation'' a genuine collective beneficiary (an actor that captures value) or should it instead be classified as a vindicated proposition (a doctrine that collects no rents)?',
    'Trace whether federalism-as-laboratory produces measurable downstream value capture by any identifiable actor (e.g., states that experiment successfully attracting business relocation) versus remaining a purely normative claim about policy diversity''s abstract worth.',
    'If it resolves as a vindicated proposition only, removing it from beneficiaries would leave state_governments and local_businesses as the sole beneficiary class, sharpening (not changing) the tangled_rope classification since the coordination-function requirement is already met by those two.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vs_vindicated_proposition_boundary, conceptual, 'Whether decentralized experimentation is an actor-beneficiary or a non-rent-collecting doctrine.').

omega_variable(
    recalcitrant_state_prevalence,
    'In practice, how many states would decline to extend civil-rights or labor protections to intrastate conduct absent federal compulsion, versus adopting comparable protections voluntarily under this reading?',
    'Empirical survey of state statutory law in the counterfactual periods before major federal civil-rights and labor statutes were enacted, and in states that have since narrowed or repealed state-level protections when federal floors were reduced.',
    'High prevalence of recalcitrant states would validate the high-severity victim characterization (trapped exit, powerless) authored here; low prevalence would suggest the victim class is narrower and the doctrine''s practical cost is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recalcitrant_state_prevalence, empirical, 'How many jurisdictions would actually withhold protection absent federal compulsion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2001, commerce_clause_scope__narrow_originalist, theater_ratio, 2001, 0.16).
narrative_ontology:measurement(comm_tr_t2007, commerce_clause_scope__narrow_originalist, theater_ratio, 2007, 0.17).
narrative_ontology:measurement(comm_tr_t2013, commerce_clause_scope__narrow_originalist, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(comm_tr_t2019, commerce_clause_scope__narrow_originalist, theater_ratio, 2019, 0.19).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__narrow_originalist, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(comm_be_t2001, commerce_clause_scope__narrow_originalist, base_extractiveness, 2001, 0.24).
narrative_ontology:measurement(comm_be_t2007, commerce_clause_scope__narrow_originalist, base_extractiveness, 2007, 0.25).
narrative_ontology:measurement(comm_be_t2013, commerce_clause_scope__narrow_originalist, base_extractiveness, 2013, 0.26).
narrative_ontology:measurement(comm_be_t2019, commerce_clause_scope__narrow_originalist, base_extractiveness, 2019, 0.27).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__narrow_originalist, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement(comm_su_t2001, commerce_clause_scope__narrow_originalist, suppression_requirement, 2001, 0.36).
narrative_ontology:measurement(comm_su_t2007, commerce_clause_scope__narrow_originalist, suppression_requirement, 2007, 0.37).
narrative_ontology:measurement(comm_su_t2013, commerce_clause_scope__narrow_originalist, suppression_requirement, 2013, 0.39).
narrative_ontology:measurement(comm_su_t2019, commerce_clause_scope__narrow_originalist, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__narrow_originalist, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, intermediate_channels).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the commerce_clause_scope kernel. broad_effects_test authors comprehensive federal reach over any activity substantially affecting interstate commerce in the aggregate (higher federal extraction from state sovereignty, broader beneficiary set among national regulatory interests, broader victim set among states and local businesses losing autonomy). intermediate_channels authors a middle position with limiting principles (jurisdictional-element requirements, economic/non-economic distinction). Each reading is authored as its own ε-invariant constraint per the ε-invariance principle; none averages or hedges across the others. All three are linked bidirectionally via affects_constraints to preserve the kernel-family structure for contamination and network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
