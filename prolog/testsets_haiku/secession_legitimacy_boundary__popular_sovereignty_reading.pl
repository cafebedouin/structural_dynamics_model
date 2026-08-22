% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Democratic Majority Sovereignty and Unilateral Secession Right
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   This constraint embodies the popular_sovereignty reading of contested
 *   secession legitimacy: a provincial democratic majority's referendum
 *   outcome on secession is treated as self-legitimating, requiring no
 *   federal consent, constitutional amendment, or higher authority
 *   validation. The reading subordinates federal constitutional law to the
 *   expressed will of the provincial majority and frames this subordination
 *   as the logical consequence of popular sovereignty. From the majority
 *   coalition's seat, the constraint is a rope solving the coordination
 *   problem of collective self-determination. From the federal authority's
 *   seat and the seats of provincial minorities excluded from or harmed by
 *   the outcome, the same structure operates as enforced extraction: the
 *   majority imposes its preferred territorial and political reorganization
 *   without constitutional safeguard or minority protection. This is a
 *   classic tangled_rope structure—genuine coordination function paired with
 *   asymmetric extraction—because the referendum mechanism itself is a real
 *   solution to the aggregation-of-will problem, but its operation
 *   systematically advantages the provincial majority and disadvantages those
 *   bound by the outcome without exit. The claim/metric divergence is
 *   deliberate and structural: the constraint is CLAIMED as enabling
 *   democratic coordination, while the authored metrics describe
 *   substantially extractive, actively suppressed operation because the
 *   enforcement machinery exists to subordinate federal law and prevent
 *   minorities from blocking the majority outcome.
 *
 * KEY AGENTS:
 *   - provincial_majority_coalition: Primary beneficiary (agenda-setter) — organizes the referendum, controls its framing, collects the exit right and transfer of authority
 *   - federal_authority_structure: Primary target/victim (high-d payer) — bears the cost of territorial loss, authority subordination, and loss of constitutional supremacy
 *   - provincial_minority_populations: Secondary targets (trapped, powerless) — bound by majority referendum outcome without veto or exit
 *   - indigenous_treaty_holders: Excluded (would contest framework but lack formal standing) — their prior sovereignty claims are not acknowledged in this reading
 *   - federal minority-protection mandate: Observer (analytical seat) — measures constraint's impact on constitutional rights and legal pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Democratic Majority Sovereignty and Unilateral Secession Right").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/constitutional/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '80f01c97-2488-4f75-ae09-66025e31f414').
narrative_ontology:cs_kernel_codification('80f01c97-2488-4f75-ae09-66025e31f414', fixed_text).
narrative_ontology:cs_authority_grounding('80f01c97-2488-4f75-ae09-66025e31f414', extraction).
narrative_ontology:cs_interpretation_layer_present('80f01c97-2488-4f75-ae09-66025e31f414').
narrative_ontology:cs_reading_relation('80f01c97-2488-4f75-ae09-66025e31f414', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('80f01c97-2488-4f75-ae09-66025e31f414', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('80f01c97-2488-4f75-ae09-66025e31f414', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('80f01c97-2488-4f75-ae09-66025e31f414', foundational, majoritarian_referendum_supremacy).
narrative_ontology:cs_axiom_status(majoritarian_referendum_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('80f01c97-2488-4f75-ae09-66025e31f414', majoritarian_referendum_supremacy, deontological).
narrative_ontology:cs_axiom('80f01c97-2488-4f75-ae09-66025e31f414', foundational, provincial_boundary_as_sovereign_unit).
narrative_ontology:cs_axiom_status(provincial_boundary_as_sovereign_unit, holdable).
narrative_ontology:cs_axiom_grounding('80f01c97-2488-4f75-ae09-66025e31f414', provincial_boundary_as_sovereign_unit, conventional).
narrative_ontology:cs_reference_frame('80f01c97-2488-4f75-ae09-66025e31f414', provincial_democratic_majority_sovereignty).
narrative_ontology:cs_drift_state('80f01c97-2488-4f75-ae09-66025e31f414', contemporary_federal_resistance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80f01c97-2488-4f75-ae09-66025e31f414', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority_structure).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_populations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, democratic_majority_rule).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, popular_sovereignty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition commanding democratic majority in a province frames itself as holding ultimate sovereignty within its territorial boundaries. It initiates a referendum on secession; winning the referendum is, in this reading, self-legitimating and creates an unconditional right to exit the federation. It sets the terms of legitimacy discourse and enforces the interpretation that majority-endorsed referendum outcomes are constitutional unto themselves, subordinating federal law.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition, agenda_setter,
    organized, generational, mobile, regional).

% The federal government and its constitutional order are positioned as bearing the cost of the majority's unilateral exit right. In this reading, federal authority is downgraded from a foundational legitimate power to a conditional agent of provincial majorities. The federal structure bears the cost of potentially losing territory, revenue, and geopolitical standing without recourse to constitutional defense.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority_structure, payer,
    institutional, generational, constrained, national).

% Minorities within the province (ethnic, religious, linguistic, or ideological groups holding a different view of federalism or secession) are subject to the majority referendum outcome. If they oppose secession, they have no structural veto and limited exit options; they are bound by what the majority decides. If they fear post-secession governance by the provincial majority, their recourse is constrained by the reading's framework, which treats the referendum result as dispositive.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_populations, payer,
    powerless, biographical, trapped, regional).

% Indigenous nations with territories spanning the province are excluded from the provincial majority coalition's referendum legitimacy framework. Their treaty rights predate both the province and the federation; the popular_sovereignty_reading does not acknowledge prior sovereignty claims or require indigenous consent, making them excluded parties who would object if heard.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, excluded,
    moderate, civilizational, identity_locked, regional).

% Constitutional and human-rights bodies tasked with protecting minorities across the federation observe the constraint as subordinating minority rights to majority will within a province. They measure the constraint's impact on rights guarantees and legal pluralism.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minority_protection_mandate, observer,
    institutional, generational, analytical, national).

% International law frameworks (UN, regional bodies) observe the constraint as a claim about the source of sovereignty and legitimacy in secession. They assess whether unilateral referendum-based exit is compliant with self-determination norms or constitutes breach of territorial integrity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_recognition_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_coalition).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, democratic mechanism for resolving fundamental questions about political community membership and territorial organization: a provincial referendum aggregates citizen will on the question of continued federation and produces a determinative answer without ambiguity about legitimacy source.
% TRANSFER_FUNCTION: Moves the right to define political legitimacy from the federal constitutional order to the provincial democratic majority. A referendum result transfers authority over the province's political future from federal oversight to provincial self-determination. Concretely, it transfers control over territory and the population within it, along with associated resource claims and diplomatic standing.
% ABSENT_VOICES: Indigenous treaty holders (whose nations span the province and predate both the province and federation) are structurally excluded from the referendum electorate and the legitimacy framework. Provincial minorities opposing secession have no veto and are excluded from the binding decision despite bearing its consequences. Constitutional courts and federal minority-protection bodies are not parties to the referendum but would object to the subordination of constitutional protections and minority rights guarantees to majority vote.
% DISAPPEARANCE_RATIONALE: If this constraint—the claim that a provincial majority referendum is self-legitimating and unbars unilateral secession—disappeared and were replaced by a reading requiring constitutional amendment, negotiated exit, or federal consent, the province would lose unilateral exit capacity and would have to operate within the federal system's amendment procedures. Political organization, territorial boundaries, and the locus of sovereign authority would reorganize around the new legitimacy standard. Secessionist movements would need to either negotiate or accept subordination to federal constitutional processes.
% FOUNDING_PROBLEM: Early federalism locked provinces into permanent union with no exit mechanism; majorities asserting self-determination had no democratic procedure to escape perceived federal extraction or injustice. The constraint provides a solution: a referendum mechanism allowing the democratic will of the provincial population to override constitutional text and federal authority.
% FOUNDING_PROBLEM_CORROBORATION: Secessionist movements and democratic-majority advocates within provinces attest the founding problem is live: they argue federal authority blocks the democratic exit rights that popular sovereignty demands. Federal governments and constitutional courts attest the founding problem is resolved: modern federalism includes constitutional amendment procedures and negotiation frameworks for fundamental changes, and unilateral exit was never intended. Constitutional scholars outside the secessionist movement split: some support the founding problem narrative, others argue it conflates democratic will with constitutional authority. Independent political theorists and international observers attest that the ambiguity is genuine and unresolved across jurisdictions.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 endpoint) because the constraint transfers control over fundamental political organization from a federal constitutional process to a provincial majority without requiring negotiation, constitutional amendment, or minority protection. The majority collects the exit right and authority transfer; the federal order and minorities bear the cost. Suppression is high (0.71) because the constraint requires active enforcement: federal courts must be subordinated or overridden, federal treaty obligations must be reinterpreted or abandoned, international law frameworks must be contested, and provincial minorities' objections must be excluded from the binding decision. Theater is moderate (0.42) because the referendum mechanism is genuinely democratic aggregation—it actually measures and transmits the majority's will—but the framing that this result is self-legitimating and overrides all other legal authority is a claim the constraint must actively defend against constitutional and international law arguments. The measurement series show extraction and suppression rising steeply in the early interval (t=0 to t=15) as the constraint moves from theoretical claim to institutional enforcement machinery, then plateauing (t=15 to t=40) as suppression hardens and the claim settles into settled practice. Theater rises throughout as the interpretive work required to maintain the constraint's legitimacy (subordinating constitutional law, managing international response, excluding minorities) becomes more elaborate and ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   The majority coalition's and federal authority's readings of what the constraint is and does are nearly orthogonal. From the coalition's seat, it is a democratic solution to a coordination problem (aggregating will on federation membership). From the federal seat, it is an existential challenge to constitutional authority and a unilateral transfer of power. From the minority seats, it is oppressive regardless—they are bound without voice. This is the signature seat divergence of a tangled_rope: one group experiences genuine coordination benefit (the majority that organized the outcome), while another group experiences pure extraction (minorities and the federal order subordinated without recourse).
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial_majority_coalition benefits directly from the constraint: it collects the unilateral exit right and the transfer of sovereignty authority from federal to provincial scope. Deriving its directionality from beneficiary status + mobile exit gives d ≈ 0.2 (full beneficiary end). The federal_authority_structure bears the structural cost: the constraint exists to override its constitutional authority and remove territory/population/resources from its jurisdiction. Federal authority is constrained (cannot exit the union it nominally heads) and institutional (bound by law); deriving d from victim status + constrained exit gives d ≈ 0.85 (near full target). Provincial_minority_populations are trapped (cannot exit the province or avoid the majority outcome), powerless (have no veto in the referendum), and explicitly excluded from the legitimacy framework; their d ≈ 0.88 (full target). Indigenous treaty holders would occupy high d if included, but are excluded from the formal structure; their d would be ≈ 0.90 if the constraint were applied to them (identity_locked by prior sovereignty claims, unable to exit). No directionality overrides are required; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy by the M1 reading (the founding problem—majoritarian exit rights blocked by federal union—remains live). However, the founding_problem_status is contested: federal authorities and constitutional scholars argue the founding problem is resolved (modern amendment procedures exist; negotiation frameworks are available) while secessionist advocates argue it persists (amendment is too difficult; negotiation is subordinate to prior sovereign will). This contestation is exactly the condition where tangled_rope correctly classifies: the constraint solves a coordination problem (majoritarian will-aggregation on federation membership) AND systematically extracts from those excluded from or harmed by the outcome (federal authority, minorities). The mandatrophy analysis supports the tangled_rope claim because the constraint's persistence does not require universal belief in its legitimacy—it requires only that the organizing majority sustains the enforcement machinery (courts subordinated, international law contested, minorities excluded). Weaker mandatrophy would appear if federal authority capitulated completely or if minorities organized a counter-constraint with comparable force; the measurement series show neither (resistance to the constraint remains at 0.74 endpoint, indicating ongoing federal and minority contestation), so the tangled structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is ultimate sovereignty located in the democratic will of the provincial majority, or does constitutional law predate and constrain democratic will?',
    'A constitutional amendment explicitly endorsing or rejecting the unilateral exit right would resolve within the federal framework. International precedent on secession legitimacy (Kosovo, South Sudan, Catalonia) provides comparative resolution attempts but not definitive closure.',
    'If sovereignty is majoritarian and referenda are self-legitimating, the constraint enables unilateral exit and subordinates federal law. If constitutional law constrains democratic will, the constraint is usurpation and the referendum is advisory only. This is the core disagreement between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether majoritarian will or constitutional law is the source of legitimate authority.').

omega_variable(
    minority_protection_vs_majority_rule,
    'Can minority rights protections coexist with the principle that a provincial referendum outcome is dispositive, or must one subordinate the other?',
    'Post-referendum governance in a seceding province: do minorities retain enforceable constitutional protections, or does the majority''s mandate override minority rights? Comparative study of actual post-secession arrangements (e.g., Quebec, Catalonia post-independence claims) provides empirical evidence.',
    'If minority rights and majoritarian referendum outcomes can coexist, the constraint is less extractive—minorities retain legal protection despite non-consent. If the referendum outcome is absolutely dispositive, minorities are fully exposed to majoritarian governance without constitutional safeguard, confirming the high extraction measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_vs_majority_rule, empirical, 'Whether majority referenda and minority protections are structurally compatible in this framework.').

omega_variable(
    indigenous_treaty_precedence,
    'Do indigenous treaty rights predate and supersede both federal and provincial authority, or are they subordinate to the provincial referendum outcome?',
    'Legal rulings on the status of treaties relative to provincial secession; negotiated settlements between indigenous nations and secessionist majorities; international indigenous rights jurisprudence.',
    'If treaties predate the provincial majority''s authority, indigenous nations retain veto or consent rights over secession and its terms, constraining the majority''s unilateral exit right. If the provincial majority''s referendum is supreme, indigenous treaty rights are overridden or must be renegotiated post-secession without guarantee of continuity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_treaty_precedence, conceptual, 'Whether prior indigenous sovereignty predate and constrain the provincial majority''s secession right.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the reading''s enforcement machinery (subordinating federal law, excluding minorities, contesting international recognition) a necessary cost of genuine democratic coordination, or does it exceed what coordination requires?',
    'Comparison with other majoritarian decision-making mechanisms (ordinary referenda, electoral outcomes, constitutional amendments) that achieve coordination without subordinating all higher authority. Analysis of whether minorities could consent or be protected without invalidating the referendum outcome.',
    'If the enforcement machinery exceeds coordination requirements, the constraint is primarily extractive and should classify as snare. If the machinery is structurally necessary to establish the majority''s sovereignty, it is a justified cost of coordination and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether the constraint''s enforcement burden is proportionate to its coordination function or indicates pure extraction.').

omega_variable(
    federal_authority_as_victim,
    'Is the federal authority structure a legitimate party bearing costs, or is it an obstacle to legitimacy that the reading correctly subordinates?',
    'Political theory and institutional analysis: whether federalism is itself a legitimate coordination arrangement or a constraint on self-determination. Comparative study of federal legitimacy across jurisdictions.',
    'If federalism is legitimate, the federal authority is correctly identified as a victim bearing extraction costs. If federalism is illegitimate or derivative (subordinate to provincial sovereignty), the federal authority is not a victim but rather a structure being corrected, and the constraint''s cost to federal authority is a necessary realignment, not extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_authority_as_victim, preference, 'Whether federal authority is a legitimate victim of the constraint or a structure correctly subordinated by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(sece_tr_t5, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sece_tr_t25, observed).
narrative_ontology:measurement(sece_tr_t35, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(sece_tr_t35, projected).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(sece_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(sece_be_t5, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(sece_be_t25, observed).
narrative_ontology:measurement(sece_be_t35, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(sece_be_t35, projected).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sece_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(sece_su_t5, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(sece_su_t25, observed).
narrative_ontology:measurement(sece_su_t35, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(sece_su_t35, projected).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(sece_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-reading constraint family decomposing the contested kernel secession_legitimacy_boundary. Each reading instantiates a different principle for determining when secession is legitimate: this reading (popular_sovereignty) treats the provincial majority referendum as self-legitimating; the constitutional_impossibility reading denies unilateral secession entirely; the grievance_threshold reading permits secession only when federal extraction crosses a threshold; the treaty_primacy reading acknowledges indigenous prior sovereignty. These are structurally distinct constraints with different ε values, beneficiary/victim structures, and enforcement mechanisms. Links: popular_sovereignty influences constitutional_impossibility (it undermines constitutional supremacy claims), forecloses treaty_primacy (it treats provincial majority as ultimate authority, which contradicts prior indigenous sovereignty), and coexists_with grievance_threshold (both treat provincial majorities as capable of exit, but differ on the trigger condition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
