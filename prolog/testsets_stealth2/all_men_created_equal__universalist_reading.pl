% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of the Equality Clause: Iterative Expansion Regardless of Founder Intent
 *   domain: constitutional law/political philosophy/american studies
 *
 * SUMMARY:
 *   This story instantiates the universalist reading of the kernel 'all men
 *   are created equal': the claim that the Declaration's equality clause
 *   states a universal principle whose scope must iteratively expand
 *   regardless of founder intent. Institutionally born at Gettysburg (1863)
 *   and constitutionalized through the Reconstruction Amendments, the reading
 *   operates as the governing grammar of American equality disputes: each
 *   generation's excluded group invokes the clause, a coalition forms around
 *   the claim, courts vindicate or defer, and the payer population grows to
 *   include whichever jurisdictions, institutions, and privilege holders the
 *   newest expansion sweeps in. The constraint genuinely coordinates — it
 *   gives dispersed excluded groups a shared, prestigious legitimacy language
 *   and solves their collective-action problem — and it genuinely extracts —
 *   compliance is compelled by courts and occasionally by federal force,
 *   costs concentrate on identifiable payers, and the victim set expands by
 *   design. KEY AGENTS (by structural relationship): -
 *   marginalized_status_claimants: Primary beneficiary
 *   (organized/constrained) — claims inclusion, receives standing and
 *   remedies - equality_expansion_coalitions: Secondary beneficiary
 *   (organized/mobile) — wields the clause as a legitimacy resource, collects
 *   victories - federal_judiciary: Agenda-setter
 *   (institutional/identity_locked) — administers the standard, accrues
 *   interpretive authority with each vindication - resistant_jurisdictions:
 *   Primary payer (institutional/trapped) — bears compliance costs, secession
 *   foreclosed - inherited_privilege_holders: Payer (powerful/constrained) —
 *   loses relative position as exclusions dissolve -
 *   successive_expansion_targets: Payer (moderate/constrained) — newly
 *   non-compliant as protected classes multiply - founder_intent_holders:
 *   Excluded non-party (dead; overridden by the reading's defining move) -
 *   constitutional_theorists: Analytical observer — maps the contest,
 *   collects no flows. The claim/metric gap is deliberate: the reading CLAIMS
 *   itself as pure moral principle (no extraction, only truth unfolding),
 *   while the authored metrics describe a moderately extractive, actively
 *   enforced, cyclically resisted operation. The engine measures that
 *   divergence; the claim is not reconciled to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.58).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.48).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of the Equality Clause: Iterative Expansion Regardless of Founder Intent").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional law/political philosophy/american studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '6754d997-697c-4024-80a1-9f13b1a745aa').
narrative_ontology:cs_kernel_codification('6754d997-697c-4024-80a1-9f13b1a745aa', fixed_text).
narrative_ontology:cs_authority_grounding('6754d997-697c-4024-80a1-9f13b1a745aa', lineage).
narrative_ontology:cs_interpretation_layer_present('6754d997-697c-4024-80a1-9f13b1a745aa').
narrative_ontology:cs_reading_relation('6754d997-697c-4024-80a1-9f13b1a745aa', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6754d997-697c-4024-80a1-9f13b1a745aa', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('6754d997-697c-4024-80a1-9f13b1a745aa', foundational, equality_scope_transcends_founder_intent).
narrative_ontology:cs_axiom_status(equality_scope_transcends_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('6754d997-697c-4024-80a1-9f13b1a745aa', equality_scope_transcends_founder_intent, deontological).
narrative_ontology:cs_axiom('6754d997-697c-4024-80a1-9f13b1a745aa', secondary, iterative_expansion_is_fidelity_not_revision).
narrative_ontology:cs_axiom_status(iterative_expansion_is_fidelity_not_revision, holdable).
narrative_ontology:cs_axiom_grounding('6754d997-697c-4024-80a1-9f13b1a745aa', iterative_expansion_is_fidelity_not_revision, conventional).
narrative_ontology:cs_reference_frame('6754d997-697c-4024-80a1-9f13b1a745aa', declaration_as_universal_self_expanding_commitment).
narrative_ontology:cs_drift_state('6754d997-697c-4024-80a1-9f13b1a745aa', contemporary, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('6754d997-697c-4024-80a1-9f13b1a745aa', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_status_claimants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, equality_expansion_coalitions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, resistant_jurisdictions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, inherited_privilege_holders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, successive_expansion_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose equal civic status has been denied under prevailing arrangements — at different moments enslaved people and their descendants, women, racial minorities, same-sex couples. They invoke the Declaration's equality clause to claim inclusion and receive standing, remedies, and access as the standard expands. Leaving the polity is possible in principle but costly and rarely chosen; their leverage comes from organization, litigation, and moral claim rather than wealth or office.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_status_claimants, beneficiary,
    organized, generational, constrained, national).

% Movements and their legal arms — abolitionist societies, suffrage organizations, civil rights organizations, LGBTQ advocacy groups — that wield the equality clause as a shared legitimacy resource. Each campaign converts the principle's prestige into concrete wins and organizational capital; when a campaign stalls, leaders can reframe the claim or shift forums. They collect victories and standing but do not administer the standard's enforcement.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, equality_expansion_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Interprets and enforces the equality clause's reach. Each expansion controversy arrives as a case; each vindication converts a contested political question into settled doctrine and enlarges the court's interpretive dominion. The judiciary's post-Reconstruction authority is bound up with its role as the clause's authoritative reader — its members cannot abandon that role without dissolving the institution's own claim to supremacy over constitutional meaning.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% States, municipalities, and school districts whose local arrangements — segregation statutes, marriage codes, district boundaries — are invalidated as the standard expands. They bear compliance costs, litigation losses, and in extreme episodes federal troop deployments. Secession and nullification were foreclosed by the Civil War; their remaining moves are delay, evasion, and political pushback within the union.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, resistant_jurisdictions, payer,
    institutional, generational, trapped, regional).

% Persons and families whose status, property, or market position presupposes the exclusions the standard dissolves — historically the planter class, uncompensated by emancipation; in later rounds, incumbents of advantaged access to schools, jobs, and neighborhoods. They lose relative position as inclusion proceeds and absorb the sharpest redistribution costs; exit means private enclaves or expatriation, both expensive.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, inherited_privilege_holders, payer,
    powerful, biographical, constrained, national).

% Institutions that complied with the standard as of one decade and find themselves non-compliant the next as protected classes and recognized statuses multiply — employers adjusting to each newly protected category, districts ordered to remedy disparities they had already addressed under the prior formula. Their costs arrive unpredictably, set by doctrine they do not write.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, successive_expansion_targets, payer,
    moderate, biographical, constrained, national).

% The founding generation whose intentions and social taxonomy the universalist reading overrides. Dead and unable to object, they participate only through the originalist reading's advocacy on their behalf; the universalist reading's defining move is to subordinate their intent to the principle's content. Kept on the surface for narrative completeness; not an acting party.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, founder_intent_holders, excluded,
    powerful, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__universalist_reading, founder_intent_holders).

% Scholars and commentators who map the clause's competing readings, trace doctrinal lineages, and publish the analyses both camps cite. They hold no enforcement power and collect no compliance flows; their stake is argumentative position.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, expandable standard of equal civic status that lets differently situated excluded groups coordinate claims without renegotiating first principles each round, and embeds that standard in the founding document itself so each new claimant inherits an already-prestigious legitimacy language.
% TRANSFER_FUNCTION: Moves enforceable recognition — standing, remedies, access, status — toward groups denied equal status, financed by compliance and restructuring costs drawn from jurisdictions, institutions, and privilege holders whose arrangements presuppose hierarchy; incidentally moves interpretive authority toward the federal judiciary with each vindication.
% ABSENT_VOICES: The founder generation whose intent the reading overrides — present only through the originalist reading's advocacy, structurally unable to answer. At each expansion round, the newly targeted institutions lack a seat until enforcement names them. Historically, the excluded themselves were absent from the founding conversation the reading now reinterprets on their behalf.
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight, the operative constitutional grammar collapses: the legitimating frame behind Brown-through-Obergefell doctrine loses its warrant, expansion coalitions lose their shared language and fragment into issue-specific campaigns, and the contest reverts to founder-intent bounds — a wholesale rearrangement of rights, enforcement, and coalition structure.
% FOUNDING_PROBLEM: The founding contradiction: a polity declaring universal equality while constitutionally protecting chattel slavery and categorical exclusion. The reading was built (Lincoln onward) to answer how a nation founded on an equality proposition could contain permanent caste.
% FOUNDING_PROBLEM_CORROBORATION: Expansion coalitions and affected claimant communities attest the problem remains live in transformed forms (disparity critiques, new claimant classes). Originalist jurists and independent constitutional historians — outside the beneficiary set — attest the founding problem is substantially resolved and further expansion is overreach rather than completion. The disagreement itself, documented on both sides from outside any single benefiting party, is the corroboration of 'contested'.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58 at interval end) because the burden the reading imposes decomposes into coordination costs of expansion (litigation, compliance restructuring, social disruption) plus enforcement burdens — not rent collection; the reading's own lights additionally discount the dissolution of unearned privilege as the price of justice rather than extraction, leaving the coordination/enforcement residual as the honest measure. Suppression (0.48) is a raw structural property, unscaled by power or scope: it records the active force holding the reading against rival readings and resistant practice — judicial compulsion, occasional federal troop deployment — while rival readings remain discursively alive (hence accessibility_collapse 0.40, well below mountain range). Resistance (0.72) is among the highest recorded for any constitutional constraint: civil war, Massive Resistance, and continuous contemporary contestation. Theater (0.38) is moderate-low but rising at interval end as ceremonial invocation outpaces operational commitment amid backlash. CYCLICAL PATTERN: the series oscillates rather than drifting monotonically — expansion surges (Reconstruction, Brown/Civil Rights era, Obergefell/Bostock) alternate with retrenchments (Plessy nadir, late-20th-century consolidation). Theater_ratio moves inversely to base_extractiveness: when enforcement is real the clause functions and ceremony is low; when enforcement withdraws the clause persists rhetorically at high theater (0.65 at Plessy) while imposing few costs. The oscillation is partly the mechanism itself: each retrenchment suspends the reading's costs while preserving its prestige intact, positioning it for revival — an intermittent-reinforcement dynamic at institutional scale. All three metric series share one nine-point grid (1863–2026); values at each point reflect the cycle phase at that date.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural data. From the payer seats (resistant_jurisdictions, trapped after secession was foreclosed; inherited_privilege_holders; successive_expansion_targets facing unpredictable doctrinal reclassification), the reading operates as coerced restructuring with no exit — extraction-dominant. From the beneficiary seats, the same structure is liberation-by-standard: a coordination device that converted dispersed exclusion into actionable claims — coordination-dominant. The federal_judiciary seat experiences administration and authority accrual: each controversy it adjudicates enlarges its interpretive dominion, so it neither pays nor merely coordinates — it compounds. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for marginalized_status_claimants (subsidized by the standard, constrained exit keeps them engaged rather than arbitraging) and equality_expansion_coalitions (mobile exit — reframing and forum-shifting — places them nearest the beneficiary pole). Victim declarations drive high directionality for the three payer seats, amplified by exit structure: resistant_jurisdictions sit at the full-target end because their exit (nullification, secession) was militarily foreclosed, making them trapped rather than merely constrained; inherited_privilege_holders and successive_expansion_targets are constrained but not trapped, moderating their effective extraction slightly. The federal_judiciary is deliberately left to the derivation chain's fallback: it appears in neither beneficiaries nor victims because its structural position is administrator-and-accruer, captured instead through its stakeholder situation and the gain_flow declaration (the reading's authority yield lands there). Scope is national, which modestly amplifies effective extraction through verification difficulty — equality compliance across a continent-scale polity is harder to verify than within a single jurisdiction. Suppression is reported unscaled per the structural-property rule; only extractiveness is scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   Three mislabelings are blocked. First, without the declared coordination function, the enforcement record alone (war, troops, compelled restructuring) would read as pure extraction; the genuine collective-action solution the clause provides for excluded groups forbids that collapse. Second, without the victim declarations, the clause's moral prestige would read as pure coordination; the concentrated, compelled, and expanding payer burden forbids that collapse. Third, the classic zombie path is structurally obstructed twice over: the founding problem is contested rather than dead (so the dead-mandate-plus-rearrangement mismatch does not fire), and the reading's mandate self-extends — each completed expansion defines the next frontier, so a mandate-outliving-function verdict may be unreachable in principle (see omega expansion_endpoint_ambiguity). The piton path is likewise blocked: gains demonstrably accrue to a named seat (federal_judiciary), so the diffuse-gains condition fails. What remains available is a degradation path through enforcement decay — theatrical maintenance of a standard the political branches decline to enforce — tracked by the enforcement_capacity_trajectory omega rather than by mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separability,
    'This story is one reading of the kernel all_men_created_equal. Does any classification computed for this story travel to the kernel label itself?',
    'Generate the sibling readings (originalist_reading, textualist_paradox_reading) as separate stories with their own epsilon and stakeholder surfaces; compare per-reading classifications before attributing anything to the kernel.',
    'Merging readings would misattribute extraction: the originalist-governed arrangement and the universalist mandate impose costs on nearly disjoint populations. Only per-reading results are meaningful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Committer-frame separation: one kernel, three constraints; classification is reading-indexed.').

omega_variable(
    originalist_counterfactual_structure,
    'Under the originalist sibling''s governance, which structural elements of this story flip?',
    'Author the originalist reading''s story and diff the beneficiary/victim sets: populations that are beneficiaries here (claimants) become the excluded there, and populations that are payers here (hierarchy holders) become the unconstrained there.',
    'Confirms that the two readings share a referent (the equality clause''s application) but instantiate constraints with inverted directionalities — the corpus''s cleanest natural experiment in reading-indexed classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_counterfactual_structure, conceptual, 'Structural delta against the originalist sibling reading.').

omega_variable(
    expansion_endpoint_ambiguity,
    'Does the universalist reading have a terminus — a state in which all statuses are equalized and the coordination function completes — or is it open-ended by design?',
    'Track whether the reading''s own doctrine ever declares completion, or whether each completed expansion generates the next frontier (as every round so far has). Absence of any declared end-state across the doctrinal record is the evidence.',
    'If a terminus exists, the constraint is transitional despite carrying no declared sunset clause — scaffold-flavored dynamics inside a tangled-rope shell. If open-ended, extraction-bearing expansion compounds indefinitely and no mandatrophy resolution is ever available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_endpoint_ambiguity, conceptual, 'Whether the reading''s mandate is completable or self-extending.').

omega_variable(
    extraction_justice_attribution,
    'Are the costs borne by the payer seats wrongful extraction, or the price of dismantling unearned privilege?',
    'Not resolvable by data alone: the reading''s own lights classify privilege-dissolution as justice''s cost and count only coordination and enforcement burdens as honest residual extraction; the payers'' lights classify the whole burden as extraction. The split is value-indexed.',
    'Determines whether the measured effective extraction indicts the constraint or measures justice''s price tag. Structure is unchanged either way; the normative reading of chi flips.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_justice_attribution, preference, 'Reading-indexed attribution of the payer burden: extraction versus cost-of-justice.').

omega_variable(
    victim_set_totalization_limit,
    'Is there a structural limit to the expanding victim set, or does the reading eventually classify every differential treatment as an equality violation?',
    'Trace doctrinal trajectories: whether each expansion round''s target class converges (finite stock of categorical exclusions) or regenerates (disparity-based standards that reproduce new targets indefinitely).',
    'Convergence implies the moderate-extraction profile stabilizes; totalization implies monotonic growth in the payer population and eventual resistance saturation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_totalization_limit, empirical, 'Whether the expanding victim set converges or totalizes.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will the political branches'' willingness to supply enforcement for the reading continue to erode, and does the judiciary''s identity-lock suffice to hold the standard alone?',
    'Observe subsequent enforcement episodes: whether court-vindicated expansions are implemented without political-branch backing, and whether non-enforcement becomes routinized.',
    'Falling enforcement capacity pushes the reading toward theatrical maintenance (rising theater_ratio, falling suppression_requirement) — a degradation path toward inertial persistence rather than active coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Trajectory of the enforcement machinery holding the reading in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1863, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ameq_universalist_tr_t1863, all_men_created_equal__universalist_reading, theater_ratio, 1863, 0.2).
narrative_ontology:measurement(ameq_universalist_tr_t1877, all_men_created_equal__universalist_reading, theater_ratio, 1877, 0.42).
narrative_ontology:measurement(ameq_universalist_tr_t1896, all_men_created_equal__universalist_reading, theater_ratio, 1896, 0.65).
narrative_ontology:measurement(ameq_universalist_tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.5).
narrative_ontology:measurement(ameq_universalist_tr_t1954, all_men_created_equal__universalist_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(ameq_universalist_tr_t1968, all_men_created_equal__universalist_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(ameq_universalist_tr_t1990, all_men_created_equal__universalist_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(ameq_universalist_tr_t2015, all_men_created_equal__universalist_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(ameq_universalist_tr_t2026, all_men_created_equal__universalist_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(ameq_universalist_be_t1863, all_men_created_equal__universalist_reading, base_extractiveness, 1863, 0.35).
narrative_ontology:measurement(ameq_universalist_be_t1877, all_men_created_equal__universalist_reading, base_extractiveness, 1877, 0.45).
narrative_ontology:measurement(ameq_universalist_be_t1896, all_men_created_equal__universalist_reading, base_extractiveness, 1896, 0.25).
narrative_ontology:measurement(ameq_universalist_be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(ameq_universalist_be_t1954, all_men_created_equal__universalist_reading, base_extractiveness, 1954, 0.5).
narrative_ontology:measurement(ameq_universalist_be_t1968, all_men_created_equal__universalist_reading, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement(ameq_universalist_be_t1990, all_men_created_equal__universalist_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(ameq_universalist_be_t2015, all_men_created_equal__universalist_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(ameq_universalist_be_t2026, all_men_created_equal__universalist_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ameq_universalist_su_t1863, all_men_created_equal__universalist_reading, suppression_requirement, 1863, 0.85).
narrative_ontology:measurement(ameq_universalist_su_t1877, all_men_created_equal__universalist_reading, suppression_requirement, 1877, 0.7).
narrative_ontology:measurement(ameq_universalist_su_t1896, all_men_created_equal__universalist_reading, suppression_requirement, 1896, 0.3).
narrative_ontology:measurement(ameq_universalist_su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(ameq_universalist_su_t1954, all_men_created_equal__universalist_reading, suppression_requirement, 1954, 0.75).
narrative_ontology:measurement(ameq_universalist_su_t1968, all_men_created_equal__universalist_reading, suppression_requirement, 1968, 0.8).
narrative_ontology:measurement(ameq_universalist_su_t1990, all_men_created_equal__universalist_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(ameq_universalist_su_t2015, all_men_created_equal__universalist_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(ameq_universalist_su_t2026, all_men_created_equal__universalist_reading, suppression_requirement, 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'all men are created equal' covers three structurally distinct claims. The originalist reading (scope fixed by 18th-century taxonomy) instantiates a bounded-membership constraint; the textualist_paradox reading (universal language irreconcilable with restricted application) instantiates a contradiction-diagnosis constraint; this universalist reading instantiates a self-expanding universal-standard constraint. Each has its own epsilon, beneficiary/victim structure, and classification. Edges run from this reading to both siblings because its expansion rounds change their operating environments: each vindication pressures the originalist reading to defend intent-fixity against accumulating counter-practice, and progressively shrinks the contradiction the textualist-paradox reading trades on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
