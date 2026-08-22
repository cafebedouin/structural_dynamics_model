% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Constitutional Authority Boundary — Parliamentary Primacy Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary primacy reading of the
 *   constitutional authority boundary kernel: where constitutional text
 *   exists, it is read as subordinate to the elected legislature, which
 *   retains final authority to define constitutional meaning through ordinary
 *   or entrenched legislation. Courts may review, but their rulings are
 *   contingent on legislative forbearance rather than structurally final.
 *   This is one of three readings of the same underlying kernel — the
 *   judicial supremacy reading and the coordinate construction reading are
 *   separate constraint stories with their own ε values and stakeholder
 *   structures, linked via network.affects_constraints. Under this reading's
 *   own lights, the extraction is low: the doctrine primarily functions as
 *   democratic coordination (preventing rule by unaccountable authority)
 *   rather than extraction, though it imposes real costs on constitutional
 *   minorities whose protections become majoritarian-contingent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.3).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Constitutional Authority Boundary — Parliamentary Primacy Reading").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '8ba50806-e0ce-461d-96a1-42f56a7e9816').
narrative_ontology:cs_kernel_codification('8ba50806-e0ce-461d-96a1-42f56a7e9816', distributed).
narrative_ontology:cs_authority_grounding('8ba50806-e0ce-461d-96a1-42f56a7e9816', practice).
narrative_ontology:cs_interpretation_layer_present('8ba50806-e0ce-461d-96a1-42f56a7e9816').
narrative_ontology:cs_reading_relation('8ba50806-e0ce-461d-96a1-42f56a7e9816', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('8ba50806-e0ce-461d-96a1-42f56a7e9816', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('8ba50806-e0ce-461d-96a1-42f56a7e9816', foundational, electoral_accountability_confers_final_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_accountability_confers_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('8ba50806-e0ce-461d-96a1-42f56a7e9816', electoral_accountability_confers_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('8ba50806-e0ce-461d-96a1-42f56a7e9816', foundational, no_body_may_permanently_bind_a_future_parliament).
narrative_ontology:cs_axiom_status(no_body_may_permanently_bind_a_future_parliament, holdable).
narrative_ontology:cs_axiom_grounding('8ba50806-e0ce-461d-96a1-42f56a7e9816', no_body_may_permanently_bind_a_future_parliament, conventional).
narrative_ontology:cs_axiom('8ba50806-e0ce-461d-96a1-42f56a7e9816', secondary, judicial_review_functions_as_advisory_dialogue_not_veto).
narrative_ontology:cs_axiom_status(judicial_review_functions_as_advisory_dialogue_not_veto, holdable).
narrative_ontology:cs_axiom_grounding('8ba50806-e0ce-461d-96a1-42f56a7e9816', judicial_review_functions_as_advisory_dialogue_not_veto, instrumental).
narrative_ontology:cs_reference_frame('8ba50806-e0ce-461d-96a1-42f56a7e9816', crown_versus_parliament_settlement).
narrative_ontology:cs_drift_state('8ba50806-e0ce-461d-96a1-42f56a7e9816', contemporary_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8ba50806-e0ce-461d-96a1-42f56a7e9816', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_majorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, entrenched_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_self_governance_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, legislative_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to enact, amend, or override constitutional meaning through ordinary or entrenched legislation. Can respond to judicial rulings it disagrees with by re-legislating, and treats judicial interpretation as advisory input rather than binding constraint on its own reading of the text. Collects the legitimacy premium of being the sole body directly accountable to the electorate.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, beneficiary).

% Benefit from having their electorally-expressed preferences translate into binding constitutional meaning without a court able to permanently block them. Can change the legislature's composition and, through it, change constitutional interpretation itself — their exit option is the ballot box rather than litigation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_majorities, beneficiary,
    organized, biographical, mobile, national).

% Retains a review function but its constitutional rulings can be legislatively overridden, narrowed, or entrenched around by the body it is reviewing. Cannot issue a final, unchallengeable constitutional ruling; its interpretive authority is contingent on legislative forbearance rather than structural finality.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, excluded).

% Groups whose rights claims depend on a counter-majoritarian check being able to bind the legislature permanently. Under this reading, any protection they win in court can be legislatively reversed by a subsequent majority, so their protection is only as durable as the current electoral coalition.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% Litigants seeking to establish rights as constitutionally fixed against future legislative majorities. Under parliamentary primacy their wins are provisional: even entrenched legislation is itself legislation, alterable by a legislature willing to pay the higher procedural cost of amendment.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, entrenched_rights_claimants, payer,
    moderate, generational, constrained, national).

% The written or unwritten instrument itself, treated under this reading as subordinate — a source the legislature consults and can supersede rather than a supreme law binding the legislature. Included for completeness; it is not an actor and captures nothing.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_text, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_text).

% Study Westminster-model systems (UK, historically New Zealand) where parliamentary sovereignty is the operative doctrine, comparing outcomes against judicial-supremacy and coordinate-construction systems to assess rights durability and majoritarian excess.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of who has final say when constitutional text is ambiguous or contested, by vesting that final say in the body most directly and repeatedly accountable to the electorate — avoiding rule by an unelected, life-tenured judiciary on contested value questions.
% TRANSFER_FUNCTION: Moves final interpretive authority from courts to the elected legislature; moves durability of rights protections from counter-majoritarian guarantee toward majoritarian contingency, from constitutional minorities and entrenched-rights claimants toward whatever coalition currently holds legislative power.
% ABSENT_VOICES: Constitutional minorities and rights claimants whose protections depend on permanence would object that this reading makes their status a standing electoral question rather than a settled guarantee; they are formally represented in the legislature but structurally unable to bind future majorities against themselves.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy were abandoned overnight in favor of judicial supremacy, courts would gain the power to permanently invalidate legislation with no legislative remedy — legislatures would lose the ability to respond to adverse rulings, rights litigation would displace electoral politics as the primary channel for constitutional change, and the balance of institutional power would shift decisively toward the judiciary.
% FOUNDING_PROBLEM: In Westminster-derived systems, the founding problem was preventing an unelected monarch or unelected judiciary from overriding the will of an elected representative body — parliamentary sovereignty emerged as the settlement of a specific historical contest (Crown vs. Parliament) for supremacy, later extended to a contest of judiciary vs. Parliament.
% FOUNDING_PROBLEM_CORROBORATION: Legislatures and democratic theorists attest the founding problem (unaccountable authority overriding elected will) remains live wherever courts strike down popular legislation. Rights scholars and comparative constitutionalists — outside the legislative beneficiary set — attest that the original problem (monarchical or aristocratic override) has been solved, and that unconstrained parliamentary sovereignty now creates a new problem (majoritarian override of minority rights) that the doctrine was never designed to address.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15-0.20 across the interval) because, from this reading's own vantage, the arrangement solves a genuine coordination problem — preventing an unelected body from permanently overriding elected preferences — and the cost imposed on rights claimants is a byproduct of majoritarian accountability, not a designed extraction. Suppression is moderate-low (0.22-0.30): the doctrine does not require heavy coercive machinery to maintain itself; it operates through ordinary legislative procedure and judicial deference norms, with a slow drift upward as legislatures increasingly use override powers explicitly (e.g., notwithstanding clauses, court-curbing statutes) rather than leaving deference implicit. Theater ratio stays low and stable — this is a live operative doctrine, not vestigial performance.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the structural beneficiary: it collects the interpretive authority and can adjust its own constraints. Democratic majorities benefit indirectly through responsive governance. The judiciary is a partial payer — it retains a real function but one that is contingent and reversible, so its directionality sits closer to target than beneficiary despite its institutional power. Constitutional minorities and entrenched-rights claimants are full targets: their protections are only as durable as the current legislative majority, and their exit options are structurally trapped or constrained because they cannot appeal to a body above the legislature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unaccountable Crown or judicial power overriding elected will) is contested as live: legislative-supremacy proponents point to ongoing instances of judicial invalidation of popular legislation as evidence the problem persists; rights scholars point out that the doctrine, once a check on monarchical/aristocratic power, has outlived that specific threat and now primarily functions to insulate current majorities from binding constraint by prior constitutional commitments. This divergence is exactly the kind of contested genealogy the founding_problem_status field is built to surface — the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a doctrine whose original justification may have shifted function without formal acknowledgment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_majoritarian_capture,
    'Is parliamentary sovereignty, as practiced, still solving the coordination problem it was built for (preventing unaccountable override), or has it become a mechanism by which current majorities extract from constitutional minorities who cannot bind future legislatures on their behalf?',
    'Longitudinal comparison of rights outcomes in parliamentary-sovereignty jurisdictions (UK, historically NZ) against judicial-supremacy and coordinate-construction jurisdictions on minority-protection durability across electoral cycles; frequency and content of legislative override of adverse rulings.',
    'If minority protections in parliamentary-sovereignty systems show systematically lower durability across power transitions than in judicial-supremacy systems, that supports reclassifying this reading''s real-world operation as tangled_rope (genuine democratic coordination function plus asymmetric extraction from minorities) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_majoritarian_capture, empirical, 'Whether legislative supremacy still functions as coordination or has drifted into majoritarian extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the constitutional_authority_boundary kernel best understood as a single ambiguous textual/practice-based kernel with three competing readings, or are there in fact separate historical kernels (the UK''s uncodified constitutional practice vs. codified-constitution systems that adopt override clauses) that only superficially resemble one another under the shared label ''parliamentary primacy''?',
    'Comparative constitutional history distinguishing systems with no codified constitutional text (pure Westminster model) from systems with codified constitutions containing explicit override or notwithstanding mechanisms (e.g. Canada''s Charter s.33).',
    'If these are structurally distinct kernels, this story should decompose further into an uncodified-practice reading and a codified-override reading, each with potentially different ε given the codified version formalizes and thereby increases the visibility (and contestability) of the override.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether uncodified and codified-with-override variants of parliamentary primacy are one kernel or two.').

omega_variable(
    entrenchment_durability_paradox,
    'Can legislation be genuinely ''entrenched'' under a parliamentary primacy reading, or does the reading''s own logic imply that any entrenchment mechanism is itself repealable by ordinary process, making entrenchment a matter of degree (procedural friction) rather than kind (structural bindingness)?',
    'Doctrinal analysis of whether courts under this reading have ever held an entrenchment clause to bind a subsequent Parliament against its own explicit repeal, versus treating entrenchment as raising only the political cost of change.',
    'If entrenchment is purely procedural friction, the extraction from entrenched_rights_claimants is higher than the low ε suggests — their protection is provisional, not structural, and the ε for this reading may need revision toward the upper end of its authored range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_durability_paradox, conceptual, 'Whether entrenched legislation is structurally binding or merely procedurally costly to repeal under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t8, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(cons_tr_t16, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(cons_tr_t32, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cons_be_t8, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(cons_be_t16, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement(cons_be_t32, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cons_su_t8, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(cons_su_t16, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement(cons_su_t32, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 32, 0.29).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the constitutional_authority_boundary kernel, each authored as a separate constraint story per the ε-invariance principle: parliamentary_primacy_reading (this story, ε≈0.15-0.20, legislature as final authority), judicial_supremacy_reading (courts as final unchallengeable arbiter, expected higher ε reflecting counter-majoritarian extraction concerns from a different vantage), and coordinate_construction_reading (distributed authority across three branches, no final arbiter). The three stories share the same underlying textual/practice kernel but instantiate structurally distinct constraints with different beneficiary/victim structures and different ε values, because each reading answers 'who has final say' differently and that answer determines who benefits and who pays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
