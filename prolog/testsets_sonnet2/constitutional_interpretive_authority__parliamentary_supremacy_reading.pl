% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-supremacy reading of the
 *   constitutional interpretive-authority kernel: the elected legislature
 *   holds final say over constitutional meaning, and no court may void a duly
 *   enacted statute on constitutional grounds. This is not a description of
 *   the contest among readings — the judicial-supremacy and
 *   coordinate-construction readings are separate constraints, each with
 *   their own ε and stakeholder structure. Here, ε is authored for the
 *   standing parliamentary-supremacy arrangement as this reading's own
 *   defenders and critics would assess it, not for any hypothetical
 *   rights-court regime it forecloses. The coordination function (avoiding
 *   institutional deadlock over constitutional finality) is real; the
 *   extraction (majoritarian entrenchment against durable minorities) is also
 *   real, which is why this reads as tangled_rope rather than pure rope or
 *   pure snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.48).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '95106c9a-87ae-4805-b842-37cb7618cd98').
narrative_ontology:cs_kernel_codification('95106c9a-87ae-4805-b842-37cb7618cd98', formalized).
narrative_ontology:cs_authority_grounding('95106c9a-87ae-4805-b842-37cb7618cd98', lineage).
narrative_ontology:cs_interpretation_layer_present('95106c9a-87ae-4805-b842-37cb7618cd98').
narrative_ontology:cs_reading_relation('95106c9a-87ae-4805-b842-37cb7618cd98', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('95106c9a-87ae-4805-b842-37cb7618cd98', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('95106c9a-87ae-4805-b842-37cb7618cd98', foundational, electoral_accountability_supersedes_judicial_finality).
narrative_ontology:cs_axiom_status(electoral_accountability_supersedes_judicial_finality, holdable).
narrative_ontology:cs_axiom_grounding('95106c9a-87ae-4805-b842-37cb7618cd98', electoral_accountability_supersedes_judicial_finality, conventional).
narrative_ontology:cs_axiom('95106c9a-87ae-4805-b842-37cb7618cd98', secondary, legislative_self_correction_sufficient_for_rights_protection).
narrative_ontology:cs_axiom_status(legislative_self_correction_sufficient_for_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('95106c9a-87ae-4805-b842-37cb7618cd98', legislative_self_correction_sufficient_for_rights_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('95106c9a-87ae-4805-b842-37cb7618cd98', electoral_mandate_finality).
narrative_ontology:cs_drift_state('95106c9a-87ae-4805-b842-37cb7618cd98', contemporary_rights_jurisprudence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('95106c9a-87ae-4805-b842-37cb7618cd98', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, cabinet_executive).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, unwritten_constitutional_conventions_adherents).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, future_electoral_minorities).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_self_correction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass, amend, or repeal any statute, including ones touching rights or procedure, with no court empowered to strike the result down. Frames this as democratic legitimacy: the people's elected representatives, not appointed judges, should have the final word. Benefits from being able to correct or reverse unpopular judicial-style interpretations by ordinary legislative majority rather than constitutional amendment.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority, beneficiary).

% Draws confidence from and typically controls the parliamentary majority; benefits from the absence of judicial veto over its legislative program, since a hostile court cannot void executive-sponsored statutes. Faces electoral exit risk but not judicial constraint on enacted law.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, cabinet_executive, beneficiary,
    institutional, biographical, mobile, national).

% Groups whose claims (religious minorities, political dissidents, non-citizens, prisoners) depend on a rights floor that survives a hostile majority. Under this reading, no court can void a statute that strips or narrows their protections; their only recourse is persuading the same majority that enacted the harm, or waiting for electoral turnover. Exit means leaving the jurisdiction, which most cannot do.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_rights_claimants, payer,
    powerless, generational, trapped, national).

% Retains ordinary adjudicative and statutory-interpretation functions but is denied the power to nullify a parliamentary act on constitutional grounds. Can flag incompatibility, issue declarations, or interpret narrowly, but cannot strike the statute down; the legislature can override or ignore any judicial signal. The judiciary would, under either sibling reading, hold a materially different power.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, excluded,
    institutional, civilizational, constrained, national).

% Anyone who becomes an out-of-power political or demographic minority under a future majority inherits whatever the current majority enacts as final and judicially unreviewable; they bear the risk that today's procedural convenience becomes tomorrow's entrenched disadvantage, with no counter-majoritarian check available to them either.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, future_electoral_minorities, payer,
    powerless, generational, trapped, national).

% Study and debate the comparative merits of parliamentary supremacy against judicial and coordinate-construction models, drawing on cross-jurisdictional evidence about rights protection, legislative self-restraint, and majoritarian overreach under each arrangement.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates ultimate interpretive authority in a single, electorally accountable body, avoiding the coordination problem of two branches both claiming final say over what the constitution means, and allowing rapid correction of interpretations through ordinary politics rather than supermajority amendment or judicial reversal.
% TRANSFER_FUNCTION: Moves the power to settle constitutional meaning from an insulated judicial body toward whichever electoral coalition currently controls the legislature, and correspondingly moves risk of majoritarian overreach onto groups without durable majority-coalition power.
% ABSENT_VOICES: Minority rights claimants and future electoral minorities have no seat with veto-equivalent power in this arrangement; their objections are heard, if at all, as political lobbying to the very majority whose actions they contest, not as a claim a court can adjudicate against the statute.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy were displaced overnight by judicial supremacy or coordinate construction, courts would gain (or share) power to void statutes on constitutional grounds; legislative majorities would lose the ability to treat ordinary statute as the final word on rights questions, and minority claimants would gain a forum with power to bind the majority rather than merely petition it.
% FOUNDING_PROBLEM: Historically arose from the claim that appointed, life-tenured judges lack democratic legitimacy to override the will of elected representatives, and from a preference for political accountability and correctability over insulated judicial finality — often traced to a tradition distrustful of judicial policymaking and confident in periodic electoral correction.
% FOUNDING_PROBLEM_CORROBORATION: Sitting majorities and much of the political-science literature on democratic legitimacy attest the problem (unaccountable judicial power) remains live. Minority-rights scholars, comparative constitutionalists, and international human-rights bodies outside the legislative-beneficiary set attest that the arrangement has, in several jurisdictions, been used to entrench majoritarian outcomes against groups the original justification did not anticipate — corroboration is genuinely split rather than absent.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises modestly over the interval (0.28 to 0.42) as I model accumulating instances where legislative majorities use unreviewable statutory power to narrow protections previously assumed durable — a realistic drift pattern for a supremacy doctrine tested across successive governments. Suppression is moderate (0.48 at t=50): the suppressive force is legal/structural (no forum exists to challenge the statute) rather than violent, and grows slowly as precedent accumulates that no judicial check will materialize. Theater ratio stays low (0.20) because the coordination function — settling the accountability question — remains substantially functional rather than performative; this is a genuinely operating doctrine, not a hollowed-out one. Accessibility collapse is moderate (0.5): the doctrine does not eliminate political alternatives (electoral change, later legislative repeal), but it does collapse judicial and rights-based alternatives entirely for the duration of a given majority's tenure.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting parliamentary majority and the cabinet it commands are structural beneficiaries: the doctrine directly vests them with final, unreviewable interpretive discretion (low d, near the beneficiary end). Minority rights claimants and future electoral minorities are structural targets: they bear the risk of majoritarian statutes with no counter-majoritarian recourse (high d, near the target end), and their exit options are trapped rather than mobile because leaving the jurisdiction is rarely a live option. The judiciary is neither beneficiary nor victim in the extraction sense but is structurally excluded from the power the sibling reading would grant it — its situation is best captured as excluded rather than payer, since it does not bear the cost of the arrangement so much as lose the authority the doctrine denies it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distrust of unaccountable judicial policymaking) remains partially live in most jurisdictions that retain this reading, which is why founding_problem_status is authored as contested rather than dead — treating it as simple mandatrophy would understate the genuine, ongoing democratic-accountability argument for the doctrine. But the corroboration split (legislative beneficiaries attest continued live function; outside rights scholars and comparative constitutionalists attest majoritarian entrenchment beyond the original justification) is exactly the kind of divergence the tangled_rope classification is built to hold without collapsing to either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_majoritarian_entrenchment,
    'Is parliamentary supremacy best understood as a genuine democratic-accountability mechanism, or as a structure that legitimates majoritarian entrenchment against minorities who cannot secure electoral leverage?',
    'Comparative empirical study of rights outcomes for durable minorities across parliamentary-supremacy, judicial-supremacy, and coordinate-construction jurisdictions over multiple electoral cycles.',
    'If entrenchment dominates empirically, the tangled_rope classification''s extraction component is validated and likely understated; if genuine accountability correction dominates, the arrangement drifts closer to a rope with occasional extraction episodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_majoritarian_entrenchment, empirical, 'Whether parliamentary supremacy functions primarily as accountability or as majoritarian entrenchment.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading diverge structurally from the judicial_supremacy_reading and coordinate_construction_reading siblings — is the disagreement about WHO holds final authority, or about WHETHER any single body should hold final authority at all?',
    'This is committer-structure documentation, not an empirically resolvable question: the disagreement is located precisely at the allocation of final interpretive authority. Parliamentary supremacy and judicial supremacy agree a single body should hold finality and disagree only about which body; coordinate construction rejects the premise that any single body should hold it. A sibling reading adopting coordinate construction would not merely reassign the beneficiary seat — it would dissolve the beneficiary/victim structure entirely, since no single actor gains unreviewable discretion.',
    'This locates the exact structural axis of contest for downstream network analysis: parliamentary_supremacy and judicial_supremacy are structurally closer to each other (same-shape different-holder) than either is to coordinate_construction (different-shape).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Documents where the three kernel readings actually diverge structurally, for the committer frame.').

omega_variable(
    convention_vs_codified_limit,
    'Does this reading''s persistence depend on unwritten constitutional convention (self-restraint) rather than any codified limit, and if so, how stable is that convention under sustained majoritarian pressure?',
    'Track instances where a parliamentary majority tests or breaches previously assumed conventions (e.g., rights-protective statutory norms) without judicial or codified sanction, across multiple jurisdictions retaining this doctrine.',
    'If conventions prove fragile under sustained pressure, effective suppression and extraction are understated by this story''s metrics, which assume convention holds reasonably well across the measured interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convention_vs_codified_limit, empirical, 'Whether the doctrine''s moderating conventions are robust or fragile under majoritarian pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'final constitutional interpretive authority' per the ε-invariance principle. Each sibling reading has its own ε, beneficiary/victim structure, and classification: parliamentary_supremacy_reading (this story, tangled_rope) vests the legislature with unreviewable finality; judicial_supremacy_reading vests courts with nullification power over statutes; coordinate_construction_reading denies any single branch final authority, treating constitutional meaning as constructed through ongoing inter-branch and political contestation. The three are linked, not merged, because measuring 'final interpretive authority' one way (legislature-holds-it) versus another way (courts-hold-it) versus a third way (no one holds it) yields different ε, different victim sets, and different classifications — exactly the signal the ε-invariance test is designed to catch.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
