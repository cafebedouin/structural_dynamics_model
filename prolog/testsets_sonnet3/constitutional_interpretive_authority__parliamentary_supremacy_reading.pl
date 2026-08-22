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
 *   human_readable: Parliamentary Supremacy Reading of Final Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary supremacy reading of the
 *   contested kernel over final constitutional interpretive authority: the
 *   elected legislature, not the judiciary, holds the last word on what the
 *   constitution permits, and courts lack the power to void validly enacted
 *   statutes. The reading legitimates coercive statutory power through
 *   electoral mandate rather than through judicially-enforced rights
 *   guarantees. This is a genuine coordination mechanism (it resolves the
 *   who-decides-last problem cleanly and ties ultimate authority to electoral
 *   accountability) but it also structurally exposes permanent minorities and
 *   rights claimants to majoritarian extraction with no institutional remedy,
 *   requiring active enforcement (courts must in fact decline to strike down
 *   statutes, even when persuaded of rights incompatibility) to hold.
 *
 * KEY AGENTS:
 *   - sitting_legislative_majority: agenda_setter/beneficiary (institutional/arbitrage) — sets and enforces final interpretive discretion
 *   - electoral_mandate_holders: beneficiary (organized/mobile) — legitimating constituency, rotates with electoral fortune
 *   - constitutional_minorities: payer (powerless/trapped) — bear extraction with no judicial backstop
 *   - litigants_seeking_rights_review: payer (powerless/trapped) — no remedy against unambiguous statute
 *   - judiciary_as_institution: payer/observer (institutional/constrained) — retains interpretation, loses nullification power
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — sees the full comparative structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.38).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Final Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '85c98dbe-9af1-4121-a5c0-d2f695d44283').
narrative_ontology:cs_kernel_codification('85c98dbe-9af1-4121-a5c0-d2f695d44283', formalized).
narrative_ontology:cs_authority_grounding('85c98dbe-9af1-4121-a5c0-d2f695d44283', lineage).
narrative_ontology:cs_interpretation_layer_present('85c98dbe-9af1-4121-a5c0-d2f695d44283').
narrative_ontology:cs_reading_relation('85c98dbe-9af1-4121-a5c0-d2f695d44283', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('85c98dbe-9af1-4121-a5c0-d2f695d44283', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('85c98dbe-9af1-4121-a5c0-d2f695d44283', foundational, electoral_accountability_as_ultimate_legitimacy_source).
narrative_ontology:cs_axiom_status(electoral_accountability_as_ultimate_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('85c98dbe-9af1-4121-a5c0-d2f695d44283', electoral_accountability_as_ultimate_legitimacy_source, conventional).
narrative_ontology:cs_axiom('85c98dbe-9af1-4121-a5c0-d2f695d44283', foundational, no_unelected_body_may_nullify_elected_legislative_will).
narrative_ontology:cs_axiom_status(no_unelected_body_may_nullify_elected_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('85c98dbe-9af1-4121-a5c0-d2f695d44283', no_unelected_body_may_nullify_elected_legislative_will, conventional).
narrative_ontology:cs_reference_frame('85c98dbe-9af1-4121-a5c0-d2f695d44283', parliamentary_sovereignty_as_founding_settlement).
narrative_ontology:cs_drift_state('85c98dbe-9af1-4121-a5c0-d2f695d44283', contemporary_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('85c98dbe-9af1-4121-a5c0-d2f695d44283', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_legislative_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_holders).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, litigants_seeking_rights_review).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary_as_institution).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_as_ultimate_legitimacy_source).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes statutes with the knowledge that no court can void them for incompatibility with fundamental rights or constitutional principle; treats electoral victory as conferring final interpretive discretion over what the constitution permits. Can rewrite, override, or ignore prior judicial commentary at will through ordinary legislation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_legislative_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_legislative_majority, beneficiary).

% The voting majority whose periodic electoral choice is treated as the sole legitimating mechanism for constitutional meaning; benefits from a system where their current preferences (via elected representatives) cannot be checked by unelected judges, but this benefit rotates with electoral fortune and offers no protection once out of majority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_holders, beneficiary,
    organized, generational, mobile, national).

% Groups whose rights claims depend on countermajoritarian protection — permanent electoral minorities, disfavored groups, unpopular claimants — bear the cost of a system with no judicial backstop against majoritarian legislation. Cannot exit the jurisdiction easily and have no institutional forum empowered to override a hostile majority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities, payer,
    powerless, generational, trapped, national).

% Individuals who bring claims that a statute violates constitutional rights find courts structurally barred from providing a remedy beyond interpretation-in-favor-of-rights where text permits; where a statute is unambiguous, courts must apply it regardless of rights impact. Their only recourse is political mobilization for legislative repeal.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, litigants_seeking_rights_review, payer,
    powerless, biographical, trapped, national).

% Courts retain interpretive and declaratory functions (statutory construction, declarations of incompatibility) but no power to strike down or refuse to apply validly enacted legislation. The institution absorbs the reputational and functional cost of being unable to provide final remedies in rights cases, while its interpretive rulings can be legislatively overridden without constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary_as_institution, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary_as_institution, observer).

% Study parliamentary sovereignty systems (e.g., Westminster-model jurisdictions) comparatively, assessing rights outcomes, legislative override patterns, and the practical operation of political constitutionalism relative to judicial-supremacy systems elsewhere.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, sitting_legislative_majority).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Locates ultimate constitutional authority in a single, electorally accountable body, avoiding the coordination problem of two branches each claiming final interpretive say over the same text — settles conflicts by fiat of the body most directly answerable to voters.
% TRANSFER_FUNCTION: Moves the power to have the last word on constitutional meaning from the judiciary to the elected legislature; moves the practical protection available to rights claimants and permanent minorities from a judicially-enforceable guarantee to a politically-contingent one.
% ABSENT_VOICES: Permanent electoral minorities and unpopular rights claimants have no institutional voice with binding force in this arrangement; they can lobby and litigate for interpretation but cannot compel a remedy against an unambiguous statute. Future minorities not yet identified as such are also structurally unrepresented.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy were displaced overnight (e.g., by a court asserting the power to strike down primary legislation), the legislature would lose the ability to enact rights-infringing statutes with legal finality, litigants would gain a judicial remedy path, and the balance of institutional power would shift substantially toward courts — a structurally different constitutional order.
% FOUNDING_PROBLEM: Historically built to resolve a specific power struggle (in the paradigm case, the subordination of the Crown and unelected judicial/aristocratic authority to an elected representative body) and to ground legitimacy in popular consent rather than judicial or monarchical fiat.
% FOUNDING_PROBLEM_CORROBORATION: Legislative majorities and political theorists in the republican/political-constitutionalist tradition attest the founding problem (checking unaccountable judicial or executive power) remains live. Rights litigants, comparative scholars documenting legislative overrides of rights-protective rulings, and international human-rights bodies attest that the arrangement now often functions to insulate majoritarian statutes from rights scrutiny rather than to check unaccountable power — a corroboration source outside the legislative beneficiary set.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) reflects that the arrangement is not itself the abuse — it is the removal of a check on abuse, so extraction is moderate and rises slowly (0.28 to 0.42 over the interval) as legislative majorities learn the boundaries of unchecked statutory power and increasingly legislate at the edge of rights concerns once tested against no judicial veto. Suppression (0.38) is likewise moderate: rights claimants are not physically coerced into silence, but the doctrine forecloses a legal remedy path, which is a real (if procedural) suppression of contestation. Theater ratio is low (0.2) because the coordination function — settling who decides constitutional meaning — is genuinely performed, not merely staged; there is little pretense involved in the arrangement's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative majority's seat this looks like Rope: a clean, electorally legitimated coordination solution to the who-decides-last problem. From the seat of constitutional minorities and rights litigants the same structure computes closer to Tangled Rope or worse: a genuine coordination function (someone must have final say) riding alongside asymmetric extraction (that final say is wielded, over time, against groups with no institutional recourse). The engine should register this divergence as the diagnostic signal, not resolve it toward either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority and the electoral mandate holders it answers to are beneficiaries: they retain unchecked discretion over statutory meaning and face no institutional override. Constitutional minorities, rights litigants, and the judiciary itself are payers: minorities and litigants because they cannot obtain a binding remedy against majoritarian legislation, the judiciary because it absorbs the institutional cost of being able to interpret but not enforce against the legislature. Directionality tracks trapped exit options for minorities/litigants (they cannot easily exit the jurisdiction or route around parliamentary authority) versus the arbitrage-grade exit of the legislative majority (which can rewrite the rules that would otherwise bind it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking unaccountable judicial, executive, or aristocratic power via electoral accountability — is genuinely contested rather than simply dead or simply live. Where legislatures remain broadly rights-respecting and courts historically overreached, the founding problem retains force. Where legislative majorities increasingly use unchecked statutory power against disfavored minorities, the arrangement has drifted from its founding justification toward the very majoritarian overreach it was partly designed to avoid (subordination to unaccountable power, just relocated from crown/judiciary to legislative majority). Classifying this as Tangled Rope rather than Rope or Snare outright prevents both errors: treating a real coordination function as pure extraction, and treating persistent extraction of minorities as accidental noise in an otherwise benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_mandate_as_sufficient_legitimation,
    'Does periodic electoral accountability provide sufficient legitimation for a legislature''s power to override or ignore fundamental rights claims, absent any judicial backstop?',
    'Comparative empirical study of rights outcomes in parliamentary-supremacy jurisdictions versus judicial-supremacy jurisdictions over matched historical periods and matched rights categories (e.g., minority protections, criminal procedure, free expression).',
    'If electoral accountability reliably produces comparable rights protection to judicial review, the tangled_rope classification''s extraction component weakens toward rope; if legislative majorities in these systems systematically erode minority protections relative to judicial-supremacy peers, it strengthens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_mandate_as_sufficient_legitimation, empirical, 'Whether electoral mandate substitutes adequately for judicial rights review.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with judicial_supremacy_reading and coordinate_construction_reading live — is it about WHO decides (institutional locus) or about WHETHER any single body should have final authority at all (structural design)?',
    'Trace historical constitutional drafting debates and subsequent doctrinal commentary within a single jurisdiction that has moved between these readings (e.g., a jurisdiction adopting a rights charter with declarations-of-incompatibility rather than strike-down power) to see which axis actually shifted.',
    'If the disagreement is purely about institutional locus, the three readings are more interchangeable in practice than in theory (courts and legislatures converge via dialogue anyway); if it is about structural design, the readings produce genuinely different rights outcomes and the classification gap between this reading and judicial_supremacy_reading is real and load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel contest is about locus of authority or about structural design of authority-holding itself.').

omega_variable(
    future_minority_unrepresentedness,
    'Is the structural exclusion of not-yet-identified future minorities from this arrangement''s protective scope an irreducible feature of any majoritarian legitimation scheme, or specifically a feature of the parliamentary supremacy reading?',
    'Compare protection for emergent/unanticipated minority claims (e.g., new technology-driven rights claims) across parliamentary-supremacy and judicial-supremacy systems to see whether judicial review meaningfully anticipates claims legislatures do not.',
    'If judicial review systems show no better anticipatory protection, the payer classification for constitutional_minorities is a feature of majoritarianism generally, not this reading specifically, softening the tangled_rope severity attributed to this reading in particular.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_minority_unrepresentedness, conceptual, 'Whether unrepresentedness of future minorities is reading-specific or general to majoritarian legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t12, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(cons_tr_t24, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(cons_tr_t36, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 36, 0.16).
narrative_ontology:measurement(cons_tr_t48, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 48, 0.18).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t12, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(cons_be_t24, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(cons_be_t36, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 36, 0.38).
narrative_ontology:measurement(cons_be_t48, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 48, 0.4).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t12, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(cons_su_t24, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(cons_su_t36, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 36, 0.34).
narrative_ontology:measurement(cons_su_t48, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 48, 0.36).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language kernel 'constitutional_interpretive_authority' per the ε-invariance principle. judicial_supremacy_reading and coordinate_construction_reading are sibling constraints, each with independently authored ε, beneficiaries, and victims — not alternative measurements of this same constraint. The three form a constraint family; this reading's declared beneficiary (legislative majority) and victim (constitutional minorities, litigants, judiciary) sets differ structurally from the siblings' sets because each reading allocates final authority differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
