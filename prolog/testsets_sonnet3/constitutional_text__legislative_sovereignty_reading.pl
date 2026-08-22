% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Supremacy (Parliament-Final, Notwithstanding-Clause Systems)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates the legislative sovereignty reading of the
 *   constitutional text kernel: the text is read as establishing parliament,
 *   not the courts, as the final authority on constitutional meaning, with
 *   judicial review operating in an advisory capacity that the legislature
 *   can override through mechanisms like notwithstanding clauses or simple
 *   re-enactment. This is one of three structurally distinct readings of the
 *   same kernel (the others being judicial_supremacy_reading and
 *   popular_sovereignty_reading, authored as separate constraint stories) —
 *   the ε-invariance principle requires this story to describe only this
 *   reading's own arrangement, at this reading's own extraction level,
 *   without averaging over or importing the other readings'
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - parliamentary_majority: primary agenda-setter and beneficiary (institutional/arbitrage) — controls both legislation and override authority
 *   - executive_governing_coalition: secondary beneficiary (institutional/mobile) — drives the agenda the majority enacts and shields
 *   - constitutional_courts: structurally subordinated reviewer (institutional/constrained) — advisory only, findings can be overridden
 *   - discrete_minorities and unpopular_rights_claimants: primary targets (powerless/trapped) — bear the cost when overrides nullify favorable rulings
 *   - electorate: diffuse beneficiary (organized/constrained) — retains majoritarian control over contested constitutional questions
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — studies override invocation patterns cross-nationally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.52).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.48).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Supremacy (Parliament-Final, Notwithstanding-Clause Systems)").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '2dc9de25-731c-4aa3-b876-62946d86ea81').
narrative_ontology:cs_kernel_codification('2dc9de25-731c-4aa3-b876-62946d86ea81', fixed_text).
narrative_ontology:cs_authority_grounding('2dc9de25-731c-4aa3-b876-62946d86ea81', practice).
narrative_ontology:cs_interpretation_layer_present('2dc9de25-731c-4aa3-b876-62946d86ea81').
narrative_ontology:cs_reading_relation('2dc9de25-731c-4aa3-b876-62946d86ea81', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dc9de25-731c-4aa3-b876-62946d86ea81', constitutional_text__popular_sovereignty_reading, influences).
narrative_ontology:cs_axiom('2dc9de25-731c-4aa3-b876-62946d86ea81', foundational, electoral_accountability_is_the_legitimate_seat_of_final_constitutional_authority).
narrative_ontology:cs_axiom_status(electoral_accountability_is_the_legitimate_seat_of_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('2dc9de25-731c-4aa3-b876-62946d86ea81', electoral_accountability_is_the_legitimate_seat_of_final_constitutional_authority, conventional).
narrative_ontology:cs_axiom('2dc9de25-731c-4aa3-b876-62946d86ea81', secondary, judicial_review_without_override_displaces_democratic_self_governance).
narrative_ontology:cs_axiom_status(judicial_review_without_override_displaces_democratic_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('2dc9de25-731c-4aa3-b876-62946d86ea81', judicial_review_without_override_displaces_democratic_self_governance, instrumental).
narrative_ontology:cs_reference_frame('2dc9de25-731c-4aa3-b876-62946d86ea81', westminster_parliamentary_supremacy_tradition).
narrative_ontology:cs_drift_state('2dc9de25-731c-4aa3-b876-62946d86ea81', contemporary_rights_charter_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2dc9de25-731c-4aa3-b876-62946d86ea81', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, executive_governing_coalition).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, discrete_minorities).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, unpopular_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, electorate).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, majoritarian_democratic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass ordinary legislation and, when courts flag a rights conflict, to re-enact the measure under a notwithstanding or override clause. Treats judicial constitutional review as advisory input rather than a binding veto. Sets the terms on which constitutional meaning is finally settled, since it controls both the legislative text and the override mechanism that can supersede judicial interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, beneficiary).

% Drives the legislative agenda that the parliamentary majority enacts and, where needed, shields from judicial invalidation via override. Benefits from a low-rigidity constitutional order because policy priorities can be implemented without the delay or reversal risk that binding judicial review would impose.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, executive_governing_coalition, beneficiary,
    institutional, biographical, mobile, national).

% Reviews legislation for constitutional conformity and issues findings, but under this reading those findings are advisory: the legislature may override them by re-enactment or explicit notwithstanding declaration. The court's institutional voice on constitutional meaning is present but structurally subordinated to the body whose acts it reviews.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_courts, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, constitutional_courts, observer).

% Bears the practical cost when a rights claim loses in the political process: even a favorable judicial ruling can be nullified by legislative override if the majority disagrees. Has no institutional actor above the legislature to appeal to once the override is invoked. Exit is not realistic — citizenship and residence are not exit options from national constitutional structure.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, discrete_minorities, payer,
    powerless, biographical, trapped, national).

% Individuals or small groups asserting constitutional rights against majoritarian preference (e.g. criminal procedure protections, speech unpopular with the electorate, religious minority practice). Wins in court can be reversed by a same-session or next-session override vote, so the durability of any judicial protection they obtain depends entirely on the majority's continued forbearance.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, unpopular_rights_claimants, payer,
    powerless, immediate, trapped, national).

% Retains the ability, through ordinary elections, to have its preferences on contested constitutional questions prevail over unelected judicial interpretation, without needing a supermajority amendment process. Benefits from the sense that constitutional meaning tracks democratic will rather than being locked in by judicial precedent it cannot easily revisit.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, electorate, beneficiary,
    organized, generational, constrained, national).

% Studies override-clause systems (Canada's Section 33, UK parliamentary sovereignty, Israel's override mechanism debates) comparatively, assessing how often overrides are invoked, against whom, and whether minority-protective norms survive as convention even without judicial finality.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of who has final say over contested constitutional meaning by vesting it in the institution most directly accountable to the electorate, avoiding rule by an unelected judiciary on questions the polity considers properly political — this coordinates legitimacy around electoral accountability rather than judicial expertise.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional rights claims from courts to the legislative majority; in practice this transfers the durability of minority rights protections from a judicially-guaranteed floor to a politically-contingent convention, redistributing risk from the majority (who retain control) to minorities (who lose a binding backstop).
% ABSENT_VOICES: Discrete minorities and unpopular rights claimants are formally represented through ordinary electoral and parliamentary channels but are, by definition, unable to secure durable protection against majority preference through those same channels — the override mechanism nullifies the one channel (judicial review) structurally designed to hear them independent of majority approval.
% DISAPPEARANCE_RATIONALE: If legislative override authority disappeared and judicial rulings became final, the balance of constitutional power would shift decisively toward courts: legislation currently vulnerable to override would instead require constitutional amendment to survive adverse rulings, materially raising the durability of minority-protective precedent and lowering the majority's capacity for rapid policy reversal of judicially-identified rights violations.
% FOUNDING_PROBLEM: Built to resolve the counter-majoritarian difficulty: the concern that unelected judges wielding final constitutional authority displace democratic self-governance on contested moral and political questions the polity should settle for itself.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary supremacy theorists and many democratic theorists outside the legislature (e.g. Waldron's work on judicial review skepticism) attest the counter-majoritarian concern remains live and is not merely a self-serving legislative rationale. Minority-rights advocates and comparative constitutional scholars studying override invocation patterns attest that in practice overrides are disproportionately invoked against minority claimants rather than in genuine disputes about reasonable disagreement, suggesting the founding problem has been partially supplanted by majoritarian convenience — corroboration exists on both sides from outside the legislature itself.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.52 (rising modestly over the interval) because the coordination function is genuine — resolving the counter-majoritarian difficulty by anchoring final authority in electoral accountability is a real problem with a real solution here — but the mechanism also enables systematic, if intermittent, extraction from minority claimants whenever the majority's preferences and a minority's constitutional claim diverge. Suppression (0.48) reflects that override authority is not merely latent but actively exercised in specific jurisdictions to nullify judicial rulings, which functions as an enforcement mechanism against minority claims. Theater ratio is authored low-moderate (0.28) because override authority is substantively exercised, not merely symbolic — though the rising trajectory reflects growing convention-based restraint norms developing alongside the formal power, some of which is performative reassurance rather than binding limitation. Accessibility collapse is moderate (0.4): a rights claimant who loses via override still has electoral, referendum, and future-legislature avenues, so alternatives are constrained rather than fully collapsed. Resistance is elevated (0.55) because override invocations against minority-protective rulings routinely generate sustained public, legal-academic, and international human-rights-body pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary majority's seat, this arrangement is genuine democratic coordination: a solution to the problem of unaccountable judicial power. From the seat of discrete minorities and unpopular rights claimants, the identical structure operates as extraction — a mechanism that converts what should be a rights guarantee into a conditional grant revocable whenever the majority's preferences shift. The engine should compute these as different seat-level classifications from the same structural data; the divergence is not an error to reconcile but the central analytical fact about legislative-sovereignty systems.
 *
 * DIRECTIONALITY LOGIC:
 *   The parliamentary majority and executive coalition sit near the full-beneficiary end of directionality: they set the rules, control the override mechanism, and bear essentially no cost from its exercise. The electorate sits closer to symmetric — genuine coordination benefit (electoral accountability preserved) with only diffuse indirect cost. Discrete minorities and unpopular rights claimants sit near the full-target end: trapped exit options (national citizenship is not a genuine exit), immediate time horizon when an override strikes down a favorable ruling, and no institutional recourse once the override is invoked. Constitutional courts occupy an unusual position — institutionally powerful in general but structurally constrained specifically with respect to this constraint, since their constitutional-review output can be nullified by the body they review.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (counter-majoritarian difficulty) is authored as contested rather than resolved-dead or clearly-live: it remains a genuine live concern in democratic theory, but the empirical pattern of override invocation in comparative practice (disproportionately targeting minority claimants rather than genuine reasonable-disagreement cases) suggests the mandate has partially drifted from its original justification toward majoritarian convenience. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: calling it a pure snare would ignore the genuine coordination function it serves for the electorate and majority; calling it a pure rope would ignore the asymmetric, actively-enforced cost it imposes on minorities who cannot exit the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_convention_vs_formal_power,
    'Is the actual protective floor for minority rights under legislative sovereignty set by the formal override power (which could in principle be invoked against any ruling) or by an unwritten political convention restraining its use (which varies by jurisdiction and political culture)?',
    'Comparative empirical study of override invocation frequency and target patterns across jurisdictions with notwithstanding-style mechanisms (Canada Section 33, historical UK practice, proposed override mechanisms elsewhere), tracking whether restraint holds under political stress (e.g. populist governing majorities) or erodes.',
    'If convention reliably restrains override use even under stress, effective extraction is lower than the formal power suggests and the arrangement functions closer to a genuine rope with a rarely-triggered safety valve. If convention erodes under stress, effective extraction converges toward the formal power ceiling and the tangled_rope classification understates the risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_convention_vs_formal_power, empirical, 'Whether minority protection rests on convention or formal power, and how robust that convention is.').

omega_variable(
    reading_selection_is_itself_political,
    'Is the choice among the three kernel readings (legislative sovereignty, judicial supremacy, popular sovereignty) itself a politically contingent act — i.e., does a polity''s selection of this reading track which institution its founding political coalition expected to control?',
    'Constitutional-drafting historical analysis: examine whether founding coalitions that expected durable electoral majorities favored legislative-sovereignty language, while coalitions expecting to lose elections favored judicial-supremacy language with entrenched rights review.',
    'If reading selection tracks anticipated institutional control, the ''coordination function'' claimed for this reading (resolving counter-majoritarian difficulty) is partly a post-hoc justification for a power allocation chosen for strategic reasons — this would not change the authored ε for this story, but it would inform how much weight the founding_problem narrative should carry as an independent justification versus a rationalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_is_itself_political, conceptual, 'Whether the kernel reading a polity adopts reflects genuine constitutional theory commitment or strategic anticipation of institutional control.').

omega_variable(
    minority_definition_boundary,
    'Which minorities are actually vulnerable under this reading — is it any electoral minority on any contested question, or specifically minorities whose claims cannot be reframed as majoritarian self-interest (i.e., discrete and insular minorities in the constitutional-theory sense)?',
    'Case-pattern analysis of which override invocations target claims by groups with limited access to ordinary political processes (incarcerated persons, non-citizens, unpopular religious or ideological minorities) versus claims by groups that could plausibly become future electoral majorities.',
    'If overrides are concentrated against discrete and insular minorities specifically, the victim declaration should be narrowed and the extraction concentration is higher than a diffuse ''any minority'' framing suggests, sharpening rather than softening the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_definition_boundary, empirical, 'Precision of the victim class: diffuse electoral minorities versus discrete and insular minorities specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__legislative_sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(cons_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(cons_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(cons_be_t32, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(cons_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(cons_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(cons_su_t32, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'constitutional supremacy' / 'who has final say on constitutional meaning' per the ε-invariance principle. The judicial_supremacy_reading authors courts as final authority with low extraction toward the polity generally but potential extraction toward majoritarian preference when courts entrench their own institutional power; the popular_sovereignty_reading locates final authority in constituent power itself, with high accessibility_collapse for both courts and legislature relative to direct democratic action. Each sibling carries its own ε, beneficiary/victim structure, and claimed_type — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
