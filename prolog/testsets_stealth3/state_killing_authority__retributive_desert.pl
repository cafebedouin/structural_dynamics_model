% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Desert Authority for Capital Punishment (Lex Talionis Reading)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   In retention jurisdictions, capital punishment for murder operates under
 *   a proportionality justification: the convicted murderer is held to have
 *   forfeited the right to life, and a death for a death discharges the
 *   desert the killing created. This story authors that arrangement as the
 *   retributive_desert reading sees it. The referent of every measure is the
 *   standing arrangement itself — statutes, capital trials, death rows,
 *   appellate review, executions — never any rights-respecting alternative a
 *   reading would prefer. Under this reading's own lights, executing the
 *   deserving discharges rather than extracts, so measured extraction
 *   concentrates in what the arrangement cannot square with its own axiom:
 *   wrongful convictions, discriminatory charging, and drift of eligible
 *   crimes beyond murder. Family note: the colloquial label 'capital
 *   punishment' is a contested kernel (state_killing_authority) with three
 *   readings, each a separate constraint story with its own epsilon over the
 *   same standing arrangement — categorical_abolition authors epsilon near
 *   0.9 (every execution a wrongful taking), deterrence_instrument authors
 *   epsilon indexed to outcome shortfall, and this reading authors epsilon
 *   near 0.31 (the error and disparity residual only). The sibling files link
 *   through network edges; this file averages over none of them. KEY AGENTS
 *   (by structural relationship): - condemned_murderers: primary target
 *   (powerless/trapped) — bears the ultimate sanction -
 *   wrongfully_condemned_prisoners: collateral target (powerless/trapped) —
 *   bears the sanction without the desert that justifies it -
 *   murdered_victims_posthumously_vindicated: symbolic vindication seat
 *   (non-agent; agent=false) - murder_victims_surviving_kin: vindication
 *   recipient (moderate/constrained) - retributive_satisfaction_constituency:
 *   mass beneficiary (organized/mobile) - capital_case_prosecutors:
 *   administrator with career stake (institutional/mobile) -
 *   corrections_execution_staff: endpoint administrators bearing moral cost
 *   (organized/constrained) - appellate_review_judges: interpretive
 *   administrators (institutional/constrained) -
 *   retaining_jurisdiction_legislatures: statutory agenda setters
 *   (institutional/constrained) - abolition_movement_advocates: excluded
 *   objectors (organized/mobile) - jurisprudence_scholars: analytical
 *   observers (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.31).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.66).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.31).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Desert Authority for Capital Punishment (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '41a027e2-2064-4535-8079-beb9cf504f9a').
narrative_ontology:cs_kernel_codification('41a027e2-2064-4535-8079-beb9cf504f9a', formalized).
narrative_ontology:cs_authority_grounding('41a027e2-2064-4535-8079-beb9cf504f9a', lineage).
narrative_ontology:cs_interpretation_layer_present('41a027e2-2064-4535-8079-beb9cf504f9a').
narrative_ontology:cs_reading_relation('41a027e2-2064-4535-8079-beb9cf504f9a', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('41a027e2-2064-4535-8079-beb9cf504f9a', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('41a027e2-2064-4535-8079-beb9cf504f9a', foundational, murder_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murder_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('41a027e2-2064-4535-8079-beb9cf504f9a', murder_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('41a027e2-2064-4535-8079-beb9cf504f9a', foundational, lex_talionis_proportionality_binding).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality_binding, holdable).
narrative_ontology:cs_axiom_grounding('41a027e2-2064-4535-8079-beb9cf504f9a', lex_talionis_proportionality_binding, deontological).
narrative_ontology:cs_reference_frame('41a027e2-2064-4535-8079-beb9cf504f9a', talionic_proportional_desert_framework).
narrative_ontology:cs_drift_state('41a027e2-2064-4535-8079-beb9cf504f9a', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41a027e2-2064-4535-8079-beb9cf504f9a', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_surviving_kin).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_satisfaction_constituency).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, capital_case_prosecutors).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderers).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongfully_condemned_prisoners).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, forfeiture_of_right_to_life_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, posthumous_vindication_of_murder_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of capital murder in a retaining jurisdiction and sentenced under the proportionality statute. Bears the ultimate sanction the arrangement imposes: loss of life, preceded by years of death-row confinement. Available levers are appeal, retrial, and clemency petition; each succeeds rarely and slowly, and there is no leaving the sentence's reach short of executive mercy or judicial reversal.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderers, payer,
    powerless, biographical, trapped, national).

% Sentenced to death for murders later shown — sometimes only after the execution — to have been committed by others. The forfeiture rationale presumes a guilt they do not have, so the sanction falls on them without the desert the statute requires. Exit runs through new evidence surfacing, often decades into confinement; for those executed before exoneration there is no exit at all.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongfully_condemned_prisoners, payer,
    powerless, biographical, trapped, national).

% The killed persons in whose name the arrangement acts. The doctrine assigns them a vindication interest that the execution is said to satisfy, but they hold no position, cast no vote, and can receive nothing themselves; whatever vindication occurs accrues to the living — kin, constituency, institutions — and to the doctrine that speaks for them. Listed for completeness of the vindication structure; contributes no directional pull.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_victims_posthumously_vindicated, beneficiary,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murdered_victims_posthumously_vindicated).

% Family members of homicide victims for whom the killer's execution is presented as official vindication. Some report the completed sentence as closing a chapter; others report that the years of appeals and the execution itself deepened their loss. They may advocate for or against a particular execution, but the arrangement's promise of vindication is addressed to them whether or not they sought it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_surviving_kin, beneficiary,
    moderate, biographical, constrained, national).

% Citizens and voters who hold that murder must be answered with death and experience each lawful execution as the moral order being kept. Their support sustains the statutes. Their costs are indirect: tax burden, the small probability that the machinery errs against someone in their own circle, and complicity objections they may or may not feel. They can revise their position at any election.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_satisfaction_constituency, beneficiary,
    organized, generational, mobile, national).

% District attorneys and their deputies decide whether to charge capital murder and seek death, run the trials, and defend the verdicts on appeal. A capital-case record is a durable career asset in elective office. They may decline to seek death in any individual case, but their offices' standing rests partly on demonstrated willingness to seek it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, capital_case_prosecutors, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, capital_case_prosecutors, beneficiary).

% Wardens, chaplains, medical personnel, and execution teams who carry out whatever the courts and governor finally order. They administer the arrangement's endpoint and absorb its psychological weight; participation-linked distress and staffing churn are documented in retention facilities. Refusal means reassignment or resignation, not any change to the schedule.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, corrections_execution_staff, agenda_setter,
    organized, biographical, constrained, regional).

% State supreme court justices and federal circuit judges who review capital sentences for proportionality and constitutional compliance, defining what counts as a reliable, proportionately administered death sentence. They cannot initiate executions or abolish them; they can slow, condition, or vacate individual ones. Their interpretive settlements stand until a higher court or legislature displaces them.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, appellate_review_judges, agenda_setter,
    institutional, generational, constrained, national).

% State legislatures that enact, narrow, expand, or repeal the capital statutes, define eligible crimes and methods, and fund the prosecution and defense infrastructure. Repeal is available at any session but carries electoral risk with the retention majority; expansion draws counter-pressure from courts and budgets.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retaining_jurisdiction_legislatures, agenda_setter,
    institutional, generational, constrained, regional).

% Organized opponents of capital punishment — advocacy organizations, religious bodies, defense counsel, international human-rights bodies — who deny that any desert justifies state killing and would put abolition itself on the table. Proportionality hearings and legislative debates are structured around who deserves death, which treats their core objection as out of bounds; they operate on outcomes from outside the formal deliberation.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolition_movement_advocates, excluded,
    organized, generational, mobile, global).

% Political philosophers and constitutional scholars who map the forfeiture, proportionality, and legitimacy questions across every side of the dispute. They publish analyses of each reading, testify occasionally, and hold no enforcement or electoral leverage over the arrangement.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, murder_victims_surviving_kin).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the social demand for response to homicide into a single authorized, due-process-bound procedure, displacing private vengeance and feud cycles; the talionic formula caps severity at the level of the offense, so the same structure that authorizes death for murder forbids escalating beyond it, and everyone subject to the law knows what murder costs under this reading.
% TRANSFER_FUNCTION: Moves life itself — the ultimate sanction — from convicted murderers to the state's account of discharged justice, and moves derived goods outward: vindication to victims' kin, moral-order assurance to the retention constituency, and career and electoral capital to the prosecutors and officeholders who administer capital cases. When the machinery convicts the wrong person, it moves a life that nothing owed.
% ABSENT_VOICES: Condemned prisoners' first-person accounts are structurally discounted once desert is presumed; families of the wrongfully convicted spend years unheard while the presumption of guilt organizes the proceedings; categorical-abolition ethicists sit outside the proportionality frame entirely; and the murdered dead, in whose name the arrangement acts, cannot say whether death answers their interest.
% DISAPPEARANCE_RATIONALE: Capital statutes, death rows, execution protocols, and the litigation economy around them would dissolve within a few legislative cycles; homicide sentencing would reorganize around maximum-security life terms; kin-vindication expectations and tough-on-crime electoral positioning would re-form around the replacement sanction. Homicide itself would not change — only the state's answer to it.
% FOUNDING_PROBLEM: Private vengeance and blood feud: before centralized criminal justice, a killing obligated the victim's kin to retaliate, with escalation limited only by negotiated settlement. The talionic formula — a life for a life, and no more — was built to cap retaliation and concentrate it in a single authorized hand.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists of the feud-to-state transition, writing outside the benefiting parties, corroborate the feud-bounding genealogy. Contemporary criminologists attest that feud suppression is practically solved by the modern state's violence monopoly, while retention jurists and kin advocates attest a continuing need for bounded proportional response. Both flanks cite sources external to the arrangement's beneficiaries; the liveness of the founding problem is corroborated on both sides rather than settled.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).
:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope. The arrangement retains a real coordination function — it channels the demand for vengeance into a single due-process bound by a severity cap (the talionic limit answers death only for death, protecting every defendant against disproportionate punishment) — while simultaneously concentrating an asymmetric, irreversible cost on the condemned class and needing continuous enforcement machinery to hold. Metrics are authored independently of the claim. Extractiveness 0.31 is reading-indexed (see narrative_context): executions of the deserving count as justice discharged, so the residue is wrongful conviction plus application disparity. Suppression 0.66 is a raw structural property, unscaled by power or scope — it reflects the condemned's absolute lack of exit and the political containment of abolition, not any directional arithmetic. Theater_ratio 0.42 tracks the growing ceremonial share — clemency processes that almost never commute, proportionality reviews that ratify anticipated outcomes, execution protocol — against a shrinking functional base as annual executions decline. Accessibility_collapse 0.58: within the reading's own logic the alternative (permitting murderers to live) collapses doctrinally, yet empirically the life-imprisonment alternative thrives across abolishing jurisdictions and increasingly inside retaining ones, so alternatives are narrowed rather than eliminated. Resistance 0.72: abolition movements, moratoria campaigns, international pressure, and defense-bar litigation meet the machinery continuously. The three measurement series share one grid (points 0, 8, 16, 24, 32, 40) so every metric is authored at every examined time. Suppression_requirement is tracked because enforcement capacity moved twice in the interval: a post-resumption ratchet through expanded death rows and streamlined appeals, then decay through execution-drug scarcity, moratoria, and falling death sentences — a rise-then-fall arc, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. The condemned seat (powerless/trapped) experiences the arrangement as absolute — the full sanction lands with no exit — so its computed classification sits at the extraction extreme. The kin and constituency seats receive vindication and moral order at negligible personal cost: a rope-like surface. The prosecutor seat administers the machinery and converts it into career standing: an administrator's surface with a beneficiary undertow. Appellate and legislative seats experience it as a revisable legal object. The wrongfully condemned share every structural atom with the condemned seat (powerless, trapped) yet bear strictly greater wrongful cost — the derivation cannot separate them, which flattens the injustice gradient inside the payer class; that limitation is flagged here rather than papered over. The engine computes per-seat types from the authored structure; nothing in the claimed_type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to living recipients: kin receive vindication, the constituency receives order-assurance, prosecutors receive career capital — each sits near the subsidized end, prosecutors lowest since they also help set the agenda. Victim declarations map to the condemned and wrongfully condemned at the full-target end. The murdered victims' vindication seat is authored with agent=false: the agent-hood gate keeps the symbolic posthumous seat out of directionality arithmetic because a dead person collects nothing — the vindication value actually flows to the living kin seat and to the doctrines recorded under vindicated_propositions. No directionality overrides are used: the derivation from roles, power, and exit reproduces the qualitative structure, and the one place same-atom agents need different d (condemned versus wrongfully condemned) cannot be separated by a power-atom-keyed override — the limitation is recorded rather than forcing an override that would distort both seats. Receipt: the arrangement's gains demonstrably land on the kin seat, since official vindication is addressed to them, with career-capital skimming by prosecutors as secondary capture — so gain_flow names the kin seat; asserting 'diffuse' would be false because named seats demonstrably accrue.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding private vengeance — is plausibly dead in consolidated states, where the violence monopoly settles homicide response regardless. Authoring its status as contested rather than dead is deliberate: a dead-status plus world_rearranges mismatch would flag the arrangement as a zombie kept alive by inertia and ceremony, and the rising theater_ratio series (0.28 to 0.42) alongside the falling enforcement requirement (peak 0.76 to 0.66) is exactly the early signature such a transition would print. What blocks a piton reading today is that maintenance is still contested and costly rather than merely ritual — executions still occur, coalitions still fight over them, and no seat profits enough to sustain it purely for show while no seat is hurt cheaply enough to repair it. The mandatrophy discipline keeps the two standing mislabelings apart: reading the arrangement as pure extraction erases the real feud-channeling and severity-cap coordination the talionic frame performs; reading it as pure coordination launders the irreversible, asymmetric cost borne by the condemned class, including members who never earned it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates only the retributive_desert reading of the state_killing_authority kernel — what would each sibling reading change structurally if it displaced this one?',
    'Cross-file comparison of the sibling stories (state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition): victim-set membership, beneficiary structure, and authority basis per reading.',
    'Under categorical_abolition the payer set empties (no sanctioned killing exists to bear costs) and the kin vindication seat loses its object; under deterrence_instrument the authority basis shifts from the proportionality norm to measured outcome, and the condemned''s desert stops doing justificatory work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one kernel, three readings, per-reading structural deltas.').

omega_variable(
    rights_holder_set_boundary,
    'Where the readings actually disagree: does forfeiture remove the condemned from the rights-holder set entirely, partially (life only, all else retained), or not at all?',
    'Doctrinal analysis of forfeiture scope in retaining jurisdictions'' case law, compared against the boundary placements the sibling readings assert.',
    'Total removal licenses any treatment of the condemned; life-only removal confines the arrangement to the execution itself while preserving due-process and confinement rights; no removal collapses this reading into categorical_abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_holder_set_boundary, conceptual, 'Location of the kernel disagreement: the boundary of the rights-holder set.').

omega_variable(
    wrongful_conviction_share,
    'What share of condemned prisoners lack the desert the statute requires, and how does that share trend?',
    'Systematic post-sentence review (DNA and full-file audits) of capital-case error rates, plus tracking of posthumous exonerations.',
    'Reading-indexed extractiveness scales with the innocent share; past roughly five percent, punishing the undeserving contradicts the very proportionality axiom this reading grounds its authority in, pressuring the reading from inside its own commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_share, empirical, 'Error rate inside the payer class as the reading''s binding internal constraint.').

omega_variable(
    kin_vindication_substance,
    'Does carrying a capital case through execution actually deliver the vindication promised to surviving kin, or does the extended process deepen their harm?',
    'Longitudinal studies of survivors'' families comparing capital and non-capital case trajectories; existing findings are mixed.',
    'If execution harms kin on balance, the primary beneficiary seat inverts toward a second paying seat, the coordination story thins, and the arrangement drifts toward pure extraction wearing a vindication cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kin_vindication_substance, empirical, 'Whether the principal declared beneficiary seat nets positive.').

omega_variable(
    partial_forfeiture_coherence,
    'Can the doctrine coherently hold that the condemned forfeits the right to life while retaining every other right — due process, humane confinement, appeal — until execution?',
    'Doctrinal and philosophical analysis, plus observation of whether forfeiture logic expands in practice toward degraded confinement and curtailed appeal rights.',
    'A stable partial forfeiture anchors this reading; collapse toward total forfeiture licenses abuses that repel the retention coalition, while collapse toward no forfeiture hands the argument to categorical_abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partial_forfeiture_coherence, conceptual, 'Internal coherence of scoped rights forfeiture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__retributive_desert, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__retributive_desert, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__retributive_desert, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__retributive_desert, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__retributive_desert, base_extractiveness, 8, 0.26).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__retributive_desert, base_extractiveness, 16, 0.31).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__retributive_desert, base_extractiveness, 24, 0.36).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__retributive_desert, base_extractiveness, 32, 0.33).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__retributive_desert, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__retributive_desert, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__retributive_desert, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__retributive_desert, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The colloquial label 'capital punishment debate' conflates three structurally distinct constraints instantiating one kernel (state_killing_authority): this retributive_desert reading (authority from proportionality and desert), deterrence_instrument (authority from outcome), and categorical_abolition (authority from inalienability). Each carries its own epsilon over the same standing arrangement — 0.31 here versus materially higher under abolition's lights and outcome-contingent under deterrence's — so no single file can represent the label without violating epsilon-invariance. Relation structure: this reading logically forecloses categorical_abolition's core premise within any single framework (a forfeitable right to life and an inalienable one cannot both hold), while coexisting with deterrence_instrument (many actors combine desert-permissibility with deterrence-advisability); this reading also sustains the statutory substrate the deterrence reading evaluates, a downstream-pressure effect documented here rather than as a second edge type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
