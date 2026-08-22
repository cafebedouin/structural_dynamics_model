% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric (Article 4) Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   The Third Geneva Convention conditions POW status on Article 4 criteria:
 *   a fixed distinctive sign recognizable at a distance, arms carried openly,
 *   a responsible command structure, and conduct in accordance with the laws
 *   of war. This reading treats those criteria as dispositive and
 *   exclusionary — fighters who cannot or do not meet them, typically
 *   irregular and asymmetric combatants, receive no guaranteed POW protection
 *   if captured and remain targetable without violating combatant-immunity
 *   norms. Conventional state militaries, whose organizational form was
 *   already built around these markers, benefit doubly: their own captured
 *   personnel are protected, and their adversaries in asymmetric conflicts
 *   are structurally disadvantaged by a test that tracks the organizational
 *   form of a conventional army rather than any measure of moral culpability
 *   or actual battlefield discipline.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: primary beneficiary (institutional/arbitrage) — receive guaranteed reciprocal POW protection and legal latitude to deny it to adversaries
 *   - state_defense_ministries: agenda_setter (institutional/arbitrage) — draft and administer status-determination procedures implementing Article 4
 *   - irregular_militia_fighters: primary target (powerless/trapped) — structurally unable to satisfy criteria built around conventional army organization
 *   - captured_unprivileged_belligerents: primary target (powerless/trapped) — lose presumption of POW status once classified as failing Article 4
 *   - non_state_armed_group_members: secondary target (moderate/constrained) — organized but visibly-marked criteria exclude asymmetric warfare methods
 *   - international_criminal_tribunals: analytical observer (institutional/analytical) — generally defer to state-centric framework in interstate contexts
 *   - icrc_legal_commentary_body: excluded voice (organized/analytical) — favors broader protective interpretation but has only persuasive, not binding, authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.42).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric (Article 4) Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '98834d05-c66d-4a47-8b74-93897f9a2c68').
narrative_ontology:cs_kernel_codification('98834d05-c66d-4a47-8b74-93897f9a2c68', fixed_text).
narrative_ontology:cs_authority_grounding('98834d05-c66d-4a47-8b74-93897f9a2c68', lineage).
narrative_ontology:cs_interpretation_layer_present('98834d05-c66d-4a47-8b74-93897f9a2c68').
narrative_ontology:cs_reading_relation('98834d05-c66d-4a47-8b74-93897f9a2c68', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('98834d05-c66d-4a47-8b74-93897f9a2c68', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('98834d05-c66d-4a47-8b74-93897f9a2c68', foundational, combatant_status_gates_protection).
narrative_ontology:cs_axiom_status(combatant_status_gates_protection, holdable).
narrative_ontology:cs_axiom_grounding('98834d05-c66d-4a47-8b74-93897f9a2c68', combatant_status_gates_protection, conventional).
narrative_ontology:cs_axiom('98834d05-c66d-4a47-8b74-93897f9a2c68', secondary, visible_distinction_is_legitimate_precondition).
narrative_ontology:cs_axiom_status(visible_distinction_is_legitimate_precondition, holdable).
narrative_ontology:cs_axiom_grounding('98834d05-c66d-4a47-8b74-93897f9a2c68', visible_distinction_is_legitimate_precondition, instrumental).
narrative_ontology:cs_reference_frame('98834d05-c66d-4a47-8b74-93897f9a2c68', id_1949_diplomatic_conference_state_consensus).
narrative_ontology:cs_drift_state('98834d05-c66d-4a47-8b74-93897f9a2c68', post_cold_war_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98834d05-c66d-4a47-8b74-93897f9a2c68', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_defense_ministries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, professional_officer_corps).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, irregular_militia_fighters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, captured_unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wear fixed distinctive insignia, carry arms openly, and operate under a chain of responsible command satisfying Article 4 of the Third Geneva Convention. This reading guarantees their captured personnel POW status and combatant immunity for lawful acts of war, and it lets them treat opposing irregular fighters who lack the same markers as unprivileged belligerents subject to ordinary criminal prosecution or targeting outside the POW framework. They wrote and continue to interpret the criteria through military manuals and status tribunals.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter).

% Draft rules of engagement and status-determination procedures that operationalize the Article 4 criteria. They benefit from a bright-line test that is easy to apply on the battlefield and that legally justifies denying combatant immunity to adversaries who cannot or will not organize into uniformed, hierarchical units — often because they are fighting from structural disadvantage against the state itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_defense_ministries, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Fight without uniforms, fixed insignia, or a command structure recognizable to a conventional army, often because clandestine organization is a survival necessity against a technologically superior state force. Under this reading, if captured they fall outside Article 4 and can be prosecuted as ordinary criminals or worse, with no guaranteed POW protections. They cannot simply adopt uniforms without exposing themselves to immediate destruction, so the criteria are not a neutral choice available to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, irregular_militia_fighters, payer,
    powerless, immediate, trapped, national).

% Once captured and classified as failing Article 4 criteria, they lose the presumption of POW status and the associated protections against prosecution for mere participation in hostilities. Their legal fate depends entirely on the capturing state's status-determination process, which they cannot contest from a position of any structural power.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, captured_unprivileged_belligerents, payer,
    powerless, biographical, trapped, national).

% Include organized insurgencies and liberation movements that may have internal discipline and a command structure but decline or cannot maintain the visible markers (fixed insignia, open carriage of arms in all contexts) that satisfy the state-centric reading of Article 4. They argue this criterion structurally excludes precisely the asymmetric warfare methods available to weaker parties, converting a status test into a permanent disadvantage.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members, payer,
    moderate, generational, constrained, regional).

% Adjudicate individual criminal responsibility and occasionally rule on status questions but generally defer to the state-centric framework in interstate contexts, applying stricter human-rights floors mainly through separate doctrinal routes rather than displacing the state-centric reading directly.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% Publishes authoritative commentary favoring broader protective interpretation (Common Article 3 floors, customary IHL extending some protections regardless of status) but has no binding authority over how individual states apply Article 4 in their own status-determination tribunals; its readings are persuasive, not dispositive, within this reading's operative framework.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, icrc_legal_commentary_body, excluded,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives belligerent states a workable, administrable test for distinguishing lawful combatants entitled to POW treatment from unlawful ones, allowing capturing states to process large numbers of detainees without individualized adjudication of every capture's legal status.
% TRANSFER_FUNCTION: Moves the burden of legal uncertainty and physical risk from state militaries (who receive guaranteed reciprocal protection for their own captured personnel) onto irregular and asymmetric fighters (who receive no guaranteed protection when captured), while state actors also retain latitude to target unprivileged belligerents without violating combatant-immunity norms.
% ABSENT_VOICES: Irregular fighters and the populations that produce them were not parties to the 1949 drafting process in any meaningful negotiating capacity; the criteria were substantially shaped by conventional-army drafting states. Non-state armed groups today have no forum to renegotiate the criteria and can only contest classification after capture, inside the capturing state's own tribunal.
% DISAPPEARANCE_RATIONALE: State militaries would say the world rearranges catastrophically — losing the bright-line test would dissolve the reciprocal incentive structure protecting their own captured soldiers and blur the distinction principle central to IHL's coordination function. Advocates for the universal-rights and hybrid-proportionality readings would say the underlying protective floor (Common Article 3, customary law, human rights law) persists regardless, so the narrower state-centric gloss disappearing would only remove a targeting/status shortcut, not the humanitarian coordination itself.
% FOUNDING_PROBLEM: In 1949, drafting states sought a way to extend reciprocal protections to captured soldiers while denying the same protections to francs-tireurs, partisans, and irregular fighters whom conventional armies regarded as illegitimate combatants blurring the line between soldier and civilian, thereby endangering both categories.
% FOUNDING_PROBLEM_CORROBORATION: State military legal advisors and NATO-aligned defense ministries attest the founding problem (need for a clear distinction test protecting reciprocal POW treatment) remains live in ongoing state-vs-state conflict. Outside the benefiting parties, the ICRC's own commentary, UN human rights bodies, and scholars of asymmetric warfare attest the problem has shifted: most contemporary armed conflicts are non-international or asymmetric, where the Article 4 criteria structurally cannot be met by the weaker party regardless of discipline or intent, converting what was framed as a neutral distinction test into an outcome-determinative advantage for state actors.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).
:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42 because the standing state-centric arrangement, on its own terms, presents itself as a genuine coordination mechanism (reciprocal protection for conventional forces) rather than pure extraction — the coordination function is real and documented. But it is not zero: the same mechanism systematically denies protection to a predictable class of fighters whose organizational form is shaped by structural weakness rather than by any lesser commitment to distinction, and that asymmetry is a real transfer, not a side effect. Suppression (0.55) reflects that the exclusionary reading is actively enforced through military status tribunals and prosecutorial discretion, not merely a passive interpretive default. Theater ratio (0.28) is moderate-low: status tribunals perform genuine adjudicative function much of the time, though an increasing share of that function (rising to 0.28 by interval end) has drifted toward post-hoc legal justification for targeting decisions already made on operational grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of conventional_state_militaries, the state-centric reading looks like principled coordination: a clear, administrable, reciprocity-respecting test that any disciplined military force can meet. From the seat of irregular_militia_fighters and captured_unprivileged_belligerents, the identical rule looks like a status test rigged against the organizational form asymmetric warfare requires — meeting the criteria would mean tactical suicide, so the 'choice' to forgo POW-qualifying markers is not a real choice at all. The engine's per-seat computation should reflect this divergence structurally, from the beneficiary/victim and exit-option declarations, not from any narrative framing choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and their defense ministries are the structural beneficiaries and rule-administrators — the derivation places them near the beneficiary end (low d) because the constraint subsidizes their captured personnel's legal status while imposing no comparable cost on their own operations. Irregular militia fighters and captured unprivileged belligerents are the structural targets: trapped exit options (they cannot simply adopt uniforms without immediate tactical destruction) push their derived d toward the full-target end, amplifying effective extraction. Non-state armed group members with moderate organizational power sit closer to the middle but still on the target side, since even internal discipline does not satisfy the visible-marker requirement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing legitimate soldiers entitled to reciprocal protection from illegitimate irregulars endangering the civilian population by blurring combatant status — was live in 1949 when most anticipated conflicts were interstate wars between conventionally organized armies. The founding_problem_status is authored as contested rather than dead because state militaries still face genuine irregular threats where the distinction concern remains real; but the corroboration from outside the beneficiary set (ICRC commentary, human rights bodies, scholars of asymmetric conflict) documents that the criterion has become outcome-determinative in the now-dominant conflict type (non-international, asymmetric) rather than serving its original distinguishing function evenly. This divergence between claimed coordination function and observed asymmetric operation is exactly what the tangled_rope classification is built to capture without either erasing the genuine reciprocal-protection coordination or excusing the extraction as incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_selection,
    'Is the state-centric Article 4 gate the correct operative reading of Geneva protective scope, or is it one of at least two other defensible readings (universal_rights_reading, hybrid_proportionality_reading) that would place the ε and victim set very differently?',
    'Comparative doctrinal analysis of state practice, ICRC customary IHL study findings, and ICTY/ICTR/ICC jurisprudence on combatant status; document whether tribunals actually apply the strict Article 4 gate or import Common Article 3 / customary floors in practice.',
    'If tribunal practice increasingly imports universal-floor protections regardless of Article 4 compliance, this reading''s exclusionary force is narrower in practice than authored here, and the sibling universal_rights_reading better describes contemporary operative law. If state practice continues to apply the strict gate in interstate contexts, this reading remains the operative one for that context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Which kernel reading (state-centric, universal-rights, hybrid-proportionality) is operatively dominant, and where the disagreement is located structurally.').

omega_variable(
    structural_impossibility_vs_choice,
    'Is the inability of irregular fighters to satisfy Article 4''s visible-marker requirements a genuine structural impossibility given asymmetric warfare conditions, or a choice reflecting insufficient commitment to distinction that the criteria are legitimately designed to penalize?',
    'Case studies of irregular forces that attempted to adopt visible markers under asymmetric conditions, assessing survival/operational outcomes versus forces that did not, controlling for conflict intensity.',
    'If adopting visible markers is genuinely tactically infeasible (not merely undesired) for weaker parties, the exclusion is closer to structural extraction disguised as neutral rule-following. If visible markers are feasible but strategically declined, the coordination framing is stronger and the extraction reading is correspondingly weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_impossibility_vs_choice, empirical, 'Whether Article 4 non-compliance by irregular forces reflects structural impossibility or strategic choice.').

omega_variable(
    fsm_natural_law_framing_check,
    'Is the Article 4 combatant-status distinction better understood as a neutral, near-natural feature of any workable laws-of-war regime (any war needs SOME distinction test), or as a constructed rule whose specific content was shaped to favor the organizational form of the states that drafted it?',
    'Comparative analysis of alternative distinction criteria proposed during and after the 1949 negotiations (e.g., functional participation tests, conduct-based rather than form-based criteria) and why they were not adopted.',
    'If a workable alternative distinction test existed that did not track conventional-army organizational form, the state-centric criteria''s specific content — not just the general need for a distinction rule — reflects drafter interest, strengthening the tangled_rope reading over a mountain/natural-necessity framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsm_natural_law_framing_check, conceptual, 'Whether the specific form of Article 4''s criteria (versus the general need for a distinction rule) is constructed to favor conventional militaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(gene_tr_t38, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 38, 0.22).
narrative_ontology:measurement(gene_tr_t51, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 51, 0.25).
narrative_ontology:measurement(gene_tr_t64, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 64, 0.27).
narrative_ontology:measurement(gene_tr_t76, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 76, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(gene_be_t38, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 38, 0.38).
narrative_ontology:measurement(gene_be_t51, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 51, 0.4).
narrative_ontology:measurement(gene_be_t64, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 64, 0.41).
narrative_ontology:measurement(gene_be_t76, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 76, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement(gene_su_t38, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 38, 0.49).
narrative_ontology:measurement(gene_su_t51, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 51, 0.52).
narrative_ontology:measurement(gene_su_t64, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 64, 0.54).
narrative_ontology:measurement(gene_su_t76, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 76, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the geneva_conventions_protective_scope constraint family, each authoring a distinct ε and victim set for the same kernel text (Article 4 / Common Article 3 / AP I-II). state_centric_reading (this file) narrows the victim set to non-Article-4-compliant fighters and authors moderate ε (0.42) reflecting a genuine but asymmetrically-operating coordination function. universal_rights_reading extends protection universally and would author a much lower ε on the same standing arrangement (denying the exclusionary gate has the force this reading claims). hybrid_proportionality_reading scales protection by conflict-type classification and would author an intermediate ε with a differently-drawn victim set (defined by IAC/NIAC classification rather than combatant markers). The upstream-downstream relationship: this reading's operative dominance in interstate state practice creates structural pressure (influences) on how hybrid_proportionality_reading's conflict-classification step gets applied, since classification disputes often turn on whether a party can claim Article 4 compliance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
