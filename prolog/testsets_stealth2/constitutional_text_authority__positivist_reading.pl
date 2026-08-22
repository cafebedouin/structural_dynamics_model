% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist Reading of Constitutional Text Authority (Pedigree-Based Validity)
 *   domain: legal/jurisprudential/political
 *
 * SUMMARY:
 *   This story models the positivist reading of constitutional text authority
 *   as a standing arrangement: constitutional validity is conferred by formal
 *   enactment procedures and institutional sources, and moral content is
 *   irrelevant to whether a norm counts as law. The arrangement solves a real
 *   problem — identifying law amid moral pluralism — while routing the cost
 *   of change through gates its beneficiaries staff, which is why the story
 *   carries both beneficiary and victim declarations. The ε referent is the
 *   pedigree-validity regime itself, assessed by the reading's own lights:
 *   what the positivist concedes as the price of certainty, not the moralized
 *   alternative it rejects. Claim and metrics are authored independently: the
 *   claimed type states what I believe is structurally true (genuine
 *   coordination function plus asymmetric extraction under active
 *   enforcement), and the metrics state what I believe is descriptively true
 *   of the regime's operation; the engine computes per-seat classifications
 *   from the structural data, and any divergence between claim and computed
 *   output is the datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - - formal_amendment_bodies: Agenda-setting beneficiary (institutional/arbitrage) — authors and administers the validity-conferring procedures
 *   - - incumbent_officeholders: Primary beneficiary (powerful/arbitrage) — collects protection from moral-validity challenge without running the machinery
 *   - - judicial_officers: Enforcing agenda-setter (institutional/constrained) — administers the pedigree test; near-symmetric position between gain and burden
 *   - - moral_reform_litigants: Payer (moderate/constrained) — routed from cheap courtroom invalidation to expensive supermajority channels
 *   - - disenfranchised_minorities_under_enacted_text: Primary target (powerless/trapped) — bears the regime's heaviest costs under rules their consent never shaped
 *   - - natural_law_jurists: Excluded voice (organized/analytical) — validity criterion dismissed as category error rather than answered
 *   - - constitutional_scholars: Analytical observer — maps announced doctrine against practiced reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.5).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.62).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist Reading of Constitutional Text Authority (Pedigree-Based Validity)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/jurisprudential/political").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '40d2def9-ff82-4e32-9804-5af3b5b5e202').
narrative_ontology:cs_kernel_codification('40d2def9-ff82-4e32-9804-5af3b5b5e202', fixed_text).
narrative_ontology:cs_authority_grounding('40d2def9-ff82-4e32-9804-5af3b5b5e202', practice).
narrative_ontology:cs_interpretation_layer_present('40d2def9-ff82-4e32-9804-5af3b5b5e202').
narrative_ontology:cs_reading_relation('40d2def9-ff82-4e32-9804-5af3b5b5e202', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40d2def9-ff82-4e32-9804-5af3b5b5e202', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('40d2def9-ff82-4e32-9804-5af3b5b5e202', foundational, separability_of_law_and_morality).
narrative_ontology:cs_axiom_status(separability_of_law_and_morality, holdable).
narrative_ontology:cs_axiom_grounding('40d2def9-ff82-4e32-9804-5af3b5b5e202', separability_of_law_and_morality, conventional).
narrative_ontology:cs_axiom('40d2def9-ff82-4e32-9804-5af3b5b5e202', foundational, enactment_pedigree_confers_validity).
narrative_ontology:cs_axiom_status(enactment_pedigree_confers_validity, holdable).
narrative_ontology:cs_axiom_grounding('40d2def9-ff82-4e32-9804-5af3b5b5e202', enactment_pedigree_confers_validity, conventional).
narrative_ontology:cs_reference_frame('40d2def9-ff82-4e32-9804-5af3b5b5e202', pedigree_exclusive_validity).
narrative_ontology:cs_drift_state('40d2def9-ff82-4e32-9804-5af3b5b5e202', post_legal_realist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40d2def9-ff82-4e32-9804-5af3b5b5e202', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, formal_amendment_bodies).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, incumbent_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, judicial_officers).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_reform_litigants).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, disenfranchised_minorities_under_enacted_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the procedures that confer validity: ratification conventions, supermajority votes, promulgation formalities. Their own outputs count as valid constitutional law by virtue of passing through those procedures, whatever their content. As authors of the procedures they can, at considerable cost, rewrite the gate itself; leaving the arrangement is available to them in a way it is not to others.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, formal_amendment_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, formal_amendment_bodies, beneficiary).

% Hold office, jurisdiction, and property distributions under the enacted text. Because validity turns on pedigree rather than merit, their positions cannot be displaced by arguing the rules that protect them are unjust — a challenger must win through the same supermajority procedures the incumbents help staff. They collect stability and security of tenure without administering anything.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, incumbent_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).

% Apply the validity test case by case: trace enactments to their institutional source, treat appeals to moral content as outside the legal question. The arrangement gives them a determinate decision procedure and insulation from charges of legislating personal morality; it also hands them the reputational burden of enforcing valid-but-harsh rules. Exiting means leaving the bench and the professional role that constitutes their standing.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judicial_officers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, judicial_officers, beneficiary).

% Bring claims with strong moral force but weak procedural anchor — unenumerated rights, emerging dignitary interests. The validity regime closes the courtroom route ('not a legal argument') and redirects them to amendment and legislation, channels demanding supermajority coordination they rarely command. Their remaining alternative is long-cycle political mobilization at far higher cost than the closed route would have been.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_reform_litigants, payer,
    moderate, biographical, constrained, national).

% Live under validly enacted rules that exclude or subordinate them — the classic configuration in which the procedures that conferred validity never counted their consent. The cheapest escape (persuading a court the rule fails as law because of what it does) is blocked by design; what remains is compliance, emigration, or amendment campaigns measured in generations.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, disenfranchised_minorities_under_enacted_text, payer,
    powerless, generational, trapped, national).

% Maintain that grossly unjust rules fail as law — that validity requires moral content. Inside the pedigree-validity regime their criterion is dismissed as a category error before argument begins rather than answered on the merits. They press the case in scholarship, dissenting opinions, and comparative jurisdictions, but hold no seat in the conversation that decides what counts as valid constitutional law here.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_jurists, excluded,
    organized, civilizational, analytical, global).

% Map the doctrine's operation from outside the enforcement machinery: code which validity criteria courts actually deploy, document the distance between announced pedigree-tracing and practiced moral reasoning, and referee the dispute between pedigree and moral-content theories of validity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, incumbent_officeholders).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared criterion — the rule of recognition — by which officials identify valid constitutional norms without first adjudicating moral disagreement, enabling prediction, planning, and peaceful settlement of validity disputes among citizens and institutions that share no comprehensive morality.
% TRANSFER_FUNCTION: Moves the burden of constitutional change from courts (where a moral-invalidity argument is cheap to raise) to formal amendment and legislation (where supermajority coordination is expensive); correspondingly moves security of position to whoever currently holds power under the enacted text.
% ABSENT_VOICES: Those whose consent the founding and amendment procedures never captured — historically disenfranchised classes — and natural-law jurists whose validity criterion is ruled out of order rather than answered. Both would object that 'validity' is being settled by the very procedures whose authority is the question, and neither is seated where that objection could register.
% DISAPPEARANCE_RATIONALE: If the pedigree-validity regime vanished overnight, every norm's validity would reopen to moral challenge: courts would need a substantive moral theory to decide what counts as law, the line between invalidation and amendment would dissolve, entrenched distributions would become immediately contestable, and the profession's decision procedures would reorganize around whichever moral criterion replaced pedigree.
% FOUNDING_PROBLEM: How can a pluralistic society identify its law — and settle disputes about constitutional validity — when citizens share no comprehensive morality? The positivist program built validity on enactment pedigree precisely so law could be identified without first winning the moral argument.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: modern written constitutions overwhelmingly adopt formal amendment procedures as the validity gate; transnational legal orders trace validity to ratification and promulgation rather than moral merit; and the reading's sharpest critics concede the certainty function while disputing its sufficiency — attestation by opponents is the strongest available corroboration that the founding problem is real. No corroborator claims the problem is solved for every case, and the grudge-informer record shows the solution's recurring cost.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits mid-range (0.50 at interval end) because the regime's costs are real but conditional: anyone who can win the procedural game faces no extraction at all, while those who cannot face the full price of rerouted change. Suppression (0.62) is a raw structural property, unscaled by power or scope: the regime actively excludes moral-validity arguments through standing doctrines, justiciability gates, and professional formation, though political channels remain formally open — hence suppression above the coordination baseline but short of coercive closure. Theater (0.28) reflects a mostly functional core (validity tracing genuinely decides cases) with a growing rhetorical component — 'neutral method' and 'we only apply the text' invocations that increasingly describe aspiration rather than practice. Accessibility collapse (0.58) is partial: once the frame is accepted, moral-validity alternatives collapse completely inside legal practice, but amendment and legislation persist as costly exits. Resistance (0.66) is high and sustained — natural-law revival, the rights-thesis critique, critical movements, and living-constitutionalist judicial coalitions have contested the frame continuously across the interval. The measurement series run on one shared time grid (t=0..60, decade steps) with every tracked metric authored at every point; the trajectories show a slow ratchet — enforcement infrastructure and professional socialization hardened while extraction crept upward as the frame shielded progressively more of the incumbent distribution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From formal_amendment_bodies and incumbent_officeholders, the regime is the thing that makes their holdings secure and their enactments authoritative — a coordination achievement they operate or profit from. From disenfranchised_minorities_under_enacted_text, the same regime is a locked door: the argument that would free them is defined out of existence before it is heard. judicial_officers sit near the hinge — they gain a determinate craft and lose the ability to soften valid-but-harsh outcomes, and their constrained exit keeps them administering what they might privately regret. constitutional_scholars see the whole shape, including the gap between announced pedigree-tracing and practiced moral reasoning. The engine computes these per-seat differences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the three benefiting seats: formal_amendment_bodies derive nearest the beneficiary pole (they author the gate and hold arbitrage-grade exit through it); incumbent_officeholders sit close behind (full subsidy of security, arbitrage exit via the procedures they staff); judicial_officers derive damped toward the middle because their gain is professional rather than distributive and their exit is constrained. Victim declarations drive high directionality for the paying seats: disenfranchised_minorities_under_enacted_text derive nearest the full-target pole (powerless, trapped, national scope amplifying verification difficulty), with moral_reform_litigants slightly less exposed (moderate power, political alternatives partially open). No directionality overrides are authored: the derivation chain captures the seat structure adequately from the declared beneficiaries, victims, and exit options. Residual coarseness is acknowledged — override atoms key on power level, so the two institutional agenda-setters cannot be separated from each other by override without dragging both; the commentary records the difference instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — identifying law amid moral pluralism — remains live, so this is not a mandate outliving its function and mandatrophy_resolved is not declared. The classification discipline cuts both ways here: the genuine, still-needed coordination function (shared validity criterion under deep moral disagreement, corroborated by opponents of the reading) blocks any move to label the arrangement pure extraction, while the documented payer seats and the enforced exclusion of the moral-validity route block any move to label it pure coordination. Holding both facts in one structure is exactly what the hybrid category exists to express, and the temporal series shows the mechanism by which such hybrids degrade: theater rising and suppression ratcheting are early-warning signatures that the coordination story is slowly becoming cover, which is the trajectory to watch rather than a verdict already rendered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the constitutional_text_authority kernel — what would the originalist and living-constitutionalist sibling readings change structurally if instantiated instead?',
    'Compile the sibling stories and compare: victim sets (those outside ratification-era understanding versus those outside contemporary moral consensus), epsilon profiles, and the foreclosure edges the engine computes from axiom contradictions across the reading set.',
    'Sibling instantiation relocates the payer seats rather than removing them — originalism shifts costs toward those whose circumstances ratifiers did not anticipate; living constitutionalism shifts costs toward holders of entrenched text-based entitlements — and changes which axioms foreclose which, altering the family''s contamination topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates the positivist_reading of the constitutional_text_authority kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    exclusive_vs_inclusive_positivism,
    'Does this reading permit the rule of recognition to incorporate moral criteria of validity where enacted text invites them, or does it insist validity is exhausted by pedigree?',
    'Doctrinal coding of how the reading treats validity arguments citing moral principles invoked through enacted clauses — due process, dignity guarantees, unwritten structural commitments.',
    'An inclusive variant weakens the foreclosure edge to the living-constitutionalist sibling and lowers measured extraction (moral arguments regain partial admissibility through the recognition rule itself); an exclusive variant preserves full foreclosure and the higher extraction profile authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusive_vs_inclusive_positivism, conceptual, 'Whether the reading is exclusive or soft positivism — the internal fault line that governs both its extraction profile and its relation to siblings.').

omega_variable(
    grudge_informer_cost_attribution,
    'Do the costs borne by minorities under validly enacted unjust rules belong to the validity regime itself, or to the political failure that produced the unjust enactment?',
    'Counterfactual comparison across validity regimes: whether jurisdictions with moral-validity review or stronger unwritten-constitution doctrines deliver faster relief to similarly situated groups, controlling for political conditions.',
    'Attribution to the regime pushes epsilon toward the pure-extraction end and would justify reclassification pressure; attribution to upstream politics locates the harm outside this constraint and drops epsilon toward the coordination end, leaving the regime a transmission channel rather than a source.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grudge_informer_cost_attribution, empirical, 'The classic Hart-Fuller cost-accounting dispute, unresolved by the historical record either way.').

omega_variable(
    covert_moral_reasoning_status,
    'Is courts'' routine use of moral language under textual cover a departure from the pedigree frame, or the interpretive layer doing pedigree-compatible work?',
    'Systematic coding of appellate validity reasoning: whether moral premises appear as independent grounds of invalidity or as interpretations of enacted text and institutional practice.',
    'The departure reading raises theater_ratio, supports drift-toward-performance hypotheses, and dates a type transition earlier; the absorption reading keeps the frame intact, keeps theater low, and treats the interpretive layer as functioning as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_moral_reasoning_status, conceptual, 'Whether the gap between announced method and practiced reasoning is drift or interpretation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of moral-validity arguments structural (procedural gates, standing rules, justiciability doctrine) or internalized (professional formation that makes such arguments unthinkable before the gate is reached)?',
    'Post-exit trajectory: track jurists and scholars who leave the positivist tradition — if moral-validity argumentation resumes immediately upon exit, suppression was structural; if the trained refusal persists, it was internalized.',
    'Internalized suppression means the regime travels with its carriers after formal exit — effective suppression exceeds the structural measure, and reform requires retraining a profession rather than rewriting rules; purely structural suppression yields to doctrinal change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in professional legal formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__positivist_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__positivist_reading, base_extractiveness, 60, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__positivist_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (constitutional_text_authority), three readings emitted as separate stories because the colloquial label 'constitutional authority' conflates structurally distinct claims with different epsilon values — pedigree-based validity (this story), ratification-fixed meaning (originalist sibling), and morally evolving authority (living-constitutionalist sibling). Measuring all three through one observable would violate epsilon-invariance. Edge direction: this reading supplies the validity substrate both siblings presuppose — each sibling's account of where authority comes from operates inside a validity regime this reading defines — so influence edges run from this story to both siblings, while the siblings' meaning-theories feed back into how the interpretive layer exercises the validity test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
