% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Authority
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   A constitutional system where the text is read as establishing parliament
 *   as the ultimate interpreter of constitutional meaning. Courts provide
 *   reasoned advice and develop doctrine, but the legislature retains the
 *   final say through override mechanisms (notwithstanding clauses,
 *   constitutional suspension, or simple repeal). This reading vindicates
 *   majoritarian democracy and parliamentary sovereignty as the authentic
 *   realization of constitutional government. The competing readings—judicial
 *   supremacy and popular sovereignty—challenge this by locating final
 *   authority elsewhere. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination problem solved: constitutional meaning is settled through a
 *   clear, updatable procedure; extraction component: majoritarian power is
 *   unilaterally defined). The metrics reflect the asymmetry: extraction
 *   rises over time as legislatures accumulate override history and
 *   minorities recognize the pattern; suppression is high because the
 *   constraint's persistence depends on preventing courts from holding
 *   interpretive authority or demos from claiming direct constitutional
 *   voice.
 *
 * KEY AGENTS:
 *   - legislative_majority: holds unilateral power to override constitutional interpretation; benefits from majoritarian will-setting; agenda-setter seat
 *   - minority_rights_holders: lack voting power to defend constitutional protections; face cascading vulnerability as majorities override protections; payer/victim seat
 *   - judicial_actors: advise but cannot enforce against legislative override; operate under constrained independence; beneficiary (institutional role) and payer (authority limits) simultaneously
 *   - out_of_power_factions: depend on constitutional limits the majority can override; must wait for electoral shifts or rely on self-restraint; payer/constrained seat
 *   - constituent_public (demos): theoretically the source of authority but excluded from ongoing interpretation; has no direct voice except through costly amendment or extreme measures; excluded seat
 *   - comparative_jurisdictions: observe rights-erosion patterns and institutional stability; provide evidence of empirical consequences; observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.68).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.71).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '5a88b01e-927d-4f56-84a8-d79193cbca72').
narrative_ontology:cs_kernel_codification('5a88b01e-927d-4f56-84a8-d79193cbca72', fixed_text).
narrative_ontology:cs_authority_grounding('5a88b01e-927d-4f56-84a8-d79193cbca72', extraction).
narrative_ontology:cs_interpretation_layer_present('5a88b01e-927d-4f56-84a8-d79193cbca72').
narrative_ontology:cs_reading_relation('5a88b01e-927d-4f56-84a8-d79193cbca72', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a88b01e-927d-4f56-84a8-d79193cbca72', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('5a88b01e-927d-4f56-84a8-d79193cbca72', foundational, electoral_legitimacy_grounds_final_authority).
narrative_ontology:cs_axiom_status(electoral_legitimacy_grounds_final_authority, holdable).
narrative_ontology:cs_axiom_grounding('5a88b01e-927d-4f56-84a8-d79193cbca72', electoral_legitimacy_grounds_final_authority, deontological).
narrative_ontology:cs_axiom('5a88b01e-927d-4f56-84a8-d79193cbca72', foundational, legislative_override_compatible_with_constitutionalism).
narrative_ontology:cs_axiom_status(legislative_override_compatible_with_constitutionalism, holdable).
narrative_ontology:cs_axiom_grounding('5a88b01e-927d-4f56-84a8-d79193cbca72', legislative_override_compatible_with_constitutionalism, conventional).
narrative_ontology:cs_reference_frame('5a88b01e-927d-4f56-84a8-d79193cbca72', parliamentary_supremacy_doctrine).
narrative_ontology:cs_drift_state('5a88b01e-927d-4f56-84a8-d79193cbca72', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a88b01e-927d-4f56-84a8-d79193cbca72', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_will).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_holders).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, out_of_power_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, judicial_actors).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judicial_actors).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, democratic_majoritarianism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the legislative body and holds constitutional meaning-making power through simple override or notwithstanding clauses. Interprets the constitutional text as conferring final say on its own meaning. Uses courts as advisory bodies only. Justifies this reading as grounding authority in electoral legitimacy and the will of the voting majority. Sets the agenda for constitutional interpretation and can override judicial findings through legislative act.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislative_majority, agenda_setter,
    institutional, generational, analytical, national).

% Advise on constitutional meaning through reasoned interpretation and precedent but hold no final authority. Their interpretations can be overridden by legislative action. Benefit from their institutional role and the appearance of judicial independence; pay by operating under the knowledge their rulings may be legislatively reversed. Tend to develop doctrines of constitutional self-restraint (obeisance to the legislative supremacist reading) to avoid direct conflict.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judicial_actors, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, judicial_actors, payer).

% Lack voting power in the legislative majority and depend on constitutional protections that courts can only advise about, not enforce. Under this reading, minority rights are protected only insofar as the majority permits through non-exercise of override power. They face cascading vulnerability: if courts interpret the constitution to protect their rights and the legislature disagrees, the legislature has final say and can override both the interpretation and the protection.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_holders, payer,
    powerless, biographical, trapped, national).

% Electoral losers who depend on constitutional limits to check the current majority's power. Under legislative supremacy, the constitutional text constrains the legislature only to the extent the majority chooses to honor it. They must wait for electoral shifts or rely on legislative self-restraint — exit is conditional on future electoral victory.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, out_of_power_factions, payer,
    moderate, biographical, constrained, national).

% Theoretically the source of constitutional authority but has no direct voice in its ongoing interpretation under this reading. Cannot speak back to the constitutional text except through formal amendment (costly, slow) or revolution (extreme). Excluded from the daily interpretive conversation between courts and legislatures that shapes how the constitution operates.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constituent_public, excluded,
    powerless, generational, trapped, national).

% Jurisdictions that have adopted judicial supremacy or popular sovereignty readings watch this reading operate and analyze its empirical consequences: rate of rights erosion, stability of democratic processes, minority protection outcomes. Provide comparative evidence that influences how the reading is evaluated.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_jurisdictions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, legislative_majority).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a determinate, updatable procedure for resolving constitutional disputes: courts reason about meaning, legislatures have override power, disputes are settled by legislative will rather than judicial finality. Solves the coordination problem of 'who decides what the constitution means and how does that decision get enforced.'
% TRANSFER_FUNCTION: Transfers interpretive power and sovereignty from a distributed/contingent authority (courts, original intent, popular referendum) to the legislative majority. The legislature can redefine the meaning of constitutionally protected rights by override, capturing the benefit of majoritarian will-setting while shifting the cost of rigidity and minority protection onto powerless groups.
% ABSENT_VOICES: The constituent public (demos) is structurally excluded from direct participation in ongoing constitutional meaning-making under this reading. Minority groups and out-of-power factions who would argue for fixed constitutional limits are present but lack voting power in the legislative majority. Courts that might prefer to hold interpretive authority are excluded from the final say.
% DISAPPEARANCE_RATIONALE: If this reading vanished (replaced by judicial supremacy or popular sovereignty), the entire architecture of constitutional adjudication would reorganize: courts would hold final authority, or the demos would be empowered to override both courts and legislatures. The distribution of power over constitutional meaning would shift fundamentally. Legislative majorities would lose their unilateral power to redefine constitutional protections.
% FOUNDING_PROBLEM: Early parliamentary systems faced chaos from interpretive conflicts: unelected judges overruling legislatures, disputed meaning of ancient charters, no clear resolution procedure. This reading establishes the legislature as the ultimate arbiter to ensure democratic legitimacy and settle disputes through electoral accountability.
% FOUNDING_PROBLEM_CORROBORATION: Legislative sovereigntists and majoritarian democracies attest the founding problem remains live and this reading solves it efficiently. Courts in majoritarian systems and judicial-supremacy advocates attest the problem has been solved differently in other jurisdictions with better minority protection outcomes; comparative rights-violation data from UN and regional human rights bodies documents the costs to minorities under legislative supremacy. Independent political theorists dispute whether democratic legitimacy requires the majoritarian override power or whether it is compatible with judicially enforced constitutional limits.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).

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
 *   Extractiveness begins at 0.55 (baseline majoritarianism has a coordination function and some legislative restraint) and rises to 0.68 as legislatures accumulate override history and minorities lose confidence in courts as protectors. Theater rises from 0.28 to 0.42 because courts develop doctrines of deference and restraint that preserve the appearance of independence while accepting legislative supremacy — performative independence under material constraint. Suppression requirement rises from 0.58 to 0.71 as the constraint requires increasing active enforcement: legislatures must override courts when courts drift toward protecting minorities; courts must suppress their own inclinations toward rigorous rights protection; minorities must be prevented from appealing to courts or the demos for relief. All measurements share a single time grid (interval 0–32) so every metric is authored at every examined point. The trajectory models extraction accumulation: the coordination function (settled meaning) persists but the extractive component (majoritarian power asymmetry) deepens as the override power is exercised and minorities face the pattern.
 *
 * PERSPECTIVAL GAP:
 *   The legislative majority and the minority rights holders should compute dramatically differently. From the majority's position, this is democratic legitimacy and efficient coordination; from the minority's position, it is entrenched extraction protected by their powerlessness. From the judicial seat, it is institutional constraint (can reason but not enforce) balanced against the appearance of independence. The engine computes these divergences from the power atoms (institutional vs. powerless vs. constrained), exit options (analytical vs. trapped vs. constrained), and beneficiary/victim declarations. The authored claim (tangled_rope) reflects the reading's self-framing; the metrics (extractiveness, suppression, theater_ratio) describe what the constraint does to the powerless and the minorities. The divergence between claim and metrics is exactly the measurement the corpus takes: does the majority's majoritarian coordination story match how minorities experience the constraint?
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority is the structural beneficiary — they control the override power and its exercise. Their directionality is low (~0.15–0.25: they define the rules and reap the benefits of majoritarian will-setting without bearing its costs). Minority rights holders are the structural targets — they pay (loss of protection) and have no exit (trapped). Their directionality is high (~0.75–0.90). Out-of-power factions sit between (moderate power, constrained exit, payer role) with directionality around 0.50–0.65. Courts are the institutional in-between: they benefit from their role and constrained independence but pay by operating under override threat; directionality around 0.40–0.50. Judicial_actors are neither fully beneficiary nor fully target; they participate in the coordination while being constrained by it. These directionalities derive from the power atoms and exit options authored above; no override is needed because the structural data captures the true asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the classification failure of calling legislative supremacy a 'pure rope' (which would ignore the asymmetric extraction of minority protections). It also avoids calling it 'pure snare' (which would ignore the genuine coordination function of settled meaning). The tangled_rope classification holds both: yes, courts and legislatures solve a real collective-action problem (who decides constitutional meaning); yes, the solution is structurally asymmetric (majorities win, minorities lose) and requires active enforcement (legislatures must override courts when courts protect minorities; courts must preemptively restrain themselves). The beneficiary and victim declarations are explicit (legislative_majority benefits; minority_rights_holders and out_of_power_factions pay). The requires_active_enforcement flag is true because the override power must be actively available and courts must be actively constrained. The measurement trajectory shows extraction accumulating (as override power is exercised, suppression requirement rises), which is diagnostic of a tangled_rope that is drifting toward snare as the extraction component dominates and the coordination function becomes pretense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_democracy_vs_constitutional_limits,
    'Is unlimited majoritarian power the authentic reading of democratic constitutionalism, or does legitimate democracy require judicially enforceable limits on what majorities can do to minorities?',
    'This is a conceptual omega: it rides on how one understands democracy itself. Different political traditions (Rousseauian popular sovereignty vs. Madisonian checks-and-balances vs. liberal constitutionalism) disagree on the answer. No empirical fact resolves it; different frameworks make different answers appear obvious.',
    'If majoritarian democracy is authentic, this reading is justified and judicial supremacy is counter-majoritarian overreach. If democracy requires constitutional limits, this reading is tyranny of the majority dressed in electoral legitimacy. The classification diverges based on which framework one holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_democracy_vs_constitutional_limits, conceptual, 'Whether unlimited majoritarian power is democratic or whether authentic democracy requires judicially enforced constitutional limits.').

omega_variable(
    legislative_override_scope_ambiguity,
    'Does ''legislative supremacy'' mean the legislature can override the constitution on any subject, or only on specified matters? Are there implicit constitutional limits even in a supremacist reading?',
    'Textual analysis of constitutional provisions (notwithstanding clauses, amendment procedures) and historical practice in parliaments that claim supremacy. The operative question: has any supremacist jurisdiction actually tested the boundary?',
    'If some topics are off-limits to legislative override, the reading is weaker than it claims — the constitution retains some enforcement against the legislature. If truly unlimited, the classification is snare rather than tangled_rope: no genuine coordination protection, pure extraction of majority power. The ambiguity goes to whether this is coordination (legislatures respecting constitutional bounds they can override but choose not to) or coerced extraction (minorities have no exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_override_scope_ambiguity, empirical, 'Whether legislative override power is truly unlimited or has implicit constitutional scope limits.').

omega_variable(
    court_independence_under_legislative_supremacy,
    'Can courts maintain genuine independence and honest reasoning if they know their findings can be overridden by the legislature? Does the override power corrupt judicial integrity or is it compatible with honest interpretation?',
    'Examine institutional patterns in supremacist jurisdictions: do courts develop doctrines of deference that preemptively align with legislative preferences? Do they maintain the appearance of independence while functionally becoming legislative advisors? Do judges who find against the legislature face retaliation or just legislative override?',
    'If override power drives courts to preemptive deference, the constraint''s suppression is higher than authored (internalized suppression of judicial reasoning). If courts maintain independence and reason honestly despite knowing they can be overridden, the constraint is genuine tangled_rope (coordination + extraction, both real). The theater ratio may rise (courts performing independence while being overridable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_independence_under_legislative_supremacy, empirical, 'Whether court independence survives legislative override power or degrades into preemptive deference.').

omega_variable(
    kernel_reading_contest,
    'Is this constraint a defensible reading of the constitutional text, or is it one among three structurally distinct readings (legislative supremacy vs. judicial supremacy vs. popular sovereignty) that dispute the same kernel?',
    'This omega routes the committer-axis structure through the apparatus: the three readings (this one + judicial_supremacy_reading + popular_sovereignty_reading) are each structurally valid, each grounded in different parts or theories of the same constitutional text. They coexist as live options in political discourse. The engine computes per-seat type for this reading; divergence from the other readings is not error but signal that the kernel is genuinely contested.',
    'This reading instantiates legislative supremacy and computes extraction + enforcement patterns accordingly. The sibling readings will compute differently from different seats and different data. No reading forecloses the others within a single framework — they coexist across different legal traditions and institutional architectures. The classification depends on the reading chosen; disagreement on the reading is not a misclassification but evidence of kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the contested constitutional_text kernel; sibling readings are judicial_supremacy_reading and popular_sovereignty_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__legislative_sovereignty_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(cons_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(cons_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cons_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_text kernel. It is structurally linked to judicial_supremacy_reading (alternative authority location) and popular_sovereignty_reading (alternative authority location). All three readings are generated from the same contested kernel but instantiate different constraint structures with different ε, beneficiary sets, and classifications. They do not replace each other; they coexist as live positions in constitutional discourse. The network edges indicate which constraints are affected by the reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
