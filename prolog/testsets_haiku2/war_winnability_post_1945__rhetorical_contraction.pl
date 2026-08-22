% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Winnability Rhetorical Taboo in Strategic Planning
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   After 1945, strategic planners immediately confronted the question of
 *   whether nuclear weapons made total war winnable. Early planning (SAC,
 *   SIOP development) operated in winnability terms—counterforce targeting,
 *   damage limitation, constrained escalation. But mutual vulnerability grew
 *   undeniable by the 1960s. Rather than resolving the strategic question,
 *   policy networks constructed a rhetorical taboo: winnability became
 *   unsayable in public discourse while remaining operationally planned. This
 *   reading instantiates the constraint as a tangled rope—genuine
 *   coordination function (stabilizing public deterrence messaging) coupled
 *   with asymmetric extraction (planning authority removed from democratic
 *   oversight and hidden behind the taboo). The kernel
 *   'war_winnability_post_1945' admits three structurally distinct readings:
 *   deterrence_unthinkable (winnability is categorically impossible and
 *   planning for it is incoherent), countervailing_thinkable (winnability is
 *   constrained but operationally real), and this
 *   reading—rhetorical_contraction (winnability is operationally planned but
 *   publicly unsayable). This reading describes the actual institutional
 *   arrangement: a dual-layer structure where discourse contracts while
 *   operations persist.
 *
 * KEY AGENTS:
 *   - Strategic planning establishment: military and intelligence agencies (Joint Chiefs, Strategic Command, allied defence ministries) that maintain classified winnability planning and benefit from public rhetorical cover.
 *   - Democratic oversight institutions: legislatures and civilian defence ministries formally responsible for nuclear strategy but structurally excluded from accessing the planning assumptions that contradict public deterrence doctrine.
 *   - Classification apparatus: the secrecy regime (classification systems, compartmentation, clearance procedures) that technically enables the boundary between planning and discourse.
 *   - Academic strategic studies: universities and think tanks that publish on deterrence but operate within the discursive constraints the taboo imposes.
 *   - Public discourse participants: journalists, citizens, non-cleared intellectuals excluded from both planning and from speaking publicly about winnability without violating the taboo.
 *   - Deterrence doctrine maintainers: policy networks and institutions (RAND, defense intellectuals) whose prestige depends on maintaining the public deterrence consensus that the taboo enables.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.76).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Winnability Rhetorical Taboo in Strategic Planning").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '5f3692ed-dba6-4999-a805-4d1183688b0e').
narrative_ontology:cs_kernel_codification('5f3692ed-dba6-4999-a805-4d1183688b0e', fixed_text).
narrative_ontology:cs_authority_grounding('5f3692ed-dba6-4999-a805-4d1183688b0e', extraction).
narrative_ontology:cs_interpretation_layer_present('5f3692ed-dba6-4999-a805-4d1183688b0e').
narrative_ontology:cs_reading_relation('5f3692ed-dba6-4999-a805-4d1183688b0e', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('5f3692ed-dba6-4999-a805-4d1183688b0e', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('5f3692ed-dba6-4999-a805-4d1183688b0e', foundational, winnability_operationally_constrained).
narrative_ontology:cs_axiom_status(winnability_operationally_constrained, holdable).
narrative_ontology:cs_axiom_grounding('5f3692ed-dba6-4999-a805-4d1183688b0e', winnability_operationally_constrained, empirically_contingent).
narrative_ontology:cs_axiom('5f3692ed-dba6-4999-a805-4d1183688b0e', foundational, public_discourse_winnability_unsayable).
narrative_ontology:cs_axiom_status(public_discourse_winnability_unsayable, holdable).
narrative_ontology:cs_axiom_grounding('5f3692ed-dba6-4999-a805-4d1183688b0e', public_discourse_winnability_unsayable, conventional).
narrative_ontology:cs_reference_frame('5f3692ed-dba6-4999-a805-4d1183688b0e', operational_winnability_possible).
narrative_ontology:cs_drift_state('5f3692ed-dba6-4999-a805-4d1183688b0e', post_mutual_vulnerability_recognition, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('5f3692ed-dba6-4999-a805-4d1183688b0e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, academic_strategic_studies).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, allied_military_establishments).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, deterrence_doctrine_maintainers).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, academic_strategic_studies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military and national security planners (Joint Chiefs, Strategic Command, allied defence ministries, think tanks with classified access) maintain operational war plans that model scenarios for nuclear exchange and damage limitation. They justify winnability planning as prudent contingency—if deterrence fails, strategy cannot be caught unprepared. The taboo on public discussion of winnability insulates this planning from legislative scrutiny and prevents the emergence of competing civilian strategic frameworks. They benefit from operational flexibility without accountability pressure.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Legislatures, parliamentary committees, and civilian defense ministries formally control nuclear doctrine and funding but are structurally excluded from access to classified planning assumptions. The rhetorical taboo on winnability prevents them from publicly challenging the operational planning that remains hidden. They fund the strategic apparatus but cannot audit its theoretical foundations without breaking the taboo—which would trigger secrecy claims and career costs for the challenger. Their exit option is limited to refusing appropriations (politically costly and ineffective given bipartisan security consensus) or public denunciation (which activates the taboo's enforcement machinery).
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions, payer,
    organized, generational, constrained, national).

% Universities and think tanks publish on deterrence theory, but access to classified planning and the rhetorical taboo shapes what can be said publicly. Those with clearances gain prestige and institutional access but are constrained by what they cannot disclose. Those without clearances exist in a artificially narrowed discursive space where winnability is unsayable as a live operational question. The taboo benefits the most prestigious institutions (those with classified access) while imposing discursive constraints on all of them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_strategic_studies, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, academic_strategic_studies, payer).

% NATO and other allied militaries receive classified briefings on U.S. strategic planning and benefit from its operational sophistication. The winnability taboo allows them to maintain deterrent credibility in public while operationally preparing for damage-limitation scenarios. They have constrained exit (alliance dependency) but strategic interest alignment with the planning establishment.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, allied_military_establishments, beneficiary,
    institutional, civilizational, constrained, global).

% Russia, China, and other nuclear powers conduct their own winnability planning (countervailing strategies, damage limitation) but are excluded from the Anglo-American public discourse that treats winnability as unsayable. They would object that the taboo is selective enforcement of a norm that Western planners violate operationally. Their exclusion from the conversation is the arrangement's enforcement mechanism: they cannot credibly cite Western winnability planning without declassifying their own, so the taboo holds across the adversarial divide.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_armed_adversaries, excluded,
    institutional, civilizational, trapped, global).

% Citizens, journalists, and non-cleared public intellectuals cannot access the classified planning or speak publicly about winnability without violating the taboo. The constraint channels them toward deterrence rhetoric (mutual assured destruction, strategic stability) that does not acknowledge the operational planning that contradicts it. Their exclusion is structural: they have no access gate and cannot break the taboo without professional or legal consequences.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public_discourse_participants, excluded,
    powerless, biographical, trapped, global).

% National security classification systems (secrecy laws, compartmentation protocols, security clearance procedures) provide the technical mechanism that separates operational planning from public discourse. The apparatus is not a party with interests—it is an agent of the arrangement. It enforces the boundary that makes the taboo possible.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, classification_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Policy networks and institutions (RAND, defense intellectuals, diplomatic corps) that stake prestige and institutional identity on mutual assured destruction and strategic stability doctrine benefit from the winnability taboo. It prevents the emergence of alternative frameworks (like countervailing strategy) gaining public legitimacy. They can exit by changing their intellectual positions, but reputational and funding incentives constrain that mobility.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, deterrence_doctrine_maintainers, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear strategy from becoming a live electoral and legislative issue, stabilizing public deterrence messaging across administrations and electoral cycles. Creates a shared rhetorical framework that gives adversaries credible signals about commitment to mutual vulnerability. Isolates military planning from constant civilian interference and political instability.
% TRANSFER_FUNCTION: Transfers authority over nuclear strategy from legislatures and accountable civilian institutions to classified military and intelligence establishments. Moves winnability from a live strategic question open to democratic revision to a taboo topic hidden behind classification. Transfers oversight capacity from democratic institutions to classified review processes inaccessible to legislatures. Extracts the ability to audit and revise strategy.
% ABSENT_VOICES: Adversarial military establishments (Russia, China, others conducting winnability planning) who would object that the Western taboo is selective enforcement. Declassified planners with access to classified documents who could testify to the operational reality of winnability planning. Non-cleared academics and civil society organizations that would challenge the framework if they had access to planning assumptions. Congressional oversight staff without clearance. International humanitarian law advocates and nuclear abolition movements who would argue winnability planning violates norms of proportionality and distinction.
% DISAPPEARANCE_RATIONALE: If the winnability taboo disappeared, classified planning documents would enter legislative and public review, winnability would become a live strategic question open to formal debate, deterrence doctrine would fragment into competing schools (countervailing, damage limitation, mutual assured destruction as incompatible alternatives). Military budgets and doctrine would face explicit civilian challenge. Allied militaries would lose diplomatic cover for their own winnability planning. The taboo's disappearance would not immediately change what planners do, but it would make the planning politically contestable and subject to democratic revision. Strategic consensus would fragment.
% FOUNDING_PROBLEM: After 1945, nuclear weapons created strategic ambiguity about winnability. Early planning (SAC through SIOP development) operated in winnability terms. By the 1960s, mutual vulnerability became undeniable and winnability planning became politically dangerous—it signaled irrationality and threatened deterrence credibility. The founding problem was not solved but suppressed: winnability remained operationally real but became publicly unsayable. The taboo emerged as a solution to the problem of managing a strategic contradiction—planners needed operational flexibility but the public needed confidence in mutual vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and officials with classified access (Joint Chiefs, Strategic Command, declassified memoirs from Kissinger, McNamara, Bush administration officials) attest that winnability planning remains operationally necessary and that the founding problem (how to plan if deterrence fails) persists. Anti-nuclear advocates and some international security scholars (Sagan, Jervis, Rhodes) attest that the founding problem is constructed and that winnability planning perpetuates the very strategic competition it claims to deter. Historians of the Cold War document the taboo's emergence post-1962 and its institutionalization. Declassified strategic documents (Nuclear Posture Reviews, Presidential Decision Directives under FOIA litigation) show operational planning for limited nuclear war, confirming that the founding problem (how to operationally plan for nuclear war) remains live among planners. No corroboration from outside the military establishment directly attests that winnability planning is strategically necessary—the corroboration is from planners themselves and from historians analyzing their documents.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint systematically transfers authority over nuclear strategy from accountable civilian institutions to classified military establishments. The transfer is not coercive in the traditional sense—legislatures and the public do not perceive themselves as oppressed—but it is asymmetric: planners retain operational flexibility while oversight institutions lose access to the planning logic. Suppression is higher still (0.76) because the taboo is enforced through classification law, career incentives, and institutional gatekeeping—not through explicit prohibition but through making winnability unsayable without professional or legal cost. Theater ratio is high (0.71 at interval end) because the constraint's primary function (controlling public narrative) has become its whole operational reality. The performative maintenance of the taboo—repeated assertions that winnability is unthinkable—now exceeds the actual coordination work (stabilizing deterrence messaging is now diffuse and assumed rather than actively coordinated). The measurement series shows sharp inflection at 1962 (Cuban Missile Crisis, when the taboo crystallizes), continuing rise through the Cold War as suppression machinery hardened, and plateau post-2000 as the taboo became institutionalized and self-maintaining. The theater ratio spike (0.25 to 0.62 from 1962 to 1980) reflects the taboo's shift from emerging norm to performative enforcement; the plateau thereafter reflects stabilized theater.
 *
 * PERSPECTIVAL GAP:
 *   From the strategic planning establishment's seat: this is genuine coordination. The taboo solves a real problem—it prevents nuclear strategy from becoming a live electoral issue and keeps deterrent messaging stable. Public debate about winnability would destabilize alliance relationships and undermine deterrence credibility. From the democratic oversight seat: this is pure extraction disguised as coordination. The taboo allows planners to operate outside civilian control and hides from the public the military's actual theory of nuclear war. Democratic institutions fund the apparatus but cannot audit its assumptions without breaking a taboo that was constructed to prevent such audits. From the public discourse seat: the constraint is barely perceptible—it operates as a background norm that winnability is absurd, making the question feel intellectually incoherent rather than politically forbidden. The engine's per-seat computation should differentiate these three experiences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners are the beneficiaries (d near 0.0): they retain operational flexibility and institutional autonomy, protected by the taboo from political interference. Democratic institutions are the victims (d near 1.0): they formally control nuclear doctrine but are structurally excluded from the planning logic, their oversight capacity is privatized. The public sits near symmetric (d ≈ 0.5): they benefit from stable deterrence messaging that the taboo supports, but they pay in loss of informed democratic choice about nuclear strategy. Deterrence doctrine maintainers sit near beneficiary (d ≈ 0.2): their prestige and funding depend on the public framework the taboo maintains. Allied militaries are quasi-beneficiaries (d ≈ 0.25): they receive the strategic umbrella and classified briefings without the political cost of explicitly defending winnability planning. Adversarial powers are trapped (d ≈ 1.0): they are excluded from the conversation while Western planning proceeds operationally, and they cannot cite Western examples to legitimize their own winnability planning without breaking their own operational security.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear ambiguity about winnability post-1945) remains CONTESTED. Deterrence theorists argue winnability is categorically impossible and planning for it is irrational—the founding problem is solved, the taboo is obsolete, and winnability planning should be abandoned. Planners argue winnability remains operationally constrained but strategically necessary—the founding problem persists, planning must continue, and the taboo protects it. This classification prevents false mandatrophy: if the constraint were misclassified as a Rope (pure coordination), the contested founding problem would suggest the coordination is no longer necessary. Classifying it as Tangled Rope correctly captures that there IS genuine coordination (deterrence stability) AND genuine extraction (planning authority privatized). The asymmetric beneficiary/victim structure is the mandatrophy detector: a Rope with one beneficiary and one victim that requires active enforcement to persist is structurally identical to a Snare in disguise, which is exactly what this constraint is—extraction justified by coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the winnability taboo enforced primarily through structural barriers (classification law, institutional gatekeeping, career incentives) or through internalized belief that winnability is genuinely unthinkable?',
    'Post-exit behavior of declassified planners (do they continue to treat winnability as taboo after leaving service?) and exit-cost variation across seats (do academics with clearance internalize the taboo more deeply than those without?)',
    'If primarily structural, the constraint persists as long as the classification apparatus holds and could be disrupted by policy change. If partially internalized, the constraint is stickier—even absent formal suppression, many would continue to self-censor. If fully internalized, the constraint is barely distinguishable from genuine coordination around what is actually true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the winnability taboo is structurally enforced or internalized as belief.').

omega_variable(
    kernel_reading_contest_resolution,
    'Does the empirical record of classified planning (accessible through historical declassification, whistleblower documents, and memoir testimony) support deterrence_unthinkable or countervailing_thinkable as the actual strategic belief?',
    'FOIA litigation forcing declassification of strategic planning documents (Nuclear Posture Reviews, Presidential Decision Directives, war plan documentation) would provide direct evidence of whether planners truly believe winnability is impossible or merely treat it as operationally constrained.',
    'If deterrence_unthinkable is supported, winnability planning is cognitive dissonance and the taboo masks self-deception. If countervailing_thinkable is supported, the taboo conceals a strategically defensible (if contested) operational posture. If evidence is mixed, the readings coexist and the taboo becomes a coordination mechanism allowing different institutional seats to hold different underlying beliefs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Whether actual strategic planning reflects deterrence_unthinkable or countervailing_thinkable reading.').

omega_variable(
    taboo_vs_genuine_norm_distinction,
    'Is the winnability taboo a constructed suppression mechanism (the reading_contraction reading''s framing) or has it become a genuine normative consensus among strategic elites that winnability is incoherent?',
    'Ethnographic access to classified planning communities: do planners discuss winnability operationally when no record is kept, or has the taboo internalized such that they genuinely avoid the topic even in private?',
    'If constructed suppression, the taboo''s removal would rapidly resurrect winnability planning debate. If genuine norm, removal of the taboo would face resistance from strategic elites who genuinely believe winnability is unthinkable. The distinction maps to whether the constraint is extraction (suppression) or coordination (shared belief).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_genuine_norm_distinction, conceptual, 'Whether the taboo is imposed suppression or internalized norm.').

omega_variable(
    democratic_oversight_capability_loss,
    'How much of the constraint''s extractive effect derives from democracies losing the capability to audit and revise nuclear strategy, versus from planners gaining operational freedom to implement winnability planning that the public would reject if it knew?',
    'Counterfactual: if legislatures had full access to classified planning and could vote on winnability doctrine, would they reject it? Public opinion polling on winnability planning (when explained clearly) provides indirect evidence.',
    'If oversight loss is small (legislatures would approve existing planning if they knew about it), the extraction is moderate and justified as classified coordination. If oversight loss is large (legislatures would demand major changes), the extraction is severe and the taboo is a suppression mechanism preventing democratic revision of strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_oversight_capability_loss, empirical, 'Whether democratic oversight loss is incidental or central to the constraint''s extraction.').

omega_variable(
    kernel_reading_coexistence_possibility,
    'Can deterrence_unthinkable and countervailing_thinkable readings coexist within a single strategic community, or does one foreclose the other?',
    'Historical and contemporary analysis of strategic communities: do prominent theorists hold mutually exclusive positions on winnability, or can they hold different positions about different scenarios (e.g., unthinkable for strategic exchange, thinkable for limited counterforce)?',
    'If mutually exclusive, at most one sibling reading is actually held and the kernel contest maps to competing factions. If they coexist, the kernel admits genuine pluralism and the rhetorical_contraction reading is the mechanism allowing institutional seats to operate under different strategic theories. This determines whether the cs_structure.reading_relations should use ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_possibility, conceptual, 'Whether kernel readings are mutually exclusive or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.62).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.71).
narrative_ontology:measurement(war__tr_t2012, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2012, 0.71).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2024, 0.71).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(war__be_t2012, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1962, 0.42).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(war__su_t2012, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2012, 0.76).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2024, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.18).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, deterrence_unthinkable_reading).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, countervailing_thinkable_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_winnability_post_1945 kernel. The kernel admits three structurally distinct constraints corresponding to three different strategic beliefs about nuclear war. Rhetorical_contraction describes the actual institutional arrangement where winnability is operationally planned but publicly unsayable. Deterrence_unthinkable describes a reading where winnability is categorically impossible (more foundational constraint). Countervailing_thinkable describes a reading where winnability is constrained but operationally real (alternative strategic posture). These three constraints are linked via network.affects_constraints because the kernel contest determines which reading's institutional seats have power to set strategy. A shift from rhetorical_contraction to countervailing_thinkable (declassification and public acknowledgment of winnability planning) would change the constraint's structure from tangled_rope to snare; a shift to deterrence_unthinkable would require abandoning winnability planning and reclassifying the arrangement as pure coordination without extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
