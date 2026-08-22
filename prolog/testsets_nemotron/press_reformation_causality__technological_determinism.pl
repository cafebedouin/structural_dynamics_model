% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Enabler of Reformation Inevitability
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The technological_determinism reading of press_reformation_causality
 *   treats the printing press as an autonomous physical-technological
 *   mountain: a fixed constraint of nature/engineering that made vernacular
 *   scripture dissemination and Reformation success structurally inevitable.
 *   Human actors (reformers, princes, printers) are downstream responders to
 *   the technology's affordances, not shapers of its deployment. This reading
 *   naturalizes a contingent historical process — the press existed for 70
 *   years before Luther; Catholic territories used it for
 *   Counter-Reformation; outcomes varied wildly by region. The reading's
 *   mountain claim obscures the strategic agency of reformers who weaponized
 *   print, the economic agency of printers who built vernacular markets, and
 *   the political agency of princes who adopted Protestantism for territorial
 *   sovereignty. The beneficiary structure (Protestant princes, vernacular
 *   printers, literate merchant classes) is rendered as 'natural consequence'
 *   rather than interested parties.
 *
 * KEY AGENTS:
 *   - printing_press_technology: Primary constraint (mountain claim) — physical enabling infrastructure
 *   - protestant_reformers: Primary beneficiaries (but framed as responders) — Luther, Calvin, Zwingli etc.
 *   - vernacular_printers: Secondary beneficiaries — commercial operators capturing new markets
 *   - literate_lay_populations: Tertiary beneficiaries — gaining scripture access
 *   - catholic_authorities: Resistant actors — attempted to deploy same technology for Counter-Reformation
 *   - princes_magistrates: Political beneficiaries — gained church lands and sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.12).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.12).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Enabler of Reformation Inevitability").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'bcb78963-fa91-475a-b0e6-3f2aa1bf6e65').
narrative_ontology:cs_kernel_codification('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', implicit).
narrative_ontology:cs_authority_grounding('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', distributed).
narrative_ontology:cs_reading_relation('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', foundational, technology_as_autonomous_causal_prime_mover).
narrative_ontology:cs_axiom_status(technology_as_autonomous_causal_prime_mover, holdable).
narrative_ontology:cs_axiom_grounding('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', technology_as_autonomous_causal_prime_mover, empirically_contingent).
narrative_ontology:cs_axiom('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', foundational, human_agency_as_downstream_epiphenomenon).
narrative_ontology:cs_axiom_status(human_agency_as_downstream_epiphenomenon, holdable).
narrative_ontology:cs_axiom_grounding('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', human_agency_as_downstream_epiphenomenon, deontological).
narrative_ontology:cs_reference_frame('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', pre_print_manuscript_culture).
narrative_ontology:cs_drift_state('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', post_eisenstein_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bcb78963-fa91-475a-b0e6-3f2aa1bf6e65', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, literate_lay_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, princes_magistrates).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, print_reformation_causality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The movable-type printing press as physical-technological infrastructure. Under this reading, it is the constraint itself — an autonomous mountain that enables vernacular mass reproduction. It has no agency, no interests, no exit. It simply exists as a structural fact of the period.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Luther, Calvin, Zwingli and their networks. Under this reading they are framed as responders to the press's affordances — the technology made their message scalable. They benefit enormously (message reaches millions instead of hundreds) but are not coded as strategic agents. Their 'exit' is constrained: once the press exists, the only way to reach mass audiences is through it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_reformers, beneficiary,
    organized, biographical, constrained, continental).

% Commercial printers in Wittenberg, Basel, Strasbourg, Geneva, Antwerp who built vernacular markets. They capture new revenue streams from religious publishing. Under this reading their entrepreneurship is rendered as 'responding to demand created by the technology' rather than active market-making. Exit is constrained: the press is the only production technology.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_printers, beneficiary,
    moderate, biographical, constrained, regional).

% Urban artisans, merchants, minor clergy who gain access to vernacular scripture. They are the downstream beneficiaries of the mountain's operation. They have no exit from the information environment the press creates — once vernacular Bibles exist, the old Latin-only world is inaccessible. Their 'benefit' is access, but they do not choose the constraint.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, literate_lay_populations, beneficiary,
    powerless, generational, trapped, local).

% Papacy, bishops, Inquisition, Catholic princes. Under this reading they are downstream responders forced to react to the press's Protestant deployment. They bear costs (loss of doctrinal control, territory, revenue) but are not coded as strategic agents. Their 'exit' is constrained: they must adopt print for Counter-Reformation or lose further ground.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_authorities, payer,
    institutional, generational, constrained, continental).

% German princes, Swiss cantons, Scandinavian monarchs who adopt Protestantism and seize church lands. Under this reading their political calculations are framed as 'made possible by the technology' rather than strategic choices. They have mobile exit: they can choose confession based on sovereignty interests. Their benefit (lands, autonomy) is rendered as technological inevitability.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, princes_magistrates, beneficiary,
    powerful, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solves the coordination problem of mass-producing identical texts at scale, replacing error-prone manual copying with reproducible editions. This enables standardized vernacular scripture, shared liturgical texts, and coordinated polemical campaigns across distances.
% TRANSFER_FUNCTION: The arrangement moves textual authority and interpretive monopoly from Latin-literate clergy to vernacular-literate populations, and moves economic value from manuscript production to print publishing. It transfers doctrinal control from centralized ecclesiastical authority to distributed print networks.
% ABSENT_VOICES: Illiterate populations (majority in 1500) who could not access vernacular print directly; oral culture bearers whose transmission modes were displaced; women excluded from print authorship and clerical office; Jewish and Muslim communities in Europe whose textual traditions were not served by the vernacular press; Catholic reformers who wanted to use print for internal renewal but were framed as reactive.
% DISAPPEARANCE_RATIONALE: If the printing press vanished in 1520, the Reformation as a mass movement would collapse — Luther's writings reached 300,000+ copies by 1525. Vernacular scripture dissemination would revert to manuscript speed. Catholic doctrinal control would likely reconsolidate. The political map of Europe (cuius regio eius religio) would not have formed as it did. The world rearranges dramatically.
% FOUNDING_PROBLEM: The founding problem of the press_reformation_causality kernel is: how did a religious movement achieve mass adoption and territorial establishment across Europe in decades rather than centuries? The technological_determinism reading answers: the printing press as autonomous enabler made this inevitable by solving the reproduction bottleneck.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) 'The Printing Press as an Agent of Change' corroborates the technological enabling thesis from outside the Protestant beneficiary set. Febvre & Martin (1958) 'The Coming of the Book' documents the press's spread but notes regional variation contradicting inevitability. Modern historians (Pettegree, Johns, Rubin) largely reject strong determinism — corroboration for the reading's core claim is thin outside its own tradition.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The authored metrics reflect the technological_determinism reading's own internal logic: the press as physical infrastructure has near-zero extractiveness (it doesn't 'extract' — it enables), negligible suppression (no one is forced to use it), minimal theater (it does what it does), high accessibility collapse (once movable type exists, hand-copying is irreversibly obsolete), and near-zero resistance (the technology faces no active opposition). The reading's claimed_type = mountain is internally consistent with these metrics. The divergence emerges only when compared to sibling readings on the same kernel, which instantiate different constraints with different epsilon values and beneficiary structures.
 *
 * PERSPECTIVAL GAP:
 *   The technological_determinism reading produces a single seat (the technology as mountain) with no seat divergence because it denies agency to human actors. The strategic_deployment reading would produce divergent seats: reformers as agenda_setters, printers as beneficiaries/payers, Catholic authorities as payers/resisters, princes as agenda_setters/beneficiaries. The co_constitution reading would produce a feedback-loop structure where seats are mutually constitutive. The perspectival gap IS the kernel contest: different readings instantiate different constraint structures with different seat topologies.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading's framing, the printing press technology itself is the constraint with no directionality (it is the mountain). Human actors are not 'seats' of the constraint — they are downstream effects. The beneficiaries listed (protestant_reformers, vernacular_printers, literate_lay_populations) are declared as structural consequences of the mountain's operation, not as agents with directional relationship to it. This is the reading's core move: it removes human agency from the constraint's structural description, rendering beneficiaries as natural outcomes rather than interested parties.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — this is a kernel reading, not a mandate-bearing institution. The 'founding problem' (disseminating scripture) is live under this reading but contested across the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Is this constraint a genuine physical/technological mountain, or a constructed reading that obscures human agency and beneficiary structure?',
    'Compare the technological_determinism reading''s epsilon against sibling readings (strategic_deployment, co_constitution) on the same kernel. If sibling readings instantiate substantially different extractiveness and beneficiary structures, the ''mountain'' classification is reading-relative, not ε-invariant across the kernel.',
    'If the mountain classification holds only under this reading''s framing, the constraint is a false summit mountain (FSM candidate) — the technological_determinism reading serves as a cover story that naturalizes what is actually a contingent historical process with identifiable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the ''mountain'' classification is kernel-invariant or reading-relative across press_reformation_causality kernel').

omega_variable(
    beneficiary_obscuration_ambiguity,
    'Does the technological_determinism reading''s classification as mountain obscure a beneficiary structure that would reclassify the constraint under other readings?',
    'Trace material benefits: Protestant princes gained legitimacy and church lands; vernacular printers captured new markets; literacy expanded among merchant classes. Under strategic_deployment, these are intentional beneficiaries; under co_constitution, they are co-constituted. If benefits are structural rather than incidental, the mountain claim is false summit.',
    'If beneficiaries are structural and intentional, the constraint reclassifies as tangled_rope (coordination + extraction) under readings that surface agency. The technological_determinism reading''s mountain claim functions as naturalization ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_obscuration_ambiguity, empirical, 'Whether mountain classification obscures structural beneficiaries visible under sibling readings').

omega_variable(
    inevitability_falsifiability,
    'Is Reformation ''inevitability'' given print technology falsifiable, or is it a post-hoc narrative that resists counterfactual testing?',
    'Counterfactual modeling: regions with presses but no Reformation (e.g., Italy, Spain); regions with Reformation but limited vernacular print (e.g., Swiss rural cantons). If outcomes diverge despite same technology, inevitability claim fails.',
    'If inevitability is falsified by counterfactuals, the constraint is not a mountain (mountains do not have counterfactual variance). The technological_determinism reading would be a constructed narrative, not a structural constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inevitability_falsifiability, empirical, 'Whether the inevitability claim survives counterfactual historical testing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.02).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causality__technological_determinism, theater_ratio, 1520, 0.04).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.05).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causality__technological_determinism, base_extractiveness, 1520, 0.1).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.12).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causality__technological_determinism, suppression_requirement, 1520, 0.07).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.08).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% Kernel decomposition: press_reformation_causality splits into three readings with different epsilon and seat structures. technological_determinism = mountain (epsilon ~0.12, no human seats). strategic_deployment = tangled_rope (epsilon ~0.45, agenda_setter reformers, beneficiary printers/princes, payer Catholic authorities). co_constitution = rope/scaffold hybrid (epsilon ~0.25, mutual constitution seats). The technological_determinism reading naturalizes what the other readings reveal as contingent agency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
