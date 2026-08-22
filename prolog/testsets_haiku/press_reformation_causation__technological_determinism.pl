% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press Material Constraint on Textual Reproducibility
 *   domain: history_of_technology/religious_history
 *
 * SUMMARY:
 *   The technological_determinism reading of the press-Reformation causation
 *   kernel asserts that the printing press created a material condition —
 *   reproducibility at scale below hand-manuscript cost — that made
 *   censorship impossible and vernacular scripture inevitable. On this
 *   reading, reformers are beneficiaries of exogenous technological capacity,
 *   not strategic agents who shaped the technology or exploited neutral
 *   tools. The Church's authority over textual production was structurally
 *   undermined by a material fact, not by rhetorical force or political will.
 *   This reading claims the press as a mountain: a natural-law-like
 *   constraint on textual economics independent of any actor's agency.
 *
 * KEY AGENTS:
 *   - printing press technology: the exogenous technological condition (not an agent, modeled as observer)
 *   - reformation movements: beneficiaries of the press's capacity to distribute vernacular theology at scale
 *   - vernacular readers: beneficiaries of the collapse in reproduction costs; could access texts in their own language
 *   - roman church authority: target/victim; monopoly on textual production and interpretation was undermined by technological fact
 *   - printers and publishers: channels for the technological imperative rather than strategic agents (in this reading's frame)
 *   - feudal authorities: excluded from the decision to adopt printing but caught in its downstream effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press Material Constraint on Textual Reproducibility").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '140c39f6-1fcd-43c7-8952-5873762436b5').
narrative_ontology:cs_kernel_codification('140c39f6-1fcd-43c7-8952-5873762436b5', distributed).
narrative_ontology:cs_authority_grounding('140c39f6-1fcd-43c7-8952-5873762436b5', diffuse_epistemic).
narrative_ontology:cs_reading_relation('140c39f6-1fcd-43c7-8952-5873762436b5', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_reading_relation('140c39f6-1fcd-43c7-8952-5873762436b5', press_reformation_causation__strategic_deployment, influences).
narrative_ontology:cs_axiom('140c39f6-1fcd-43c7-8952-5873762436b5', foundational, technology_exogenous_to_agency).
narrative_ontology:cs_axiom_status(technology_exogenous_to_agency, holdable).
narrative_ontology:cs_axiom_grounding('140c39f6-1fcd-43c7-8952-5873762436b5', technology_exogenous_to_agency, empirically_contingent).
narrative_ontology:cs_axiom('140c39f6-1fcd-43c7-8952-5873762436b5', foundational, material_cost_structure_determines_feasibility).
narrative_ontology:cs_axiom_status(material_cost_structure_determines_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('140c39f6-1fcd-43c7-8952-5873762436b5', material_cost_structure_determines_feasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('140c39f6-1fcd-43c7-8952-5873762436b5', pre_printing_hand_manuscript_monopoly).
narrative_ontology:cs_drift_state('140c39f6-1fcd-43c7-8952-5873762436b5', post_gutenberg_press_ubiquity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('140c39f6-1fcd-43c7-8952-5873762436b5', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, reformation_movements).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, roman_church_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Material substrate: the capacity to reproduce texts rapidly at scale without requiring hand-copying per copy. Not an agent but the technological condition the reading treats as causally upstream.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Religious reformers whose core message — vernacular scripture, accessible theology, critique of ecclesiastical authority — became distributable at scale once the press made hand-copying obsolete. Benefited from a technological shift they did not initiate.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, reformation_movements, beneficiary,
    organized, generational, mobile, continental).

% Readers of non-Latin languages could access scripture and theological argument in their own tongue once printing made large-scale vernacular production economically viable. Benefited from technological capacity, not from organized advocacy.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    powerless, biographical, mobile, continental).

% Ecclesiastical monopoly on authoritative textual production and interpretation was structurally undermined when copying costs collapsed. Suppression of vernacular texts became physically impossible to enforce at scale — the constraint is treated as exogenous to Church strategy, something that happened to the Church rather than something the Church chose.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, roman_church_authority, payer,
    institutional, civilizational, constrained, continental).

% Economic actors responding to market demand for texts. Under the technological_determinism reading, printers are channels for the technological imperative rather than strategic agents; demand follows from the press's capacity to meet it, not from printer foresight.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers_and_publishers, observer,
    organized, biographical, mobile, continental).

% Local and regional rulers who had depended on the Church for legitimacy and literate administration could not prevent vernacular literacy and religious challenge once texts were reproducible at scale. Were not party to decisions about printing adoption but faced downstream consequences.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, feudal_authorities, excluded,
    powerful, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading does not describe a coordination problem being solved. It describes a technological condition (reproducibility at scale) that makes prior arrangements (monopoly on text production) structurally impossible to maintain.
% TRANSFER_FUNCTION: No transfer in the coordination sense. The constraint is treated as a physical condition: once copying costs approach zero per unit, the economic logic that sustained hand-manuscript monopoly evaporates. The 'extraction' the Church bore was the loss of control, not a negotiated transfer.
% ABSENT_VOICES: Scribal copyists, whose labor was rendered economically redundant by the press, are not visible in the founding-problem framing. They would attest that demand for copying services existed and was profitable before the press; the technological determinism reading erases them as agents by treating their displacement as inevitable rather than chosen.
% DISAPPEARANCE_RATIONALE: If the printing press had not been invented, the technological condition — that hand-copying is expensive and slow — would persist. The Reformation as a movement would face the same structural barriers: vernacular scriptures would remain scarce and costly; censorship would remain enforceable via copying-bottleneck control. The world does not rearrange because the technological capacity was exogenous; the world was shaped by it.
% FOUNDING_PROBLEM: The technological determinism reading does not pose a founding problem in the coordination sense. Instead, it asserts a technological fact: hand-copying is materially expensive and slow; printing technology (Gutenberg, moveable type, ink chemistry) reduced per-unit reproduction cost by orders of magnitude. This fact created new structural possibilities independent of any actor's intent.
% FOUNDING_PROBLEM_CORROBORATION: The cost structure of pre-printing textual production is attested by manuscript studies historians and paleographers (outside the Reformation advocacy set): labor-hours per page, ink costs, vellum scarcity. The mechanical capability of the printing press is attested by engineering history and by surviving press equipment. These are empirical facts about material technology, corroborated by disciplinary work that predates Reformation historiography.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.15, rising slightly to 0.15 by interval end) reflects the Church's loss of control over textual reproduction — a form of extraction in the sense that the prior monopoly rent is now unavailable to the Church. But the constraint is claimed as a mountain because the loss is treated as driven by material fact (reproducibility economics) rather than by active extraction on anyone's part. Suppression (0.08, stable) is low because the constraint is not enforced through coercive mechanisms — it emerges from the physical cost structure of copying. Theater (0.02, negligible) reflects genuine function with minimal performative overhead: printing genuinely reduces per-unit cost; no significant ritual maintains the arrangement. Accessibility_collapse (0.92) is high because once the press's capability became available, the alternative of hand-copying at pre-press costs became structurally unavailable — copying could not compete economically. Resistance (0.05) is very low because the technological_determinism reading frames the constraint as exogenous: the Church could not meaningfully resist a material fact about reproduction cost any more than one could resist gravity. The measurement series remains stable through the interval because the reading treats the technological condition as a standing fact — once the press became materially available (circa 1440), the constraint's structural properties did not change, though the social consequences unfolded gradually.
 *
 * PERSPECTIVAL GAP:
 *   The Church's seat and the beneficiary seats experience radically different types from the same constraint. From the Church's position, the press is an external shock — a snare if one focuses on how quickly printing spread and suppressed theological alternatives; a mountain if one focuses on the material fact that copying costs had to collapse eventually. From the reformer and reader positions, the press is a coordinating force opening access — a rope if one focuses on how distribution networks formed; a mountain if one focuses on the technological fact that made their access possible. The engine should compute these differently because the structural relationship differs: the Church is a target of extraction (loses monopoly rents), beneficiaries are net positive (gain access), and the technology is exogenous to both. The claim (mountain) asserts the exogeneity; the metrics describe the asymmetry. If the engine computes a different type for different seats, that divergence is the measurement this reading is designed to enable.
 *
 * DIRECTIONALITY LOGIC:
 *   The technological_determinism reading assigns directionality based on who benefits from the technological condition and who loses the prior monopoly rent. Reformation movements and vernacular readers are beneficiaries (d near 0.0) — they gain access to distributed texts without cost or effort to acquire the printing capacity. The Roman Church is the target (d near 1.0) — it loses the monopoly premium on authorized textual production. This directionality divergence is structural: the same technological fact (low-cost reproducibility) benefits some and harms others depending on prior positional control. Printers under this reading are treated as neutral channels rather than strategic agents, so their directionality is ambiguous — they profit from the press's viability but do not control its existence or propagation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in the technological_determinism reading is not a social problem requiring coordination; it is a material fact: hand-copying is expensive. The problem is 'live' in the technical sense (hand-copying remains expensive in principle), but the solution is not achieved through institutional design or negotiated arrangement — it is achieved by the availability of a better material process. This reading faces a mandatrophy risk: if the founding problem is redefined as 'how do we democratize access to scripture and ideas' (a social problem rather than a technical constraint), then the printing press is one solution among many (oral preaching, catechism, public reading). The mandatrophy would resolve by refocusing the constraint on the social problem (demand for democratized access) and recognizing that the press was one strategy among others that reformers and printers deployed — moving the reading toward strategic_deployment or mutual_shaping. The technological_determinism reading avoids this trap by insisting the founding problem is the material fact (reproducibility cost), not the social demand. That focus is coherent but vulnerable to the omega question about whether beneficiary presence on a mountain indicates false summitry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_framing_determinism_vs_contingency,
    'Does the printing press constitute an exogenous technological constraint (upstream of human agency) or is it itself the product of strategic choices and material contingencies that could have unfolded differently?',
    'Philosophical analysis of technological agency: is the press''s development path a technological necessity (mathematics of reproducibility, physics of ink and metal) or a contingent social choice among feasible alternatives? Counterfactual history: what would have happened if screw-presses, woodblock printing, or other reproduction technologies had taken the economic lead instead?',
    'If the press is truly exogenous (a natural-law-like fact about material efficiency), the technological_determinism reading holds and reformers are beneficiaries of exogenous capacity. If the press''s development was itself strategically directed or could have been otherwise, then the strategic_deployment or mutual_shaping readings are structurally more accurate and this reading over-claims materiality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_determinism_vs_contingency, conceptual, 'Whether the printing press is a technological mountain (exogenous material law) or a constructed artifact reflecting prior choices.').

omega_variable(
    beneficiary_causation_conflation,
    'Does identifying beneficiaries (reformation_movements, vernacular_readers) on a mountain constraint violate the semantic distinction between ''who benefits from this fact'' and ''who caused it to persist''?',
    'Definitional: mountains are defined by no beneficiary collecting rents from their operation. But this story declares beneficiaries. Either (a) the beneficiaries are present because they represent future agents exploiting a technological capacity (not collecting from the constraint''s operation but using it), or (b) the constraint is not actually a mountain because the beneficiaries'' actions are selecting for the press''s spread, making it partly constructed.',
    'If interpretation (a) is correct, the mountain designation stands and beneficiaries are identified as downstream exploiters. If interpretation (b) is correct, the constraint should be reclassified as tangled_rope (beneficiaries benefit, Church pays via monopoly loss, the arrangement is actively enforced by printer supply chains and reformer distribution networks). The FSM (false_summit_mountain) mechanism flags this omega for engine review.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_causation_conflation, conceptual, 'Whether declaring beneficiaries on a technological mountain is consistent with the mountain definition or indicates a false summit.').

omega_variable(
    censorship_impossibility_empirical_claim,
    'Was censorship actually rendered impossible by printing, or did the constraint merely raise the cost of censorship and shift its form to mass-market policing rather than copy-level control?',
    'Historical analysis of censorship post-1440: papal indexes, book burnings, printing licensing requirements, clandestine presses, underground distribution networks. If censorship persisted in effective forms (index systems, licensing, arrest of printers), then printing did not make censorship impossible — it changed censorship''s form and cost. If underground networks proliferated faster than suppression could scale, then censorship was effectively impossible at the scale demanded by textual abundance.',
    'If censorship remained effective (merely more expensive), then the accessibility_collapse metric (0.92, nearly complete barrier closure) is overstated and the mountain claim weakens — alternatives (underground copying, oral transmission, local manuscript production) remained live even post-printing. If censorship truly became impossible, the 0.92 accessibility_collapse stands as accurate and the technological_determinism reading is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_impossibility_empirical_claim, empirical, 'Whether the printing press made censorship impossible or merely more difficult.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the contested press_reformation_causation kernel. How does the technological_determinism reading''s core premise (technology as exogenous upstream cause) relate structurally to the mutual_shaping and strategic_deployment readings?',
    'Framework analysis: technological_determinism treats the press as a mountain (exogenous technological fact). mutual_shaping treats the press and Reformation movements as co-evolving — feedback loops between emerging printing capability and reformer demand shape both. strategic_deployment treats the press as neutral capacity; reformers are strategic agents who chose to exploit it. These readings contest the LOCATION of causation: in the technology itself, in the interaction, or in human agency deploying available tools.',
    'The three readings coexist as live positions held by different historians. Technological_determinism forecloses mutual_shaping''s feedback loops (if the press is exogenous, co-evolution is not possible — only downstream adaptation). It influences strategic_deployment by denying the neutrality of capacity (if the press made censorship impossible, strategic choices are constrained by a prior fact rather than freely creative). See cs_structure.reading_relations for formal relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint instantiates one reading of a contested kernel; the others are separate constraint stories linked by network edges and reading relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causation__technological_determinism, theater_ratio, 1440, 0.0).
narrative_ontology:measurement_basis(pres_tr_t1440, observed).
narrative_ontology:measurement(pres_tr_t1460, press_reformation_causation__technological_determinism, theater_ratio, 1460, 0.01).
narrative_ontology:measurement_basis(pres_tr_t1460, observed).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__technological_determinism, theater_ratio, 1480, 0.015).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1500, observed).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causation__technological_determinism, theater_ratio, 1520, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1520, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__technological_determinism, theater_ratio, 1540, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causation__technological_determinism, base_extractiveness, 1440, 0.08).
narrative_ontology:measurement_basis(pres_be_t1440, observed).
narrative_ontology:measurement(pres_be_t1460, press_reformation_causation__technological_determinism, base_extractiveness, 1460, 0.12).
narrative_ontology:measurement_basis(pres_be_t1460, observed).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__technological_determinism, base_extractiveness, 1480, 0.14).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement_basis(pres_be_t1500, observed).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causation__technological_determinism, base_extractiveness, 1520, 0.15).
narrative_ontology:measurement_basis(pres_be_t1520, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__technological_determinism, base_extractiveness, 1540, 0.15).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement_basis(pres_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causation__technological_determinism, suppression_requirement, 1440, 0.08).
narrative_ontology:measurement_basis(pres_su_t1440, observed).
narrative_ontology:measurement(pres_su_t1460, press_reformation_causation__technological_determinism, suppression_requirement, 1460, 0.08).
narrative_ontology:measurement_basis(pres_su_t1460, observed).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__technological_determinism, suppression_requirement, 1480, 0.08).
narrative_ontology:measurement_basis(pres_su_t1480, observed).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement_basis(pres_su_t1500, observed).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causation__technological_determinism, suppression_requirement, 1520, 0.08).
narrative_ontology:measurement_basis(pres_su_t1520, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__technological_determinism, suppression_requirement, 1540, 0.08).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.08).
narrative_ontology:measurement_basis(pres_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.12).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The press_reformation_causation kernel decomposes into three structurally distinct constraint stories: technological_determinism (this constraint, treating the press as exogenous mountain), strategic_deployment (treating the press as neutral tool exploited by agents), and mutual_shaping (treating the press and Reformation as co-evolving). The ε-invariance principle requires separate stories because the three readings instantiate different structural claims about causation and technology. Each has its own beneficiary/victim structure, directionality, and claimed type. They are linked via network.affects_constraints as family members. The technological_determinism reading treats the constraint as upstream mountain; the others treat it as downstream to human agency. This family structure allows corpus analysis to distinguish between technological-determinist, agency-centered, and co-evolutionary frames in historical claims about technology and social change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__technological_determinism, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
