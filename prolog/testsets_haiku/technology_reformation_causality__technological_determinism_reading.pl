% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press Production Cost Reduction (Technological Determinism Reading)
 *   domain: history_of_technology/media_studies/religious_history
 *
 * SUMMARY:
 *   The printing press reduced the per-unit cost of text reproduction from
 *   approximately 2-3 labor-hours per manuscript page to mechanical batch
 *   production, making individual book costs fall to roughly 1-2% of
 *   hand-copied equivalents by 1500. Under the technological determinism
 *   reading, this cost collapse is the *constraint*: once production costs
 *   cross a threshold, mass distribution of any text becomes technologically
 *   inevitable, regardless of institutional opposition. The reading treats
 *   the press as a mountain (a brute physical fact), reformers and their
 *   opponents as downstream adapters responding to technological possibility,
 *   and the Reformation as a consequence of technological inevitability
 *   rather than strategic choice. This reading is structurally distinct from
 *   the beneficiary_agency reading (reformers deployed printing
 *   strategically) and the co_constitution reading (technology and social
 *   actors co-evolved). The constraint's ε is low (extractiveness 0.14-0.15)
 *   because the cost reduction is treated as a pure gain with no extractive
 *   overhead—it is a coordination benefit that flows to anyone who can access
 *   the technology.
 *
 * KEY AGENTS:
 *   - printing_press_technology: The technological artifact enabling mechanical text reproduction; modeled as a mountain constraint, not as an agent with goals
 *   - reformation_theology: A vindicated proposition (vernacular scripture availability) whose material implementation becomes inevitable under this reading
 *   - reformation_printers: Moderate-power downstream adapters responding to market demand and technological possibility, not strategic agents
 *   - reformation_theologians: Powerful actors whose ability to reach mass audiences is structurally enabled by the press, not independently chosen
 *   - catholic_institutional_authority: Excluded institutional actor whose suppression effort is portrayed as futile against technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.15).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press Production Cost Reduction (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/media_studies/religious_history").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '432c367d-76fb-4062-a5a2-4c9787fdba69').
narrative_ontology:cs_kernel_codification('432c367d-76fb-4062-a5a2-4c9787fdba69', formalized).
narrative_ontology:cs_authority_grounding('432c367d-76fb-4062-a5a2-4c9787fdba69', expertise).
narrative_ontology:cs_interpretation_layer_present('432c367d-76fb-4062-a5a2-4c9787fdba69').
narrative_ontology:cs_reading_relation('432c367d-76fb-4062-a5a2-4c9787fdba69', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('432c367d-76fb-4062-a5a2-4c9787fdba69', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('432c367d-76fb-4062-a5a2-4c9787fdba69', foundational, technological_cost_reduction_determines_distribution_possibility).
narrative_ontology:cs_axiom_status(technological_cost_reduction_determines_distribution_possibility, holdable).
narrative_ontology:cs_axiom_grounding('432c367d-76fb-4062-a5a2-4c9787fdba69', technological_cost_reduction_determines_distribution_possibility, empirically_contingent).
narrative_ontology:cs_axiom('432c367d-76fb-4062-a5a2-4c9787fdba69', foundational, institutional_monopoly_technologically_unenforceable_below_cost_threshold).
narrative_ontology:cs_axiom_status(institutional_monopoly_technologically_unenforceable_below_cost_threshold, holdable).
narrative_ontology:cs_axiom_grounding('432c367d-76fb-4062-a5a2-4c9787fdba69', institutional_monopoly_technologically_unenforceable_below_cost_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('432c367d-76fb-4062-a5a2-4c9787fdba69', pre_printing_press_manuscript_scarcity_regime).
narrative_ontology:cs_drift_state('432c367d-76fb-4062-a5a2-4c9787fdba69', post_press_mass_distribution_regime, gap(stable, severe, true)).
narrative_ontology:cs_created_at('432c367d-76fb-4062-a5a2-4c9787fdba69', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, reformation_theology).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_literacy).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, scriptural_accessibility_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformation_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformation_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A physical technology enabling mechanical reproduction of text. This constraint models the technological artifact itself as a structural fact: once the press exists and production costs fall sufficiently, mass distribution of any text becomes physically possible regardless of institutional opposition.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, printing_press_technology).

% The doctrinal position that scripture should be available in vernacular languages to common believers. This is a vindicated proposition under technological determinism: the constraint makes the doctrine's material implementation inevitable, not because actors chose it, but because production costs permit it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_theology, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, reformation_theology).

% Operated printing presses and chose (or were compelled by market demand or political pressure) to print vernacular scripture. Under this reading, they are downstream adapters to the technological constraint, not strategic agents deploying printing to bypass authority. Their agency is bounded by what the press makes economically viable and by what actors demand.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_printers, payer,
    moderate, biographical, constrained, continental).

% Composed and distributed reformed doctrine. Under technological determinism, they are passengers on a wave of technological possibility, not architects of a strategy. Their ability to reach mass audiences is structurally enabled by the press, not chosen by them independently.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_theologians, payer,
    powerful, biographical, constrained, continental).

% Sought to control scriptural interpretation and suppress vernacular translation. Under this reading, their opposition is futile not because they are weak, but because the technological constraint makes mass distribution inevitable once production costs fall. Their suppression effort cannot overcome the physical fact of mechanical reproducibility.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_institutional_authority, excluded,
    institutional, generational, constrained, continental).

% The doctrine and social fact that ordinary people should read scripture themselves. Under technological determinism, this becomes inevitable not through advocacy, but through the constraint: once cheap books exist, reading literacy becomes economically rational, creating demand that the constraint supplies.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, literacy_expansion, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, literacy_expansion).

% Observes the constraint from outside: the printing press reduced the per-unit cost of text reproduction below a threshold where institutional monopolies on scripture distribution become technologically unenforceable.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, analytical_historian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function: the constraint is presented as a technological fact (production cost reduction), not as a solution to a collective-action problem that actors solved together. The distribution of vernacular scripture is treated as a consequence of technological inevitability, not as coordinated choice.
% TRANSFER_FUNCTION: No transfer in the classical sense. The constraint moves the *possibility* of text production from a scarce good (hand-copied manuscripts, controlled by institutional scribal monopolies) to an abundant good (printed books, mechanically reproducible). This is a collapse of scarcity, not a transfer between agents.
% ABSENT_VOICES: Under technological determinism, all voices are absent in the relevant sense: no actor is portrayed as strategically choosing the outcome. Catholic authorities wanted to suppress vernacular scripture but are absent from the conversation about *why* suppression failed—the reading locates the answer in technology, not in the strength or weakness of their opposition. Alternative readings (beneficiary_agency, co_constitution) would give voice to the strategic choices of reformers, printers, and even institutional authorities in shaping outcomes.
% DISAPPEARANCE_RATIONALE: Under strict technological determinism, the claim is that if the printing press had not been invented or had remained high-cost, the Reformation would not have occurred in the form it did—mass distribution of vernacular scripture would have been impossible. Disappearance of the press means return to hand-copying scarcity, which the constraint says makes institutional monopolies technologically enforceable again. However, this claim is contested: alternative readings argue that reformers would have found other channels (oral preaching, smaller-scale copying) or that the theological and political pressures driving the Reformation would have taken different forms, making the verdict not simply rearrangement but genuinely disputed across readings.
% FOUNDING_PROBLEM: The production cost of text reproduction. In the pre-press era, hand-copying was labor-intensive and created a scarcity that institutional authorities could exploit by controlling the scribal supply chain. The printing press solved this problem by making mechanical reproduction cheaper than hand labor.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (Eisenstein, Febvre & Martin) and economic historians of printing (Pettegree) attest to the dramatic cost reduction: a printed book cost 1-2% of the hand-copied equivalent by the early 16th century. This is a factual claim about technology and economics, corroborated by artifacts, price records, and production data from the period. However, the interpretation of what this cost reduction *caused* is contested—beneficiary_agency and co_constitution readings accept the cost fact but dispute the causal inevitability claim.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.14-0.15 final) because under technological determinism, the constraint produces gains without extractive overhead: lower production costs mean cheaper books, which benefits all readers and users. There is no structural asymmetry where some party captures the gain while others bear cost. Suppression is extremely low (0.07-0.10) because a physical fact (mechanical reproducibility) cannot be suppressed by institutional opposition—once the technology exists and costs fall, distribution becomes inevitable. Theater ratio is negligible (0.02 throughout) because there is no performative maintenance: the constraint operates through physics and economics, not through theatrical enforcement. Accessibility collapse is very high (0.92) because once cheap books exist, the alternative (institutional monopoly on scripture) becomes physically impossible to maintain—the collapse is total and irreversible. Resistance is extremely low (0.05) because there is nothing to resist: a technological fact is not something an actor can resist; they can only adapt to it. The measurement series show a slight decline in extractiveness and suppression over the interval, reflecting the technology's maturation and the irreversibility of the cost reduction.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces near-zero perspectival gap because technological determinism treats all actors as passive recipients of technological change. Catholic authorities and reformers occupy different roles but perceive the same constraint: an inevitable cost reduction that makes their previous strategies (controlling manuscript supply, suppressing vernacular distribution) technologically unenforceable. The beneficiary_agency and co_constitution readings would show massive perspectival divergence—reformers would describe strategic choices and institutional resistance would describe active suppression rather than technological futility. The engine computes this divergence from structural data; this reading's claim of inevitability produces metrics that leave little room for per-seat classification divergence because the constraint is claimed as a mountain, not as an extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is near-zero because under technological determinism, no actor is the target or beneficiary of an extractive mechanism. The constraint is a physical fact (production cost reduction) that distributes gains broadly. Printing press technology itself is not an agent and carries no directionality. Reformation theology and literacy expansion are vindicated propositions (non-agents) whose material implementation becomes inevitable. Reformation printers and theologians are downstream adapters with moderate-to-powerful structural positions, but they do not benefit from extraction—they benefit from technological possibility. Catholic institutional authority is excluded but not targeted; their suppression fails not because they are weak but because the constraint makes their historical strategy technologically unenforceable.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading, there is no mandatrophy in the classical sense. The constraint's founding problem (high cost of text production) is live and solved: the printing press continues to reduce production costs and enable mass distribution. However, a committer-frame omega addresses whether the technological determinism mandate itself is live or dead—whether this reading's claim that 'the press made the Reformation inevitable' remains a live explanation or has been superseded by alternative readings that grant more agency to historical actors. The constraint does not extract from its operation; it produces coordination benefits (cheaper books, broader literacy). The question is whether the technological-determinism framing of the Reformation's causality has been abandoned (mandatrophy of the explanation) while the technological fact (cost reduction) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency_mandate,
    'Is the Reformation''s occurrence structurally determined by printing-press technology, or does the printing press merely enable outcomes that required human choice and agency to actualize?',
    'Comparative historical analysis: did printing-enabled mass distribution of vernacular text automatically produce religious reform, or did reformers have to choose, strategize, and act deliberately to deploy the technology? If reform occurred in contexts where printing was available but not used for scriptural distribution (or if scriptural distribution occurred without corresponding reform), the mandate is undermined.',
    'If the printing press determines outcomes, this reading''s mountain classification holds and the constraint''s causal role is inevitable. If outcomes required deliberate choice, the beneficiary_agency and co_constitution readings better capture the constraint''s actual operation, and the technological-determinism reading overstates the technology''s causal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency_mandate, conceptual, 'Whether the constraint represents technological determination or technological enablement.').

omega_variable(
    natural_law_vs_constructed_beneficiary,
    'Is the printing press a natural-law constraint (brute physical fact independent of human valuation) or a constructed constraint whose beneficiary status derives from the humans who valued vernacular scripture and literacy?',
    'Analyze whether the constraint''s ''benefit'' (cheaper books, mass distribution) is intrinsic to the technology or dependent on human actors valuing these outcomes. A technology that produces cheap books no one wants is not a benefit; the fact that reformers and readers valued vernacular scripture is a human choice, not a technological necessity.',
    'If the benefit is intrinsic, the mountain classification and low extractiveness stand. If the benefit is human-constructed (reformers and readers chose to value what printing made possible), the constraint is a false summit—technology plus human valuation produces the outcome, not technology alone. This would trigger the FSM (false-summit-mountain) override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary, conceptual, 'Whether the constraint''s benefit is a natural-law fact or a human-constructed valuation.').

omega_variable(
    counterfactual_path_dependency,
    'If the printing press had not been invented, could the Reformation have occurred through alternative means (oral preaching, smaller-scale copying, alternative technologies)?',
    'Counterfactual historical analysis: simulate the theological and political pressures driving reform under different technological regimes. If reform could have taken different forms without printing (e.g., via monastic reform movements, itinerant preaching, or eventually competing printing technologies), the constraint''s role is enabler, not determiner.',
    'If alternative paths existed, the constraint is enabler rather than determiner—the beneficiary_agency and co_constitution readings better capture contingency and choice. If the constraint was the unique path to mass distribution, the technological-determinism reading''s inevitability claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_path_dependency, empirical, 'Whether printing was the unique technological path to reformation or one of multiple possible paths.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Catholic institutional opposition to vernacular scripture was suppressed by technological inevitability (no active suppression mechanism needed—the press made their strategy technologically unenforceable) or by political and social forces that actively suppressed institutional opposition?',
    'Analyze the suppression_requirement trajectory: if it remains low (~0.07) throughout, the reading''s claim that technology made suppression unnecessary holds. If it rises over the interval, institutional actors had to actively suppress opposition, suggesting technology did not automatically render the institution''s strategy unenforceable.',
    'If suppression is truly minimal, the mountain reading holds. If suppression rises, the constraint requires active enforcement of technological outcomes, which indicates extraction and narrows the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of opposition is structural or actively maintained.').

omega_variable(
    beneficiary_identity_actor_vs_proposition,
    'Are ''reformation theology'' and ''vernacular literacy doctrine'' genuine beneficiaries (agents collecting rents from the constraint) or vindicated propositions (doctrines whose validity is asserted by the constraint''s operation)?',
    'Assess whether these entities collected benefit in the sense of extracting resources or controlling distribution. A vindicated proposition is one whose truth is asserted by events (if vernacular scripture is now widely available, the proposition ''vernacular scripture should be available'' is vindicated). A beneficiary collects rents or controls access. These entities did neither—they are propositions whose material implementation became possible.',
    'If these are vindicated propositions (not beneficiaries), the constraint has no classical beneficiary seats, which is consistent with a pure-coordination mountain reading. If they are treated as beneficiaries, the constraint''s structure becomes more like a Rope (coordination with identifiable parties benefiting), which would shift the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identity_actor_vs_proposition, conceptual, 'Whether the named entities are vindicated propositions or agent beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1440, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1440, projected).
narrative_ontology:measurement(tech_tr_t1460, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1460, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1460, observed).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1480, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1480, observed).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1500, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1500, observed).
narrative_ontology:measurement(tech_tr_t1520, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1520, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1520, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1550, 0.02).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1440, 0.18).
narrative_ontology:measurement_basis(tech_be_t1440, projected).
narrative_ontology:measurement(tech_be_t1460, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1460, 0.17).
narrative_ontology:measurement_basis(tech_be_t1460, observed).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1480, 0.16).
narrative_ontology:measurement_basis(tech_be_t1480, observed).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement_basis(tech_be_t1500, observed).
narrative_ontology:measurement(tech_be_t1520, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1520, 0.14).
narrative_ontology:measurement_basis(tech_be_t1520, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1550, 0.14).
narrative_ontology:measurement_basis(tech_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1440, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1440, 0.1).
narrative_ontology:measurement_basis(tech_su_t1440, projected).
narrative_ontology:measurement(tech_su_t1460, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1460, 0.09).
narrative_ontology:measurement_basis(tech_su_t1460, observed).
narrative_ontology:measurement(tech_su_t1480, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1480, 0.08).
narrative_ontology:measurement_basis(tech_su_t1480, observed).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement_basis(tech_su_t1500, observed).
narrative_ontology:measurement(tech_su_t1520, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1520, 0.07).
narrative_ontology:measurement_basis(tech_su_t1520, observed).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1550, 0.07).
narrative_ontology:measurement_basis(tech_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, global_infrastructure).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel technology_reformation_causality. Three readings are authored as separate constraints: (1) technological_determinism_reading (this file) treats the printing press as a mountain whose cost reduction made Reformation inevitable; (2) beneficiary_agency_reading treats the press as a tool strategically deployed by reformers and printers to bypass Church authority; (3) co_constitution_reading treats technology and social actors as co-evolving, with neither determining outcomes independently. The three readings have structurally distinct ε values, beneficiary/victim declarations, and causal framings. Each reading is ε-invariant within itself but represents a different structural interpretation of the same historical event. The three constraints form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
