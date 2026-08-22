% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence (Conceptualization Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   The conceptualization reading of digital money emergence places the
 *   boundary at 1960s telecommunications advances and especially at Chaum's
 *   1985 cryptographic formalization of electronic cash. This reading
 *   privileges theoretical feasibility and the moment when digital bearer
 *   instruments became mathematically grounded. The constraint's operation is
 *   the coordination around this boundary-setting itself: academic
 *   researchers, research institutions, and subsequent technology developers
 *   all organize around the canonical narrative that digital money emerged as
 *   a theoretical artifact in the 1980s. Beneficiaries are those who
 *   established and benefit from the intellectual priority claim. The reading
 *   deliberately excludes infrastructure milestones (ATMs, ACH) and consumer
 *   adoption (e-purses, digital wallets) — it is not claiming those did not
 *   matter, only that 'emergence' should be dated to conceptualization, not
 *   deployment. This is a committer-axis constraint: the kernel (digital
 *   money) is contested; different readings assign the boundary differently;
 *   this JSON instantiates exactly one reading and names the others as
 *   siblings.
 *
 * KEY AGENTS:
 *   - academic_cryptographers: establish and benefit from theoretical priority; d near 0.2 (beneficiary, mobile exit)
 *   - research_institutions: institutional beneficiaries; d near 0.3 (institutional power, mobile resources)
 *   - central_banks, commercial_banks: observers; d near 0.5 (institutional power, analytical exit; their interests are noted but not coordinated by this constraint)
 *   - technology_industry, consumers, regulators: excluded; d varies (industry and regulators would participate later; consumers never directly benefit from theoretical boundary-setting)
 *   - analytical_observer_seat: the Deferential Realism engine itself reads the structure; d = 0.5 (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.31).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.12).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence (Conceptualization Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '002e447f-b9f1-4102-8717-d1a8abaad06d').
narrative_ontology:cs_kernel_codification('002e447f-b9f1-4102-8717-d1a8abaad06d', formalized).
narrative_ontology:cs_authority_grounding('002e447f-b9f1-4102-8717-d1a8abaad06d', expertise).
narrative_ontology:cs_interpretation_layer_present('002e447f-b9f1-4102-8717-d1a8abaad06d').
narrative_ontology:cs_reading_relation('002e447f-b9f1-4102-8717-d1a8abaad06d', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('002e447f-b9f1-4102-8717-d1a8abaad06d', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('002e447f-b9f1-4102-8717-d1a8abaad06d', foundational, theoretical_feasibility_establishes_emergence).
narrative_ontology:cs_axiom_status(theoretical_feasibility_establishes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('002e447f-b9f1-4102-8717-d1a8abaad06d', theoretical_feasibility_establishes_emergence, deontological).
narrative_ontology:cs_axiom('002e447f-b9f1-4102-8717-d1a8abaad06d', foundational, cryptographic_mathematics_is_sufficient_boundary).
narrative_ontology:cs_axiom_status(cryptographic_mathematics_is_sufficient_boundary, holdable).
narrative_ontology:cs_axiom_grounding('002e447f-b9f1-4102-8717-d1a8abaad06d', cryptographic_mathematics_is_sufficient_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('002e447f-b9f1-4102-8717-d1a8abaad06d', cryptographic_feasibility_as_origin).
narrative_ontology:cs_drift_state('002e447f-b9f1-4102-8717-d1a8abaad06d', contemporary_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('002e447f-b9f1-4102-8717-d1a8abaad06d', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, research_institutions).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, priority_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish theoretical priority and intellectual authority over digital money concepts. Benefit from publication venues, citation counts, and subsequent technological attribution (Chaum's 1985 formalization becomes the canonical origin point). Can move between institutions and research domains; the theoretical work itself is portable.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers, beneficiary,
    moderate, biographical, mobile, global).

% Host the formalization work, accrue prestige from foundational contributions to digital money theory. Fund cryptography research. Can shift research agendas and attract grant funding based on the recognized importance of their early work.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, research_institutions, beneficiary,
    institutional, generational, mobile, global).

% Watch the theoretical development unfold. Recognize that digital money concepts originating in academia (rather than in their own labs or commercial banks) reshape policy discussion about monetary innovation and sovereign currency.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_banks, observer,
    institutional, generational, analytical, national).

% Observe the theoretical work with mixed interest. Early academic formalization establishes that digital bearer instruments are feasible, which opens strategic questions about payment system disruption and competitive response.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, commercial_banks, observer,
    institutional, generational, analytical, global).

% Not yet represented in the theoretical boundary-setting. Later, when digital money moves to consumer holdings and infrastructure phases, technology companies will have strong claims to participate (payment integration, wallet design, protocol implementation). Their absence from the conceptualization phase means their interests are not reflected in which theoretical framings become canonical.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, technology_industry, excluded,
    organized, biographical, constrained, global).

% Not yet participants. The conceptualization reading defines digital money in academic and theoretical terms, without consumer input into what they will want or what practical constraints matter.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, consumers, excluded,
    powerless, biographical, trapped, global).

% Monitor the theoretical development as it becomes clear that digital bearer instruments are not hypothetical but mathematically grounded. Will later need to decide how to regulate or respond, but at the conceptualization boundary they are spectators to academic priority-setting.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared theoretical framework and canonical reference point for understanding digital money: Chaum's 1985 formalization becomes the recognized origin, coordinating how the financial world dates and attributes the innovation. Creates a focal point for subsequent research, patent claims, and policy discussion.
% TRANSFER_FUNCTION: Moves intellectual priority and attributed authorship from distributed experimental and conceptual work toward a concentrated canonical formalization. The benefit accrues to the researchers and institutions that crystallize the concept into publishable, citable form.
% ABSENT_VOICES: Technology companies that will later implement digital money systems are not present. Central banks and regulators are observers, not participants in boundary-setting. Consumers have no voice in what 'digital money' is theoretically defined to mean. Commercial banks are excluded from the research community setting the terms.
% DISAPPEARANCE_RATIONALE: If this theoretical boundary-setting were absent, the subsequent infrastructure and consumer holdings readings would still happen — ATMs, ACH, e-purses would still emerge. But the LINE DRAWN (when digital money is said to have emerged) would shift. Without the 1960s-1980s conceptualization reading, later boundary-setters would adopt infrastructure_reading or consumer_holdings_reading as the canonical origin, reassigning intellectual priority. The world's technical and financial capabilities rearrange little; the attribution and historical narrative would reorganize substantially.
% FOUNDING_PROBLEM: How can bearer instruments (bearer bonds, cash) be replicated in electronic form using cryptographic mathematics rather than physical possession? The problem: once information is digital and copyable, what mechanism prevents double-spending?
% FOUNDING_PROBLEM_CORROBORATION: Cryptographic researchers from outside the initial Chaum circle (Merkle, Diffie, Hellman on public-key foundations; later Szabo, Back on computational work) attest that the problem was genuinely hard and not trivial to solve. Later blockchain and CBDC research independently rediscovers and re-solves variants of the same double-spending problem, confirming the founding problem's realness.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).
:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.31 at interval end) because the benefit is intellectual priority and prestige rather than direct economic rent. The beneficiaries (academic researchers and institutions) accrue citation advantage and research funding, which is real but not equivalent to extracting value from operational users. Suppression is low (0.12) because the theoretical framing is not actively defended against alternatives — it coexists with infrastructure and consumer readings in different institutional contexts. Theater ratio is near-minimal (0.08) because the performance is minimal: the constraint is mostly what it claims (coordinating a canonical boundary-setting), not theatrical maintenance of something else. Accessibility collapse is moderate (0.45) because once the Chaum formalization is published and recognized, the theoretical boundary becomes a focal point that is hard to ignore — but it remains a matter of contested interpretation (hence not complete collapse). Resistance is low (0.22) because the academic community is not actively resisting the boundary — the resistance comes from adherents of infrastructure or consumer readings, who are not in the room when this reading's framework is set. The measurement series shows extractiveness and suppression rising as the formalization crystallizes (1985 peak) then plateauing (1985-1990), because the theoretical work is published and the boundary-setting is complete; later phases (infrastructure deployment, consumer adoption) are not part of this interval or reading.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (technology_industry, excluded from boundary-setting at time=1985) and observer seats (central_banks, regulators) will later bear costs when they must navigate the crystallized theoretical boundary — infrastructure and consumer adoption phases will be shaped by this reading's canonical framing. The beneficiary seats (researchers, institutions) collect in the present (citations, prestige, funding) and are insulated from later costs by their mobile exit (they move on to other work; they do not maintain the boundary). This asymmetry — beneficiaries collect early and leave; payers and observers navigate the consequences later — should produce seat divergence in how the constraint is classified. From the researcher seat, it is coordination; from the infrastructure-builder seat (future), it is extraction. The engine computes this from directionality and power; I am declaring the structural relationship here.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic_cryptographers are beneficiaries (d ≈ 0.2): they establish priority, accrue citations and prestige, and have mobile exit (can publish in other venues, move to other research domains). Research_institutions are beneficiaries (d ≈ 0.25): they host the work and gain prestige; their exit is somewhat mobile (can shift research focus, but institutional identity is partly tied to the breakthrough). Central_banks, commercial_banks, and regulators are observers (d ≈ 0.5): they have institutional power and analytical exit, but they are not directly coordinated by this reading's boundary-setting — they observe it and respond later. Technology_industry is excluded (d ≈ 0.6, moving toward target): they have organized power and significant mobile exit (can develop infrastructure without academic blessing), but they are not present to shape the theoretical boundary-setting. Consumers are excluded and powerless (d ≈ 0.8, deep target): they will later be affected by the boundary (if digital money is dated to theory, policy and technology development follow that attribution), but they have zero input now and cannot exit the exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT experiencing mandatrophy — the founding problem (how to make electronic bearer instruments) remains live, the theoretical solution (Chaum formalization) is still canonical, and the boundary-setting is not obsolete. However, the reading IS contested: infrastructure_reading and consumer_holdings_reading place the boundary elsewhere, and technology development follows one or multiple boundaries simultaneously. The question is not whether the founding problem has died (it hasn't), but whether this READING'S boundary has been superseded. The measurement data shows extractiveness and suppression plateauing after 1985, which is consistent with a boundary-setting that is complete and stable, not with degradation. If, by 2010, infrastructure or consumer readings had completely displaced the conceptualization reading (no one cited Chaum, no one dated digital money to theory), that would signal mandatrophy. Current state: alive but contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theoretical_vs_practical_boundary,
    'Should ''emergence'' be dated to when digital money became theoretically grounded (this reading) or when it became practically deployed and used (infrastructure and consumer readings)?',
    'Normative assessment of what ''emergence'' means in financial history and technology governance. What do economists and policy makers use as the canonical boundary when they cite the origin of digital money? Track citations and policy references.',
    'If ''emergence'' is established as a theoretical concept, Chaum formalization becomes the canonical origin, research institutions gain prestige, and infrastructure/consumer readings become subordinate historical developments. If emergence requires practical deployment, infrastructure_reading or consumer_holdings_reading become primary, and Chaum''s work becomes ''necessary but not sufficient'' — a conceptual precondition, not the boundary itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theoretical_vs_practical_boundary, conceptual, 'Whether digital money''s emergence boundary is theoretical or practical.').

omega_variable(
    academic_priority_vs_market_emergence,
    'Does the academic research community''s formalization of digital money establish a legitimate canonical boundary, or does that boundary-setting merely reflect academic institutional interests in claiming priority?',
    'Examine whether Chaum''s formalization accelerated practical development (did practitioners cite it and build on it?) or whether it was discovered/validated retroactively after infrastructure and markets emerged independently. If practitioners independently converged on similar cryptographic solutions, the priority claim is weaker.',
    'If formalization was prerequisite and practitioners built on it, the academic boundary-setting is genuine coordination; academic researchers are authentic beneficiaries of a coordination mechanism they helped create. If practitioners converged independently, the academic boundary-setting is extractive (claiming priority for something that would have happened anyway, via different institutional routes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(academic_priority_vs_market_emergence, empirical, 'Whether Chaum formalization was necessary or merely prior.').

omega_variable(
    committer_contest_unresolvability,
    'Is the kernel digital_money_emergence_boundary genuinely under-determined by facts (such that multiple readings are permanently sustainable), or will evidence eventually resolve which reading is ''correct''?',
    'The answer is structural, not empirical: ''emergence'' is not a natural-kind boundary but a human category that serves different institutional functions (research priority, policy jurisdiction, market timing). Academic/finance/policy communities will never agree on one boundary because they use emergence differently. The contest is preference-unresolvable, not empirically resolvable.',
    'If the contest is structurally unresolvable, the three readings will persist indefinitely as live options, each coordinating a different community. If one reading becomes canonical across all communities, the others become historical footnotes. Current state: all three remain live in their respective domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_contest_unresolvability, preference, 'Whether the digital money emergence boundary is empirically or structurally contested.').

omega_variable(
    fsm_candidate_academic_vindication,
    'Is the beneficiary status of academic_cryptographers and research_institutions a natural feature of priority-setting (coordination benefit), or is it a constructed institutional arrangement that benefits from claiming the concept as natural scholarly advancement?',
    'Post-exit test: if a researcher leaves academia for industry, does the intellectual priority and prestige travel with them or remain institutional? If researchers can arbitrage the prestige (move between academic and industry careers, carry the credit), the benefit is portable and coordination-flavored. If prestige is locked in institutions, it is more extractive (the institution captures benefit regardless of individual career).',
    'This is a false-summit-mountain candidate: if the theoretical boundary-setting is presented as inevitable scholarly progress (a natural law of how knowledge develops) but beneficiaries are identifiable (researchers, institutions), it may be FSM-reclassifiable from rope/mountain to tangled_rope if enforcement to exclude alternative readings is discovered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_candidate_academic_vindication, empirical, 'Whether academic priority-setting in digital money is natural advancement or constructed extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmeb_concept_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.02).
narrative_ontology:measurement(dmeb_concept_tr_t1968, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1968, 0.04).
narrative_ontology:measurement(dmeb_concept_tr_t1976, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1976, 0.06).
narrative_ontology:measurement(dmeb_concept_tr_t1983, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1983, 0.07).
narrative_ontology:measurement(dmeb_concept_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(dmeb_concept_tr_t1990, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1990, 0.08).

% Extraction over time
narrative_ontology:measurement(dmeb_concept_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(dmeb_concept_be_t1968, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1968, 0.12).
narrative_ontology:measurement(dmeb_concept_be_t1976, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement(dmeb_concept_be_t1983, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1983, 0.26).
narrative_ontology:measurement(dmeb_concept_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(dmeb_concept_be_t1990, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1990, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(dmeb_concept_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(dmeb_concept_su_t1968, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1968, 0.04).
narrative_ontology:measurement(dmeb_concept_su_t1976, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1976, 0.07).
narrative_ontology:measurement(dmeb_concept_su_t1983, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1983, 0.09).
narrative_ontology:measurement(dmeb_concept_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.12).
narrative_ontology:measurement(dmeb_concept_su_t1990, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1990, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.04).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% digital_money_emergence_boundary is a contested kernel with three coexisting readings: conceptualization_reading (this file, Chaum 1985), infrastructure_reading (ATM/ACH/SWIFT 1967-1977), and consumer_holdings_reading (e-purses/EMD 1990-2000). Each reading instantiates a different constraint with different beneficiaries, extraction profiles, and enforcement structures. They coexist because different communities (academic cryptographers, financial infrastructure operators, digital money regulators, consumers) use 'emergence' to mean different things. The readings are related via network.affects_constraints (this reading influences the others by establishing theoretical precedent); reading_relations in cs_structure declare the logical relationships (coexists_with for all three, as they are live competing framings within different institutional domains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__conceptualization_reading, powerless, 0.75).
constraint_indexing:directionality_override(digital_money_emergence_boundary__conceptualization_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
