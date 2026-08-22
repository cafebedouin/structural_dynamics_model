% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary — Conceptualization Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This reading draws the emergence boundary of digital money at the moment
 *   it becomes theoretically thinkable: 1960s telecommunications advances
 *   (packet switching, early encryption) make electronic value representation
 *   conceivable, and Chaum's 1985 formalization of blind signatures provides
 *   the cryptographic primitive for untraceable digital cash. The constraint
 *   is a scaffold — it coordinates a research community around a shared
 *   founding problem (cryptographic user sovereignty vs. institutional
 *   intermediation) with an implicit sunset: once deployed systems arrive
 *   (DigiCash 1990, e-purses, Bitcoin 2009), the theoretical boundary's
 *   coordinating function is complete and it should yield to operational
 *   definitions. The beneficiary is the academic cryptography community that
 *   establishes priority claims through this boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.12).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.08).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, scaffold).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:has_sunset_clause(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'bc9f2088-23f5-4762-9202-e4dc0f49cd54').
narrative_ontology:cs_kernel_codification('bc9f2088-23f5-4762-9202-e4dc0f49cd54', distributed).
narrative_ontology:cs_authority_grounding('bc9f2088-23f5-4762-9202-e4dc0f49cd54', practice).
narrative_ontology:cs_reading_relation('bc9f2088-23f5-4762-9202-e4dc0f49cd54', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc9f2088-23f5-4762-9202-e4dc0f49cd54', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('bc9f2088-23f5-4762-9202-e4dc0f49cd54', foundational, cryptographic_untraceability_defines_digital_money).
narrative_ontology:cs_axiom_status(cryptographic_untraceability_defines_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('bc9f2088-23f5-4762-9202-e4dc0f49cd54', cryptographic_untraceability_defines_digital_money, deontological).
narrative_ontology:cs_axiom('bc9f2088-23f5-4762-9202-e4dc0f49cd54', secondary, theoretical_priority_establishes_field_legitimacy).
narrative_ontology:cs_axiom_status(theoretical_priority_establishes_field_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bc9f2088-23f5-4762-9202-e4dc0f49cd54', theoretical_priority_establishes_field_legitimacy, conventional).
narrative_ontology:cs_reference_frame('bc9f2088-23f5-4762-9202-e4dc0f49cd54', cryptographic_money_origin_narrative).
narrative_ontology:cs_drift_state('bc9f2088-23f5-4762-9202-e4dc0f49cd54', post_bitcoin_genesis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc9f2088-23f5-4762-9202-e4dc0f49cd54', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, central_bank_research_divisions).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, early_digital_cash_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, telecommunications_standard_bodies).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, cryptographic_money_feasibility).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, telecommunications_enables_monetary_innovation).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, chaum_blind_signature_scheme).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes theoretical priority through publications (Chaum 1983, 1985) and conference presentations. Gains academic recognition, citation capital, and research funding. Can exit to other research domains without career penalty. The constraint's boundary draws the map of their field.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community, agenda_setter).

% Funds and monitors theoretical work on digital cash (e.g., Bank of Finland's Avant project precursors, ECB exploratory work). Benefits from early-warning intelligence on monetary technology. No structural extraction — they hold the monetary authority that would later regulate or deploy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_research_divisions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, central_bank_research_divisions, observer).

% Directly build the prototypes (DigiCash, Mondex pre-patents, academic testbeds). Their careers are tied to the field's legitimacy — exit means abandoning a research identity built on this boundary. Collect grant money and commercial options but bear reputational risk if the boundary proves vacuous.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, early_digital_cash_researchers, agenda_setter,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, early_digital_cash_researchers, beneficiary).

% Produce the 1960s–1980s network advances (packet switching, X.25, early encryption standards) that make the theoretical boundary thinkable. Benefit from expanded mandate — 'money' becomes a new application domain for their infrastructure. Can pivot to other application domains.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, telecommunications_standard_bodies, beneficiary,
    organized, generational, arbitrage, global).

% Operate the existing electronic transfer infrastructure (ACH, SWIFT, ATM networks) but are not consulted on the theoretical boundary. Would object to a boundary that makes their deployed systems 'not digital money' — their exclusion is structural: the academic reading precedes and does not incorporate operational reality.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, commercial_bank_payments_divisions, excluded,
    institutional, biographical, trapped, national).

% Later scholars who must choose which boundary to adopt for periodization. Their work is shaped by the boundary but they do not collect from it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed research community around a shared definition of 'digital money' that enables cumulative theoretical progress — cryptographic primitives, blind signatures, untraceability proofs — without requiring deployed infrastructure.
% TRANSFER_FUNCTION: Moves academic recognition, citation priority, and early research funding toward the cryptography community that establishes the theoretical boundary. No monetary value transfers yet — the extraction is reputational and field-defining.
% ABSENT_VOICES: Operational payments engineers (ACH, SWIFT, ATM network operators) and commercial bank technology divisions who had already built working electronic transfer systems by the 1970s. They are absent because the boundary is drawn by theorists who define 'digital money' as cryptographic rather than institutional-electronic. Their objection — that digital money existed in production decades earlier — is structurally excluded by the reading's definitional choice.
% DISAPPEARANCE_RATIONALE: If this conceptual boundary vanished, the field of cryptographic digital cash would lose its founding moment and priority map. Chaum's 1985 paper would become one of many cryptographic protocols rather than THE origin of digital money. Research funding narratives, patent lineages, and the 'pre-history' of Bitcoin would reorganize around different antecedents (e.g., institutional electronic transfer). The academic field's self-understanding depends on this boundary.
% FOUNDING_PROBLEM: How to define 'digital money' as a distinct theoretical object separable from existing electronic fund transfers, such that cryptographic properties (untraceability, user sovereignty, no trusted intermediary) become the defining criteria rather than mere electronic representation.
% FOUNDING_PROBLEM_CORROBORATION: Chaum and the academic cryptography community attest the problem is live — cryptographic user sovereignty remains unrealized in deployed systems. Central bank digital currency (CBDC) architects and payments industry historians attest the problem is substantially solved or misposed — electronic money has existed since the 1970s in production systems, and the cryptographic criteria are a research preference, not a monetary necessity. No consensus outside the benefiting academic community.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.12) because the constraint operates primarily as a field-defining coordination mechanism, not a rent-extraction device — the 'extraction' is reputational priority within academia. Suppression is minimal (0.08) — the boundary excludes operational payments engineers by definition, not by active coercion. Theater ratio (0.25) reflects that the boundary increasingly performs 'origin story' work for later cryptocurrency narratives while its coordinating function wanes. Accessibility collapse (0.75) is moderately high because once the cryptographic definition is accepted, alternative boundaries (institutional-electronic) are marginalized in academic discourse. Resistance (0.15) is low because the excluded parties (bank payments divisions) operate in a different institutional universe and do not contest the academic boundary directly.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat, this is a genuine coordination scaffold: it solves the 'what counts as digital money' problem for a research field. From the excluded operational seat, it is a definitional snare that erases decades of production electronic money. The engine will compute this divergence from the structural data — the academic beneficiaries have mobile exit and organized power, while the excluded have trapped exit and institutional power but no voice in the boundary-setting.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic cryptography community and early researchers are beneficiaries (d ~ 0.15–0.25) — they collect priority, citations, funding. Central bank research divisions are near-symmetric beneficiaries (d ~ 0.35) — they gain intelligence without paying costs. Commercial bank payments divisions are excluded (d ~ 0.8) — the boundary renders their deployed systems 'not digital money' by definitional fiat. Telecommunications standard bodies are beneficiaries (d ~ 0.2) — expanded application domain. The constraint's directionality is unusually benign for a scaffold because no deployed monetary value is at stake yet.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (defining digital money cryptographically) remains contested — CBDC architects argue electronic money existed long before Chaum; cryptocurrency advocates treat Chaum as the true origin. The sunset clause is implicit: the boundary was meant to expire when deployed cryptographic cash arrived (DigiCash 1990), but it persists as a 'pre-history' narrative for Bitcoin. This is mandatrophy — the coordinating function completed, but the boundary remains because it now serves as an origin myth for a later extraction apparatus (cryptocurrency). The mandate has outlived its function but persists through narrative capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theoretical_vs_operational_boundary,
    'Is the conceptualization boundary a genuine coordination scaffold for a research field, or a retroactive origin myth constructed to legitimize later cryptocurrency extraction?',
    'Trace citation networks and funding narratives from 1985–2009: if the boundary actively coordinated research before Bitcoin, it is a scaffold; if it was dormant and only activated post-2009 as Bitcoin''s ''pre-history,'' it is a constructed myth.',
    'If scaffold, the constraint''s low extraction is genuine coordination. If myth, the boundary is a piton — theatrical maintenance of an origin story that serves later extractive systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theoretical_vs_operational_boundary, empirical, 'Whether the boundary''s coordinating function was live during its interval or constructed retroactively.').

omega_variable(
    exclusion_mechanism_ambiguity,
    'Does the boundary''s exclusion of operational electronic money (ACH, SWIFT, ATM) reflect a genuine theoretical distinction (cryptographic vs. institutional) or a disciplinary boundary-drawing that serves academic priority?',
    'Compare the 1980s cryptographic literature''s engagement with existing payment systems: if they seriously engaged and rejected institutional models, the distinction is theoretical; if they ignored them, the exclusion is disciplinary.',
    'If theoretical, the boundary is a valid coordination tool. If disciplinary, the exclusion is a suppression mechanism that erases prior art to establish priority — raising effective suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, conceptual, 'Whether the exclusion of operational systems is a theoretical necessity or a priority-claiming strategy.').

omega_variable(
    committer_framing_kernel_digital_money_emergence_boundary,
    'How does this reading''s structural relationship to its sibling readings (infrastructure_reading, consumer_holdings_reading) affect the classification of the kernel as a whole?',
    'The engine computes per-reading classifications and their structural relations. This omega records the committer frame: this reading is the earliest boundary, includes theoretical concepts and research prototypes, and its beneficiary is the academic/research community establishing priority claims. The sibling readings draw later boundaries with different beneficiaries.',
    'If all three readings compute as different constraint types, the kernel is a site of genuine structural contestation, not mere semantic disagreement. The classification divergence across readings is the signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_digital_money_emergence_boundary, conceptual, 'Committee-frame record: this constraint is the conceptualization_reading of kernel digital_money_emergence_boundary; siblings are infrastructure_reading and consumer_holdings_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_conceptualization_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(dm_conceptualization_tr_t1972, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1972, 0.12).
narrative_ontology:measurement(dm_conceptualization_tr_t1983, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1983, 0.18).
narrative_ontology:measurement(dm_conceptualization_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(dm_conceptualization_tr_t1990, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(dm_conceptualization_tr_t1995, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1995, 0.25).

% Extraction over time
narrative_ontology:measurement(dm_conceptualization_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(dm_conceptualization_be_t1972, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1972, 0.03).
narrative_ontology:measurement(dm_conceptualization_be_t1983, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1983, 0.08).
narrative_ontology:measurement(dm_conceptualization_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.1).
narrative_ontology:measurement(dm_conceptualization_be_t1990, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(dm_conceptualization_be_t1995, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1995, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(dm_conceptualization_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(dm_conceptualization_su_t1972, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1972, 0.03).
narrative_ontology:measurement(dm_conceptualization_su_t1983, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1983, 0.06).
narrative_ontology:measurement(dm_conceptualization_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.07).
narrative_ontology:measurement(dm_conceptualization_su_t1990, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(dm_conceptualization_su_t1995, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1995, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.02).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'digital money emergence' into three structurally distinct boundaries per the ε-invariance principle. The conceptualization reading (this story) has ε=0.12 (low, coordination-dominant). The infrastructure reading will have higher ε (institutional rent extraction via deployed networks). The consumer holdings reading will have intermediate ε (consumer-facing extraction via e-purse fees). Each reading gets its own ε, stakeholders, and classification — linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__conceptualization_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
