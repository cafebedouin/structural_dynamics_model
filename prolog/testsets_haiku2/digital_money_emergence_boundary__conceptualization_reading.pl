% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Digital Money Conceptualization Boundary (Academic Priority-Claim Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel 'digital
 *   money emergence boundary.' The reading places emergence at theoretical
 *   conceptualization: when telecommunications advances (1960s) and Chaum's
 *   cryptographic formalization (1985) made digital money *thinkable* as a
 *   coherent category, distinct from prior electronic payment infrastructure.
 *   The beneficiaries are academics and cryptographers who claim priority for
 *   the invention; the payers are infrastructure operators (banks, telcos,
 *   ACH networks) whose prior work is reframed as precursor rather than the
 *   thing itself. Central banks are agenda-setters, retaining authority to
 *   accept or reject this boundary. The constraint is a rope from the
 *   academic seat (genuine coordination problem solved: establishing a
 *   coherent conceptual frame for digital authentication of value). From
 *   infrastructure operators' seat, it functions as extraction (authority to
 *   define their own work is transferred to academics without enabling new
 *   capability). The claim and metrics diverge intentionally:
 *   claimed_type=rope (the coordination story academics tell), but measured
 *   extraction=0.42 and rising theater_ratio (the infrastructure
 *   perspective).
 *
 * KEY AGENTS:
 *   - academic_cryptographers: beneficiary institutional powerful seat — establish priority claim through formalization
 *   - protocol_researchers: beneficiary institutional powerful seat — stake claims on payment cryptography invention
 *   - infrastructure_operators: payer institutional constrained seat — retroactively defined as precursor
 *   - central_banks: agenda_setter institutional arbitrage seat — retain authority to adjudicate the boundary
 *   - legal_financial_regulators: observer institutional analytical seat — use the boundary for regulatory classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.42).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.28).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Conceptualization Boundary (Academic Priority-Claim Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'ab0b2f97-1f71-42e9-ab15-a07c827a84b7').
narrative_ontology:cs_kernel_codification('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', distributed).
narrative_ontology:cs_authority_grounding('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', distributed).
narrative_ontology:cs_reading_relation('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', foundational, digital_money_requires_cryptographic_formalization).
narrative_ontology:cs_axiom_status(digital_money_requires_cryptographic_formalization, holdable).
narrative_ontology:cs_axiom_grounding('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', digital_money_requires_cryptographic_formalization, empirically_contingent).
narrative_ontology:cs_axiom('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', foundational, theoretical_coherence_constitutes_emergence).
narrative_ontology:cs_axiom_status(theoretical_coherence_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', theoretical_coherence_constitutes_emergence, deontological).
narrative_ontology:cs_reference_frame('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', infrastructure_substrate_agnosticism_technical_possibility).
narrative_ontology:cs_drift_state('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', regulatory_adoption_era_2010_onward, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab0b2f97-1f71-42e9-ab15-a07c827a84b7', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, protocol_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_operators).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, money_is_substrate_independent).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, cryptographic_authentication_sufficient_for_value_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established intellectual priority through publications and formalization (Chaum 1985 onward). The conceptualization boundary secures their claim to have invented the category 'digital money' at the moment it became theoretically coherent. This boundary determines whether their intellectual work is foundational or merely incremental to prior infrastructure. Career advancement, citations, and field prestige ride on the priority date.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers, beneficiary,
    institutional, generational, mobile, global).

% Include academics and technologists who stake claims on the invention of payment cryptography and decentralized ledger theory. The conceptualization boundary validates their lineage of ideas as 'first formalization.' Funding, academic recognition, and patent priority rest partly on this date.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, protocol_researchers, beneficiary,
    powerful, generational, mobile, global).

% Banks, telecom operators, and ACH networks had built electronic transfer systems (ATMs, SWIFT, ACH) without framing them as 'digital money.' The academic conceptualization boundary retroactively redefines their infrastructure as a precursor or inferior form rather than the thing itself. This constrains their historical authority over the definition and may imply their systems were less revolutionary than they claimed.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_operators, payer,
    institutional, generational, constrained, global).

% Maintain monetary authority by controlling how 'money' is defined in legal and regulatory frameworks. The conceptualization boundary forces them to either adopt the academic definition (ceding authority over the category to technologists) or dispute it (asserting that money is what central banks say it is). They enforce and adjudicate which boundary holds in their jurisdiction.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_banks, agenda_setter,
    institutional, generational, arbitrage, national).

% Need a working definition of 'digital money' to apply existing money-transmission and banking regulations. The conceptualization boundary determines what regulatory category digital assets fall into. Early adoption of the academic definition accelerates regulatory response; delay preserves ambiguity.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, legal_financial_regulators, observer,
    institutional, generational, analytical, national).

% Historians of banking and telecommunications infrastructure would argue that ATMs (1967), ACH (1972), and SWIFT (1977) constitute electronic money systems and that the conceptualization boundary misappropriates credit. They are excluded from the academic priority-claim conversation but their perspective would fundamentally contest the boundary's placement.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_continuity_historians, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_cryptographers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared interpretive frame for what constitutes 'digital money' as a theoretical category, enabling academic discourse, patent coherence, and downstream regulatory classification. Solves the definitional coordination problem: without a conceptual boundary, claims about innovation cannot be distinguished from pre-existing infrastructure.
% TRANSFER_FUNCTION: Transfers intellectual priority and definitional authority from infrastructure operators (who built electronic systems) to academic cryptographers (who formalized payment cryptography). The boundary moves prestige, citation priority, and claim-staking authority from engineering to mathematics.
% ABSENT_VOICES: Infrastructure historians and legacy banking technologists who would argue the boundary misplaces priority. Postal savings systems (Japan, Korea) and non-Western monetary traditions (hawala, token systems) are absent from the academic framing. Consumer voices asking 'what makes this money?' are excluded in favor of technical definitions.
% DISAPPEARANCE_RATIONALE: If this specific conceptualization boundary disappeared, academic priority claims would shift (perhaps to infrastructure operators or to later adoption milestones), but the underlying technical capability would persist unchanged. The world of electronic transfer does not rearrange; the attribution of its discovery does.
% FOUNDING_PROBLEM: Mid-20th-century telecommunications and cryptographic advances created theoretical possibility for authenticated value transfer without trusted intermediaries. The founding problem: how should we understand what 'digital money' is, at what boundary does it emerge, and who gets to define it?
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of cryptography (e.g., Martin, Narayanan) corroborate that Chaum's 1985 work formalized a theoretical breakthrough in anonymous digital payment. However, infrastructure historians and banking technologists dispute whether this formalization counts as an 'emergence' or merely codification of existing capability. Central banks dispute whether academic definitions should govern regulatory classifications. No consensus exists outside the academic cryptography community itself.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint transfers intellectual authority and priority claims without requiring anyone to do new work or abandon existing infrastructure. The transfer is pure authority reallocation: academics claim the category 'digital money' was born at Chaum 1985; infrastructure operators' contributions are redefined as 'electronic payment' or 'money-substitutes,' not money proper. Suppression is low (0.28) because the boundary persists through academic consensus and citation practice, not through enforcement that prevents alternatives — central banks and infrastructure operators can contest it without legal penalty. Theater is very low (0.15) because the boundary does solve a genuine coordination problem (establishing a conceptual frame for payment cryptography research), even if it also extracts authority. The measurement series shows extractiveness rising from 1960–1992 as the academic frame solidifies, then flattening 1992–2000 as regulatory systems adopt the academic definition. Suppression requirement rises modestly as central banks begin actively adjudicating whether the boundary holds in their jurisdictions — after 2000 (outside the interval) this rises sharply as Bitcoin and stablecoins force regulators to either adopt or reject the academic frame.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat: this is genuine coordination (establishing what digital money is). From the infrastructure seat: this is extraction (authority over the definition transferred without technical enablement). From the central bank seat: this is a useful frame for regulation, but challenged authority (if central banks say 'money is what we define,' the academic frame competes with central authority). From infrastructure historians: this is retroactive narrative misappropriation. The engine should compute rope from the academic/protocol seat and snare from the infrastructure seat, despite both analyzing the same constraint. The directionality derivation (beneficiary/victim + exit) routes this divergence correctly.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for academic_cryptographers: beneficiary + institutional power + mobile exit (can publish elsewhere) → d near 0.15 (net beneficiary). Directionality for infrastructure_operators: payer + institutional power + constrained exit (can't redefine their own history) → d near 0.75 (substantial target). Directionality for central_banks: agenda_setter + institutional + arbitrage (can accept or reject the frame in their jurisdiction) → d near 0.50 (symmetric). No overrides needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is definitional: 'what is digital money and when did it emerge?' The founding problem status is contested — academics say Chaum 1985, infrastructure operators say 1967+ ACH, central banks say 'we define it.' The disappearance verdict is contested: if the boundary disappeared, would the world rearrange? The academic priority disappears, but electronic transfer continues unchanged. The constraint does NOT solve a persistent problem (digital money still works if the boundary moves or vanishes); rather it allocates authority over how to categorize what already works. This is a classic mandate corruption flag: the constraint started as a coordination problem (how should we understand payment cryptography?) but persists as authority extraction (who gets to say what 'money' is?). Mandatrophy is partially resolved: the founding problem's status has shifted from 'live' (1960s, genuine uncertainty about digital money's nature) to 'dead' (2000s onward, the technical capability is undisputed, only the naming is contested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmeb_conceptual_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement_basis(dmeb_conceptual_tr_t1960, projected).
narrative_ontology:measurement(dmeb_conceptual_tr_t1972, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1972, 0.08).
narrative_ontology:measurement_basis(dmeb_conceptual_tr_t1972, projected).
narrative_ontology:measurement(dmeb_conceptual_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement_basis(dmeb_conceptual_tr_t1985, observed).
narrative_ontology:measurement(dmeb_conceptual_tr_t1992, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1992, 0.14).
narrative_ontology:measurement_basis(dmeb_conceptual_tr_t1992, observed).
narrative_ontology:measurement(dmeb_conceptual_tr_t2000, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(dmeb_conceptual_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(dmeb_conceptual_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement_basis(dmeb_conceptual_be_t1960, projected).
narrative_ontology:measurement(dmeb_conceptual_be_t1972, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1972, 0.22).
narrative_ontology:measurement_basis(dmeb_conceptual_be_t1972, projected).
narrative_ontology:measurement(dmeb_conceptual_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement_basis(dmeb_conceptual_be_t1985, observed).
narrative_ontology:measurement(dmeb_conceptual_be_t1992, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement_basis(dmeb_conceptual_be_t1992, observed).
narrative_ontology:measurement(dmeb_conceptual_be_t2000, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(dmeb_conceptual_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(dmeb_conceptual_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement_basis(dmeb_conceptual_su_t1960, projected).
narrative_ontology:measurement(dmeb_conceptual_su_t1972, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1972, 0.18).
narrative_ontology:measurement_basis(dmeb_conceptual_su_t1972, projected).
narrative_ontology:measurement(dmeb_conceptual_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement_basis(dmeb_conceptual_su_t1985, observed).
narrative_ontology:measurement(dmeb_conceptual_su_t1992, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1992, 0.27).
narrative_ontology:measurement_basis(dmeb_conceptual_su_t1992, observed).
narrative_ontology:measurement(dmeb_conceptual_su_t2000, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement_basis(dmeb_conceptual_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The digital_money_emergence_boundary kernel decomposes into three structurally distinct constraints per the ε-invariance principle: this reading (conceptualization) places emergence at academic formalization and measures ε=0.42 (authority transfer); the infrastructure_reading places emergence at SWIFT/ACH deployment with lower ε (necessity-driven); the consumer_holdings_reading places emergence at consumer accessibility with higher ε (lock-in and platform control). Each reading has its own beneficiary set, victim set, and persistence mechanisms. They are linked via network edges because adoption of one reading affects the legitimacy and adoption probability of the others. The kernel itself is the disputed boundary; the readings are alternative frames applied to that boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
