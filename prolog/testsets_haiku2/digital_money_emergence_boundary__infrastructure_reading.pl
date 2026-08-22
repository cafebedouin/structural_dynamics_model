% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence: Infrastructure Boundary Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the infrastructure reading of the
 *   digital-money-emergence kernel: money is understood to exist at the
 *   boundary where banks CAN move value electronically (1967 ATMs, 1972 ACH,
 *   1977 SWIFT), regardless of whether consumers can directly hold that
 *   value. This reading sets the definitional boundary at infrastructure
 *   capability, not consumer capability. The kernel is contested: alternative
 *   readings place the boundary at theoretical-conceptualization (1960s
 *   telecommunications thinking, Chaum cryptography) or at
 *   consumer-direct-holding (1990s e-purses, 2000s EMD). Each reading
 *   instantiates a different constraint with different beneficiary structures
 *   and different ε values. The infrastructure reading specifically benefits
 *   banking infrastructure operators (SWIFT, ACH, central settlement) and
 *   central banks (who retain monetary authority via the infrastructure
 *   boundary) while excluding competing payment systems and marginalizing
 *   cash. Extractiveness rises from 0.32 to 0.68 over the interval as the
 *   infrastructure boundary becomes increasingly enforced as the de facto
 *   definition of money.
 *
 * KEY AGENTS:
 *   - banking_infrastructure_operators: Sets and controls the standards for electronic transfer; benefits from the infrastructure boundary that excludes non-bank systems (institutional power, arbitrage exit).
 *   - central_banks: Maintains monetary authority via the infrastructure boundary; can treat electronic bank deposits as money while treating non-bank digital instruments as 'not money' until they reach critical mass (institutional power, arbitrage exit).
 *   - competing_payment_systems: Forced to build parallel infrastructure while excluded from the 'money' category; pay the cost of being outside the regulatory perimeter (moderate power, constrained exit).
 *   - cash_ecosystem_agents: Progressively marginalized as the infrastructure boundary becomes normative; experience the constraint as their irrelevance (powerless, trapped exit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.55).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence: Infrastructure Boundary Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '3db1a919-896a-4892-a354-cbbc86bf0778').
narrative_ontology:cs_kernel_codification('3db1a919-896a-4892-a354-cbbc86bf0778', formalized).
narrative_ontology:cs_authority_grounding('3db1a919-896a-4892-a354-cbbc86bf0778', extraction).
narrative_ontology:cs_interpretation_layer_present('3db1a919-896a-4892-a354-cbbc86bf0778').
narrative_ontology:cs_reading_relation('3db1a919-896a-4892-a354-cbbc86bf0778', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('3db1a919-896a-4892-a354-cbbc86bf0778', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('3db1a919-896a-4892-a354-cbbc86bf0778', foundational, operational_capability_defines_money).
narrative_ontology:cs_axiom_status(operational_capability_defines_money, holdable).
narrative_ontology:cs_axiom_grounding('3db1a919-896a-4892-a354-cbbc86bf0778', operational_capability_defines_money, instrumental).
narrative_ontology:cs_axiom('3db1a919-896a-4892-a354-cbbc86bf0778', foundational, banking_infrastructure_monopoly_necessary).
narrative_ontology:cs_axiom_status(banking_infrastructure_monopoly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3db1a919-896a-4892-a354-cbbc86bf0778', banking_infrastructure_monopoly_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('3db1a919-896a-4892-a354-cbbc86bf0778', banking_infrastructure_gatekeeping_authority).
narrative_ontology:cs_drift_state('3db1a919-896a-4892-a354-cbbc86bf0778', contemporary_2010, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3db1a919-896a-4892-a354-cbbc86bf0778', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, competing_payment_systems).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cash_ecosystem_agents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, regular_consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, regular_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the electronic transfer rails (SWIFT, ACH, ATM networks) that enable inter-bank value movement. They set technical standards, coordinate settlement, and define what 'money' means in the operational sense through their infrastructure capabilities. They benefit from the definitional boundary that treats electronic bank-to-bank transfer as the essence of digital money, which consolidates their monopoly on clearing and places non-bank payment systems outside the regulated 'money' category.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Maintain monetary authority by defining money through the banking infrastructure operators deploy. This reading lets them treat electronic bank deposits as the boundary of 'digital money,' preserving the distinction between money (regulated, traceable through banking) and alternative payment instruments (unregulated, outside their mandate). They benefit from the infrastructure boundary because it keeps non-bank payment innovation outside the 'money' category and thus outside direct regulatory scope until after critical mass.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary,
    institutional, generational, arbitrage, national).

% Operate as the sole intermediaries between consumers and the electronic transfer infrastructure. By defining digital money at the infrastructure-accessibility boundary rather than the consumer-accessibility boundary, the reading preserves banks' function as mandatory gatekeepers: consumers cannot hold digital money directly; they hold bank deposits, which the banks then move electronically on their behalf. This position persists as long as the definitional boundary holds.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    organized, biographical, constrained, regional).

% Non-bank payment networks (Visa, early e-cash systems, eventual crypto) that seek to move value electronically but lack access to the central banking and settlement infrastructure. They are forced to carry the entire cost of their own infrastructure build-out while the banking system's infrastructure costs are socialized across the financial system. They bear the extraction of being excluded from the category 'digital money' even when they provide electronic transfer; their exclusion sustains the banking system's monopoly on the definition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, competing_payment_systems, payer,
    moderate, biographical, constrained, global).

% Currency printers, armored transport providers, retailers, informal networks that depend on cash-based transactions. They experience the infrastructure boundary as the boundary that triggers their irrelevance: once electronic transfer is defined as the essence of digital money and becomes normative, cash is reclassified as pre-digital, legacy, expensive to handle, and regulatory burden. They bear the cost of the transition not through direct extraction but through progressive marginalization.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cash_ecosystem_agents, payer,
    powerless, immediate, trapped, local).

% Gain convenience through ATM access and electronic payment (rather than cash-only), but remain locked into bank intermediation: they cannot hold digital money directly, only bank deposits. They pay indirectly through deposit fees, interest-rate spreads, and the constraint that their access to digital money is contingent on maintaining a banking relationship. They have no choice to opt out—the infrastructure boundary makes banking mandatory.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, regular_consumers, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, regular_consumers, payer).

% Would-be innovators in payment systems and digital value transfer who are structurally excluded from the 'money' category by this reading's boundary definition. They remain outside the regulatory perimeter and thus outside the legitimacy structure that would allow them to integrate with the central banking system. Their exclusion is what the infrastructure boundary enforces.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_technologists, excluded,
    moderate, biographical, mobile, global).

% Monitors whether the infrastructure boundary accurately captures the economic and operational reality of digital money, or whether it has become a definitional cage that protects incumbent financial infrastructure at the cost of excluding legitimate innovation and financial inclusion. They observe the constraint's persistence and can intervene to shift the boundary (as occurred with e-money directives and later crypto regulation).
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-coincidence problem at scale: electronic banking infrastructure enables banks to move value on behalf of customers without physical currency exchange, reducing transaction friction and enabling automated clearing across institutions and geographies.
% TRANSFER_FUNCTION: Moves the definitional authority to place what counts as 'money' from tangible-currency-in-hand to electronic-transfer-capable-by-banks. Transfers this authority from consumers (who used to define money by carrying it) to infrastructure operators (who now define it by being able to route it). Captures regulatory and technical gatekeeping power for banking infrastructure providers.
% ABSENT_VOICES: Non-bank payment innovators, alternative settlement systems, direct-holding digital money proponents (who are explicitly excluded by this boundary), and cash-dependent populations who experience the boundary shift as their elimination from the modern financial system.
% DISAPPEARANCE_RATIONALE: If this infrastructure boundary vanished and digital money reverted to consumer-holding definitions, banking gatekeeping would collapse within months; non-bank payment systems would integrate directly with central banks; cash would retain legitimacy as an alternative digital form; and the infrastructure operators' monopoly on 'money' definition would dissolve. The entire structure of modern finance depends on this boundary holding.
% FOUNDING_PROBLEM: Physical currency could not scale to handle the volume of transactions required by post-industrial economies; inter-bank settlement required trusted clearinghouses and communication standards to move value without moving physical currency.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians (Helleiner, Carruthers, Lacker) attest the founding problem was real in the 1960s–70s. Infrastructure operators (SWIFT founding documents, Federal Reserve ACH justifications) attest it remains live. However, competing payment systems and fintech researchers contest whether the founding problem requires the banking infrastructure monopoly or whether alternative architectures could solve the same coordination problem without excluding non-bank actors—that contest is exactly what the reading gate decides.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the infrastructure reading provides enormous structural advantage to banking operators: it lets them define 'money' in a way that preserves their monopoly on clearing and settlement, excludes non-bank innovators from the 'money' category while leaving them still subject to 'financial system' regulation, and keeps consumers locked into bank intermediation because direct holding is outside the definition. Suppression is moderate-high (0.55) because the constraint requires active enforcement: regulatory policy must exclude alternative payment systems from 'money' status, technical standards must favor banking infrastructure, and competing definitional boundaries must be actively delegitimized (which is why regulatory and academic papers keep re-litigating the boundary). Theater is low-moderate (0.28) because the coordination function (solving double-coincidence, enabling scale) is real, but its ratio to the pure extraction (gatekeeping, monopoly rents) shifts over time as infrastructure matures. The measurement series show extractiveness rising as the infrastructure becomes more established and the boundary more entrenched as policy fact. Theater rises slowly because the functional justification (coordination) remains present even as the extractive overlay accumulates.
 *
 * PERSPECTIVAL GAP:
 *   From the banking infrastructure seat, this reading is pure coordination: 'We solved the problem of scaling currency transfer without moving physical money, and the definition naturally emerged from what the infrastructure could do.' From the competing-payment-systems seat, the same constraint is pure extraction: 'They defined 'money' in a way that preserves their monopoly and keeps us outside the regulatory perimeter where we can never integrate.' From the cash ecosystem seat, it is irrelevance enforced by definition: 'They redefined what counts as money to exclude us, and now our entire economic participation is treated as legacy.' The engine computes per-seat type classifications from the structural data; these divergences are exactly what the classification should reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure operators are the primary beneficiary (they set the rules, control the rails, extract the definition authority) — d approaches 0.1–0.2 (strong beneficiary). Central banks benefit secondarily (they retain monetary authority) — d around 0.3. Competing payment systems bear the cost (excluded from 'money' category, forced to build parallel infrastructure) — d approaches 0.8–0.9 (strong target). Cash agents pay the cost of marginalization — d around 0.75. Regular consumers sit near d=0.5 (genuine coordination benefit in convenience, but mandatory bank intermediation as the cost). The engine derives d from the beneficiary/victim declarations and exit options; the directionality logic explains why different seats experience the same constraint so differently.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is classified as tangled_rope (not snare) because: (1) it has genuine coordination function (the infrastructure solved a real double-coincidence problem), (2) it has identifiable beneficiaries (banking operators, central banks) and victims (competing systems, cash agents), and (3) it requires active enforcement (regulatory policy, technical standards, deliberate exclusion of alternatives). The mandatrophy question is whether the founding problem (scaling currency transfer) is still live, dead, or contested. The founding problem is CONTESTED: the constraint persists because beneficiaries actively defend the infrastructure boundary, not because the underlying coordination problem remains unsolved. Alternative architectures (competing payment networks, direct digital currency) could solve the same coordination problem without the banking monopoly. The infrastructure boundary is thus a case of extracted coordination—real solution, overlaid with extraction—which is the definition of tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_vs_capability_boundary,
    'Is the definitional boundary correctly placed at ''banks CAN move value electronically'' rather than ''consumers CAN hold value directly'' or ''the concept is theoretically coherent''?',
    'Historical analysis of when each boundary becomes policy-salient (regulatory documents, banking standards, academic consensus); comparison of which boundary enabled the greatest economic coordination vs. greatest extraction.',
    'If the boundary is misplaced (should be consumer-holding or conceptualization), then the infrastructure reading instantiates a snare rather than a tangled_rope — pure extraction disguised as coordination. If correctly placed, the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_capability_boundary, conceptual, 'Whether the infrastructure boundary is the natural emergence point for digital money, or an artificial boundary that preserves banking monopoly.').

omega_variable(
    alternative_architecture_viability,
    'Could the same coordination problem (scaling currency transfer without physical movement) be solved by non-banking infrastructure (e.g., competing settlement systems, later blockchain networks, direct central bank digital currency)?',
    'Empirical test via regulatory allowance of competing infrastructure (e.g., EMD directives in Europe, Libra/Diem proposal, CBDC development). Does economic coordination persist or improve under alternative architectures?',
    'If alternative architectures can solve the founding problem without banking gatekeeping, then the infrastructure boundary is revealed as extractive overlay rather than necessity, and the reading should reclassify to snare. If alternatives fail or worsen coordination, the tangled_rope classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_architecture_viability, empirical, 'Whether banking infrastructure monopoly is structurally necessary for digital money coordination.').

omega_variable(
    reading_foreclosure_under_cbdc,
    'Does central bank digital currency (CBDC) — direct consumer holding of central bank liabilities, bypassing commercial banks entirely — foreclose the infrastructure reading?',
    'CBDC deployment data: if CBDCs enable consumers to hold digital money directly without bank intermediation, the infrastructure boundary loses its operational force. The boundary that defined money at ''banks can move it'' becomes obsolete if ''consumers can hold it'' directly from the central bank.',
    'If CBDCs deploy at scale, this reading''s core premise (infrastructure capability = definitional boundary) is overridden by a new operational reality (consumer capability = definitional boundary), and the reading reclassifies from holdable to overridden. The kernel''s structure shifts as one reading becomes inoperative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_under_cbdc, empirical, 'Whether CBDC development forecloses the infrastructure reading of the digital-money-emergence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(digi_tr_t1975, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(digi_tr_t1995, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(digi_tr_t2005, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2010, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.32).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement(digi_be_t1995, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(digi_be_t2005, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2010, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.22).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1975, 0.31).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(digi_su_t1995, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(digi_su_t2005, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2010, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the digital_money_emergence_boundary kernel family. Three constraint stories decompose the contested kernel: infrastructure_reading (THIS), conceptualization_reading (theoretical boundary, 1960s), and consumer_holdings_reading (direct holding boundary, 1990s–2000s). Each reading produces different ε values because the referent (what counts as digital money) shifts per reading. The infrastructure reading (operational capability by banks) yields ε=0.68 (high extraction by beneficiary gatekeeping). The conceptualization reading treats the boundary at theoretical coherence, yielding lower extraction (coordination without monopoly). The consumer-holdings reading treats the boundary at direct access, yielding different beneficiary set (consumers, fintech) and potentially higher extraction if gatekeeping shifts. All three are linked by affects_constraints; each story fully instantiates its reading independent of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
