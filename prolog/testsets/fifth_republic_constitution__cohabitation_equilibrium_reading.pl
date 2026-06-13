% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Negotiation
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic (1958–present) creates a dual executive: a directly
 *   elected president as head of state, combined with a prime minister
 *   heading a government that requires Assembly confidence. The Constitution
 *   allocates authority ambiguously — the president 'determines' foreign
 *   policy and appoints judges, while the prime minister 'leads' government
 *   and legislates. When the Assembly majority opposes the sitting president,
 *   both actors must negotiate the boundary between their domains. This
 *   reading instantiates the constraint as a negotiated equilibrium where
 *   neither actor can act unilaterally and neither can exit. The
 *   extractiveness is moderate (0.51) because the arrangement both
 *   coordinates (prevents single-actor dominance) and extracts (policy
 *   coherence suffers, citizens absorb gridlock costs). This reading treats
 *   cohabitation as a stable, reciprocal constraint — neither president nor
 *   Assembly is permanently dominant; dominance shifts with elections and
 *   constitutional interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.51).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.38).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Negotiation").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '9fc377fa-236a-4df0-85c1-871d6025ebe2').
narrative_ontology:cs_kernel_codification('9fc377fa-236a-4df0-85c1-871d6025ebe2', fixed_text).
narrative_ontology:cs_authority_grounding('9fc377fa-236a-4df0-85c1-871d6025ebe2', lineage).
narrative_ontology:cs_interpretation_layer_present('9fc377fa-236a-4df0-85c1-871d6025ebe2').
narrative_ontology:cs_reading_relation('9fc377fa-236a-4df0-85c1-871d6025ebe2', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fc377fa-236a-4df0-85c1-871d6025ebe2', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('9fc377fa-236a-4df0-85c1-871d6025ebe2', foundational, dual_executive_mutual_veto).
narrative_ontology:cs_axiom_status(dual_executive_mutual_veto, holdable).
narrative_ontology:cs_axiom_grounding('9fc377fa-236a-4df0-85c1-871d6025ebe2', dual_executive_mutual_veto, conventional).
narrative_ontology:cs_axiom('9fc377fa-236a-4df0-85c1-871d6025ebe2', foundational, electoral_legitimacy_dual_source).
narrative_ontology:cs_axiom_status(electoral_legitimacy_dual_source, holdable).
narrative_ontology:cs_axiom_grounding('9fc377fa-236a-4df0-85c1-871d6025ebe2', electoral_legitimacy_dual_source, deontological).
narrative_ontology:cs_axiom('9fc377fa-236a-4df0-85c1-871d6025ebe2', secondary, constitutional_ambiguity_intentional).
narrative_ontology:cs_axiom_status(constitutional_ambiguity_intentional, holdable).
narrative_ontology:cs_axiom_grounding('9fc377fa-236a-4df0-85c1-871d6025ebe2', constitutional_ambiguity_intentional, conventional).
narrative_ontology:cs_reference_frame('9fc377fa-236a-4df0-85c1-871d6025ebe2', dual_legitimacy_separated_powers_framework).
narrative_ontology:cs_drift_state('9fc377fa-236a-4df0-85c1-871d6025ebe2', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9fc377fa-236a-4df0-85c1-871d6025ebe2', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, sitting_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_assembly_coalition).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, executive_decisiveness).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.35 at founding) when de Gaulle dominated via informal authority; rises to 0.48 by the first cohabitation (1986) when the constraint became formally binding and negotiation became necessary; stabilizes around 0.51 after the 1995–2007 equilibration period. The 2007 constitutional reform shortened the presidential term to align with Assembly cycles, momentarily reducing extractiveness (0.49) because fewer cohabitation periods were possible; however, by 2017 extractiveness returns to 0.51 as the equilibrium reasserts. Theater ratio rises from 0.25 (early presidency's formal authority was largely real) to ~0.42 (contemporary equilibrium, where much negotiation is performed public displays of consensus before compromise is reached). Suppression requirement is moderate (0.38 endpoint) because neither actor can suppress the other through force; negotiation itself is the suppression mechanism — the actors are constrained by the need to govern jointly. Accessibility collapse is moderate (0.62) because alternatives exist (constitutional amendment, electoral dissolution, EU exit), but the costs of those exits are high. Resistance is high (0.58) because both major actors and civil society publicly contest the boundaries of cohabitation authority; the constraint persists despite continuous resistance, not through absence of it.
 *
 * PERSPECTIVAL GAP:
 *   The sitting president perceives the arrangement as constraining (trapped into negotiation with an opposition Assembly); the Assembly coalition perceives it as constraining (must compromise with a president who controls foreign policy and appointments). Both are accurate — this is a tangled rope where both coordination and extraction inhere. The gap is not an illusion but a structural symmetry: the constraint binds both parties equally but opposite-directionally. Neither can claim victim status without claiming the other is an aggressor, which would dissolve the negotiation frame. The engine should compute symmetric-ish directionalities (both near 0.5) with per-seat type divergence (one seat sees coordination-extraction, the other sees extraction-coordination — different emphasis, same structure).
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting president holds high power (institutional) but is trapped into the negotiation (trapped exit) — derives directionality near 0.6 (beneficiary-payer symmetric, but structurally constrained). The Assembly coalition holds equal institutional power but has marginally better exit options (constrained: can dissolve and call new elections) — derives directionality near 0.45 (slight beneficiary lean). Both actors benefit from the coordination function (preventing dictatorship) and both pay from the extraction function (policy coherence suffers when negotiation produces compromise). Policy coherence, listed as a victim, is not an agent — it is the non-actor cost carried by institutions and citizens. The judiciary's position is analytically asymmetric: it observes and rules on boundaries but cannot initiate change, making it neither beneficiary nor payer in the standard sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing dictatorship while preserving executive stability) remains LIVE. The constraint has NOT become a zombie or piton — cohabitation periods (1986–1988, 1993–1995, 1997–2002) produced real policy differences and required genuine negotiation, not mere performance. However, the 2000 constitutional reform (shortening the presidential term to five years, aligning it with Assembly cycles) was intended to make cohabitation rarer. The reform succeeded empirically: cohabitation became less frequent after 2002. This suggests the constraint may drift toward hyper-presidentialism as elections align — the founding problem of preventing single-actor dominance may be weakening. An omega variable flags this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_frequency_drift,
    'Will the structural alignment of presidential and Assembly terms (post-2000 reform) eventually eliminate cohabitation periods entirely, and if so, will the constraint drift from negotiated equilibrium toward hyper-presidentialism?',
    'Monitor the occurrence of cohabitation periods across electoral cycles. If cohabitation becomes vanishingly rare (< 1 in 5 elections), measure whether presidential authority expands into traditionally prime ministerial domains and whether Assembly majorities become ceremonial.',
    'If cohabitation becomes rare, this reading''s core claim (dual executive requires continuous negotiation) becomes empirically false. The constraint would reclassify as hyper_presidential_reading with minimal legislative checks. The founding problem (preventing dictatorship) would re-emerge as live and unaddressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_frequency_drift, empirical, 'Whether constitutional alignment of electoral cycles is undermining the cohabitation equilibrium itself.').

omega_variable(
    eu_supranational_constraint_subsumption,
    'As EU competency expands (trade, monetary, regulatory domains), do the boundaries negotiated between president and prime minister become irrelevant? Does supranational law supercede the domestic dual-executive question?',
    'Analyze the proportion of government policy made by EU directives vs. constitutional allocation between president and prime minister. If EU-mandated policy exceeds 50% of the government''s substantive authority, the cohabitation question becomes a regional detail within a larger supranational structure.',
    'If supranational constraints supersede constitutional boundaries, the cohabitation reading loses relevance — the victim (policy coherence) is already lost to external coordination. The constraint would shift from a political question (who decides between president and Assembly) to an administrative one (how do we implement EU requirements). The founding problem (preventing domestic dictatorship) becomes historically resolved as irrelevant; a new founding problem (coordinating with supranational structures) emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eu_supranational_constraint_subsumption, empirical, 'Whether EU integration is making the domestic cohabitation equilibrium structurally obsolete.').

omega_variable(
    kernel_reading_instability,
    'Is the cohabitation equilibrium reading itself a stable interpretation of the Constitution, or is it an unstable compromise that shifts toward hyper-presidentialism during unified government and toward parliamentary constraint during cohabitation?',
    'Examine constitutional adjudication and political practice across multiple cohabitation and non-cohabitation periods. If presidential authority expands in unified periods and contracts in cohabitation periods (a cyclical pattern), the ''equilibrium'' reading is actually two alternating readings masquerading as one stable constraint.',
    'If the reading is empirically unstable, decompose into separate constraints for unified government (hyper_presidential_reading dominates) and cohabitation government (equilibrium reading dominates). The current story would become a periodic reading, not a stable equilibrium — reclassify as a cyclic snare where each actor extracts when they hold power and negotiates when they don''t.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instability, empirical, 'Whether the equilibrium reading is stable or a mask over cyclical alternation between presidential and parliamentary dominance.').

omega_variable(
    constitutional_amendment_suppression_mechanism,
    'Why has the Constitution not been amended to clarify the ambiguity between presidential and prime ministerial authority, despite 66+ years of cohabitation disputes and judicial rulings? Is the ambiguity deliberately preserved because it allows both actors to claim authority as needed?',
    'Examine constitutional amendment proposals and their fates. If clarifying amendments are proposed and rejected, determine which actors oppose clarification and why. If no clarifying amendments are proposed despite routine disputes, the absence itself is a signal.',
    'If the ambiguity is deliberately preserved, the constraint is not a coordinating equilibrium but a snare — both president and Assembly benefit from the ability to claim authority as circumstances shift, and citizens absorb the cost of permanent uncertainty. This would reclassify the reading as a false rope (claimed coordination, actual extraction via institutionalized ambiguity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_suppression_mechanism, empirical, 'Whether the constitutional ambiguity is structurally necessary or deliberately maintained for extractive gain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1959, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(measurement_theater_1959_founding, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1959, 0.25).
narrative_ontology:measurement(measurement_theater_1986_first_cohabitation, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.38).
narrative_ontology:measurement(measurement_theater_1995_mid_equilibrium, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(measurement_theater_2007_post_reform, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2007, 0.41).
narrative_ontology:measurement(measurement_theater_2017_contemporary, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2017, 0.42).
narrative_ontology:measurement(measurement_theater_2025_endpoint, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(measurement_extractiveness_1959_founding, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1959, 0.35).
narrative_ontology:measurement(measurement_extractiveness_1986_first_cohabitation, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(measurement_extractiveness_1995_mid_equilibrium, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(measurement_extractiveness_2007_post_presidential_reform, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2007, 0.49).
narrative_ontology:measurement(measurement_extractiveness_2017_contemporary, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2017, 0.51).
narrative_ontology:measurement(measurement_extractiveness_2025_endpoint, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2025, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(measurement_suppression_1959_founding, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1959, 0.28).
narrative_ontology:measurement(measurement_suppression_1986_first_cohabitation, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(measurement_suppression_1995_mid_equilibrium, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(measurement_suppression_2007_post_reform, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2007, 0.37).
narrative_ontology:measurement(measurement_suppression_2017_contemporary, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(measurement_suppression_2025_endpoint, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, eu_regulatory_harmonization_supremacy).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, french_constitutional_amendment_threshold).

% DUAL FORMULATION NOTE:
% The Fifth Republic Constitution kernel admits three structurally distinct readings: (1) HYPER_PRESIDENTIAL: president as direct sovereign, Assembly as legislative detail, extractiveness low (0.2–0.3), mountain-leaning. (2) PARLIAMENTARY_CONSTRAINT: president ceremonial, prime minister supreme, Assembly sovereign, extractiveness very high (0.75+), snare. (3) COHABITATION_EQUILIBRIUM (this story): dual executive, negotiated authority, extractiveness moderate (0.45–0.55), tangled rope. Each reading has different beneficiaries, victims, and ε. They coexist in practice across different electoral configurations; decomposition is required to avoid conflating readings into one under-determined story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
