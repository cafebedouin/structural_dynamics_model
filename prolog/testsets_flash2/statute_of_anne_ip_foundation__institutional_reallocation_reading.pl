% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation of Copyright
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'institutional reallocation' reading
 *   of the Statute of Anne, focusing on how the statute shifted existing
 *   property rights from the Stationers' Company to authors, thereby changing
 *   who occupied the institutional space of intellectual property. It views
 *   the statute primarily as a legal and economic reordering of existing
 *   claims, rather than the creation of an entirely new conceptual category
 *   of property. The claimed type is 'tangled_rope' because it provided a
 *   coordination function (clearer rights) but also involved significant
 *   extraction from the former monopolists and continued, albeit limited,
 *   extraction by publishers from authors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.7).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Copyright").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '0272f174-1166-47a8-91f4-09a1ea2ec99e').
narrative_ontology:cs_kernel_codification('0272f174-1166-47a8-91f4-09a1ea2ec99e', formalized).
narrative_ontology:cs_authority_grounding('0272f174-1166-47a8-91f4-09a1ea2ec99e', lineage).
narrative_ontology:cs_interpretation_layer_present('0272f174-1166-47a8-91f4-09a1ea2ec99e').
narrative_ontology:cs_reading_relation('0272f174-1166-47a8-91f4-09a1ea2ec99e', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0272f174-1166-47a8-91f4-09a1ea2ec99e', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('0272f174-1166-47a8-91f4-09a1ea2ec99e', foundational, property_rights_are_reallocatable).
narrative_ontology:cs_axiom_status(property_rights_are_reallocatable, holdable).
narrative_ontology:cs_axiom_grounding('0272f174-1166-47a8-91f4-09a1ea2ec99e', property_rights_are_reallocatable, conventional).
narrative_ontology:cs_axiom('0272f174-1166-47a8-91f4-09a1ea2ec99e', secondary, parliamentary_supremacy_in_lawmaking).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_in_lawmaking, holdable).
narrative_ontology:cs_axiom_grounding('0272f174-1166-47a8-91f4-09a1ea2ec99e', parliamentary_supremacy_in_lawmaking, conventional).
narrative_ontology:cs_reference_frame('0272f174-1166-47a8-91f4-09a1ea2ec99e', pre_statute_common_law_and_guild_control).
narrative_ontology:cs_drift_state('0272f174-1166-47a8-91f4-09a1ea2ec99e', post_statute_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0272f174-1166-47a8-91f4-09a1ea2ec99e', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, authorial_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lost its perpetual monopoly on printing and publishing rights, which had been enforced through royal prerogative and guild control. The statute directly curtailed its power and revenue stream, forcing it to adapt to a new legal landscape.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly, payer,
    institutional, generational, trapped, national).

% Were granted a statutory right to their works for a limited term, which they could then assign to publishers. This created a new legal basis for their claim to intellectual property, shifting power from printers to creators, though often immediately assigned away.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Acquired rights from authors for a limited term, becoming the primary commercial beneficiaries of the new statutory framework. They gained a clearer, though time-limited, legal basis for their business model, replacing the old guild-based system.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment, agenda_setter,
    powerful, biographical, mobile, national).

% Benefited from increased competition among publishers after the Stationers' monopoly was broken, potentially leading to lower book prices and greater access to works. Their access was no longer solely controlled by a single powerful guild.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    organized, generational, mobile, national).

% Enacted the statute, asserting its authority to regulate property rights and commerce, thereby establishing a new legal foundation for intellectual property distinct from royal prerogative or guild custom. It acted as the ultimate arbiter of this reallocation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, statutory basis for intellectual property rights, coordinating the relationship between authors, publishers, and the public by defining who held rights and for how long, replacing a contested common law and guild system.
% TRANSFER_FUNCTION: Transferred the primary legal locus of intellectual property rights from the Stationers' Company's perpetual monopoly to authors (who then typically assigned them to publishers) for a limited term, thereby reallocating economic value and control.
% ABSENT_VOICES: The broader public, particularly those advocating for free access to knowledge, were not directly represented in the drafting, though the statute's limited term was a concession to public interest. Their voice would have pushed for even shorter terms or public domain access.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne vanished, the legal foundation of modern copyright would collapse, leading to immediate chaos in publishing, a return to contested common law claims, and a fundamental reordering of how creative works are owned and disseminated.
% FOUNDING_PROBLEM: The existing system of perpetual common law copyright and the Stationers' Company's monopoly led to disputes over ownership, high book prices, and a lack of clear legal recourse for authors, hindering the 'encouragement of learning'.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and intellectual property scholars widely corroborate that the statute addressed a genuine problem of legal clarity and authorial recognition, even if its implementation immediately shifted benefits to publishers. The problem of balancing authorial rights with public access remains live.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because while authors gained rights, these were often immediately assigned to publishers, who continued to profit significantly. Suppression is high (0.7) as the statute actively suppressed the Stationers' Company's former monopoly and required enforcement to establish the new statutory regime. Theater ratio is low (0.1) because the statute had a direct, functional impact on legal and economic structures, with little performative maintenance. Accessibility collapse is moderate (0.4) as it broke one monopoly but established a new, albeit limited, form of control. Resistance is high (0.8) due to the intense legal and political battles waged by the Stationers' Company against the new regime.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' Company would experience this as a snare, a direct attack on their established rights and business model. Authors would experience it as a rope, a new legal protection for their work. Publishers would see it as a tangled rope, a new framework that enabled their business but also imposed new limitations. The engine's per-seat classification will reflect these structural differences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company is the primary victim (d=1.0) as their monopoly was directly curtailed. Authors are beneficiaries (d=0.0) as they gained statutory rights, though their effective benefit was often mediated by publishers. Publishers, via assignment, became agenda-setters and beneficiaries (d=0.15) as they gained a new, clearer legal basis for their business. The reading public is a beneficiary (d=0.0) due to increased competition. Parliament is an agenda-setter (d=0.1) asserting its regulatory authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_primacy,
    'Is the Statute of Anne primarily an institutional reallocation of existing rights, or did it fundamentally create a new conceptual category of intellectual property?',
    'Analysis of contemporary legal discourse and philosophical texts: if the concept of ''authorial right'' was genuinely novel and not merely a re-framing of existing claims, it supports the conceptual emergence reading.',
    'If primarily conceptual, the constraint''s extractiveness might be lower (as it created something new rather than reallocating existing value); if primarily institutional, the extractiveness is higher (as it involved a direct transfer of economic power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_primacy, conceptual, 'Ambiguity over whether the statute reallocated existing rights or created new ones.').

omega_variable(
    authors_effective_benefit,
    'To what extent did authors genuinely benefit from the statutory rights, given the common practice of immediate assignment to publishers?',
    'Empirical study of author contracts and economic conditions in the post-1710 period: if authors consistently received fair compensation for assignments, their effective benefit was high; if not, their benefit was largely nominal.',
    'If authors'' effective benefit was low, their directionality would shift closer to ''payer'', increasing the overall extractiveness of the constraint from their seat. If high, it reinforces the ''beneficiary'' role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authors_effective_benefit, empirical, 'The actual economic impact of the statute on authors versus publishers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1734).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(stat_be_t1716, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1716, 0.58).
narrative_ontology:measurement(stat_be_t1722, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1722, 0.61).
narrative_ontology:measurement(stat_be_t1728, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1728, 0.63).
narrative_ontology:measurement(stat_be_t1734, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1734, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.6).
narrative_ontology:measurement(stat_su_t1716, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1716, 0.63).
narrative_ontology:measurement(stat_su_t1722, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1722, 0.66).
narrative_ontology:measurement(stat_su_t1728, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1728, 0.68).
narrative_ontology:measurement(stat_su_t1734, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1734, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'Statute of Anne IP Foundation' kernel. This reading focuses on the institutional reallocation of rights, while others emphasize conceptual emergence or an inseparable entanglement of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
