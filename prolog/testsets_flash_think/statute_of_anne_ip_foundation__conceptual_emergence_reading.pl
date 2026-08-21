% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Conceptual Emergence of Limited Copyright
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story analyzes the Statute of Anne (1710) through the
 *   lens of 'conceptual emergence' – how it created a new conceptual space
 *   for intellectual property. This reading emphasizes the shift from an
 *   understanding of copyright as a perpetual common law right, primarily
 *   benefiting publishers, to a limited statutory right designed to encourage
 *   learning and authorship. The statute is seen as foundational in making IP
 *   'thinkable' as a distinct legal category with a public benefit rationale,
 *   rather than merely a private monopoly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.25).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Conceptual Emergence of Limited Copyright").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '559aea5b-82eb-4394-a88b-eece776653f8').
narrative_ontology:cs_kernel_codification('559aea5b-82eb-4394-a88b-eece776653f8', fixed_text).
narrative_ontology:cs_authority_grounding('559aea5b-82eb-4394-a88b-eece776653f8', lineage).
narrative_ontology:cs_interpretation_layer_present('559aea5b-82eb-4394-a88b-eece776653f8').
narrative_ontology:cs_reading_relation('559aea5b-82eb-4394-a88b-eece776653f8', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_reading_relation('559aea5b-82eb-4394-a88b-eece776653f8', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('559aea5b-82eb-4394-a88b-eece776653f8', foundational, intellectual_property_as_limited_grant).
narrative_ontology:cs_axiom_status(intellectual_property_as_limited_grant, holdable).
narrative_ontology:cs_axiom_grounding('559aea5b-82eb-4394-a88b-eece776653f8', intellectual_property_as_limited_grant, conventional).
narrative_ontology:cs_axiom('559aea5b-82eb-4394-a88b-eece776653f8', foundational, public_access_as_essential_for_progress).
narrative_ontology:cs_axiom_status(public_access_as_essential_for_progress, holdable).
narrative_ontology:cs_axiom_grounding('559aea5b-82eb-4394-a88b-eece776653f8', public_access_as_essential_for_progress, instrumental).
narrative_ontology:cs_reference_frame('559aea5b-82eb-4394-a88b-eece776653f8', limited_term_public_benefit_framework).
narrative_ontology:cs_drift_state('559aea5b-82eb-4394-a88b-eece776653f8', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('559aea5b-82eb-4394-a88b-eece776653f8', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_domain_principle).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, limited_term_copyright).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the conceptual shift that made intellectual works available for public use after a limited term, fostering knowledge dissemination and further creativity. This conceptual space is essential for its function.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    analytical, civilizational, analytical, universal).

% Gained a statutory right to their works, independent of the Stationers' Company, for a limited term. This provided an incentive to create and publish, knowing their rights were legally protected, albeit not perpetually.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, mobile, national).

% Represented by entities like the Stationers' Company, who previously asserted perpetual common law rights. They 'paid' by losing the conceptual basis for their perpetual monopoly, being limited to statutory terms. Their resistance was significant but ultimately unsuccessful against the conceptual shift.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants, payer,
    powerful, biographical, constrained, national).

% The parliamentary body that enacted the Statute of Anne, thereby creating the new conceptual space for intellectual property as a limited, public-benefit-oriented right. They defined the terms and scope of this new legal category.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legislators, agenda_setter,
    institutional, generational, analytical, national).

% Study the historical and conceptual impact of the Statute, analyzing how it fundamentally reshaped the understanding of intellectual property rights and their relationship to public good. They observe the long-term effects of this conceptual emergence.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated the conceptual understanding of intellectual property, establishing it as a limited statutory grant rather than an inherent, perpetual common law right, thereby balancing author incentives with public access to knowledge.
% TRANSFER_FUNCTION: It conceptually transferred the idea of intellectual works from being perpetual private property to being a limited-term regulatory tool for public learning, shifting the underlying rationale and legal framework.
% ABSENT_VOICES: Those who benefited from the prior perpetual common law monopoly, primarily the Stationers' Company and their members, were conceptually sidelined as their claims were re-framed as statutory and time-limited. Their arguments for perpetual rights were effectively excluded from the new conceptual framework.
% DISAPPEARANCE_RATIONALE: If the conceptual space created by the Statute of Anne vanished, the modern understanding of copyright as a limited grant would collapse. Intellectual property would revert to a contested, potentially perpetual, common law right, fundamentally reorganizing legal and economic structures around creative works.
% FOUNDING_PROBLEM: The problem was the lack of a clear, standardized, and publicly beneficial framework for intellectual property, leading to perpetual monopolies by publishers (e.g., Stationers' Company) that hindered public access to knowledge and stifled new authorship.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians, and contemporary policy debates consistently attest that the tension between author incentives and public access, which the Statute sought to balance, remains a live and contested problem in intellectual property law, particularly in the digital age. This corroboration comes from outside the direct beneficiaries of the Statute's original enactment.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.25) is low because the core function of this conceptual shift was to *reduce* the extraction inherent in perpetual monopolies, re-framing IP as a limited grant. Suppression (0.15) is also low, as the constraint primarily opened up conceptual space rather than coercively maintaining an existing one. Resistance (0.30) reflects the pushback from those who benefited from the prior system (e.g., Stationers' Company). Theater ratio (0.05) is minimal, as the conceptual shift was a genuine re-ordering of legal thought, not a performance. Accessibility collapse (0.20) is low because the new conceptual space *increased* access to alternatives (public domain, new authors). The claimed type is 'rope' because it established a beneficial coordination mechanism for knowledge creation and dissemination.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'public learning' and 'authors', the Statute of Anne represented a positive, coordinative shift, opening new possibilities. For 'perpetual_monopoly_claimants', it was a loss of an established right and a source of extraction. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a rope-like coordination and payers experiencing a snare-like extraction of their prior claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Public learning and authors are clear beneficiaries, as the conceptual shift enabled a more balanced system that fostered their interests. Perpetual monopoly claimants are victims/payers, as their prior, more extensive claims were curtailed. Legislators acted as agenda-setters, defining the new conceptual boundaries. Analytical historians serve as observers, analyzing the long-term conceptual impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_separability,
    'Is the conceptual emergence of limited copyright truly separable from the institutional reallocation of rights that occurred simultaneously with the Statute of Anne?',
    'Comparative legal history analysis of other jurisdictions where similar conceptual shifts occurred with different institutional mechanisms, or vice-versa.',
    'If inseparable, this reading''s classification as a ''rope'' might be incomplete, as the institutional extraction (from publishers) would be more central to the constraint''s identity, potentially shifting it towards a ''tangled_rope'' or ''snare'' from certain seats. If separable, the ''rope'' classification for the conceptual shift holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_separability, conceptual, 'Ambiguity regarding the analytical separability of conceptual and institutional changes in the Statute of Anne.').

omega_variable(
    true_beneficiary_of_conceptual_shift,
    'Was the primary beneficiary of this conceptual shift truly ''public learning'' and ''authors'', or did it primarily serve to legitimize a new form of state-sanctioned monopoly for a different set of actors?',
    'Detailed economic and social history analysis of the decades following the Statute, tracking actual changes in book prices, literacy rates, and author incomes, compared to publisher profits.',
    'If the primary beneficiaries were not public learning or authors, but rather a new class of monopolists, the ''rope'' classification would be challenged, potentially reclassifying it as a ''snare'' or ''tangled_rope'' from a critical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_beneficiary_of_conceptual_shift, empirical, 'Uncertainty about the actual primary beneficiaries of the conceptual shift in intellectual property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1720, 0.05).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1730, 0.05).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1740, 0.05).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1750, 0.05).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.3).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1720, 0.28).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1730, 0.26).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1740, 0.25).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1750, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.2).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1720, 0.18).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1730, 0.16).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1740, 0.15).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1750, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statute_of_anne_ip_foundation' kernel. This 'conceptual_emergence_reading' focuses on the shift in the fundamental understanding of intellectual property, distinct from institutional reallocations or an entangled view.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
