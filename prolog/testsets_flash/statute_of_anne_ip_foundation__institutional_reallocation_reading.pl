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
 *   This constraint story analyzes the Statute of Anne (1710) through the
 *   'institutional reallocation' reading, focusing on how the statute shifted
 *   existing rights from the Stationers' Company to authors, thereby changing
 *   who occupied the institutional space of copyright. It views copyright not
 *   as a wholly new concept emerging, but as a re-engineering of existing
 *   privileges and economic flows within the publishing industry. The statute
 *   broke the Stationers' perpetual monopoly and vested initial rights in
 *   authors, but these rights were quickly reassigned to publishers, who
 *   became the de facto beneficiaries of the new, time-limited system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.6).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.7).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Copyright").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '3cb93a02-34ba-4acf-b006-90e2c9786a09').
narrative_ontology:cs_kernel_codification('3cb93a02-34ba-4acf-b006-90e2c9786a09', formalized).
narrative_ontology:cs_authority_grounding('3cb93a02-34ba-4acf-b006-90e2c9786a09', lineage).
narrative_ontology:cs_interpretation_layer_present('3cb93a02-34ba-4acf-b006-90e2c9786a09').
narrative_ontology:cs_reading_relation('3cb93a02-34ba-4acf-b006-90e2c9786a09', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cb93a02-34ba-4acf-b006-90e2c9786a09', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('3cb93a02-34ba-4acf-b006-90e2c9786a09', foundational, rights_are_reallocatable_institutional_privileges).
narrative_ontology:cs_axiom_status(rights_are_reallocatable_institutional_privileges, holdable).
narrative_ontology:cs_axiom_grounding('3cb93a02-34ba-4acf-b006-90e2c9786a09', rights_are_reallocatable_institutional_privileges, conventional).
narrative_ontology:cs_axiom('3cb93a02-34ba-4acf-b006-90e2c9786a09', foundational, authors_as_initial_rightsholders_by_statute).
narrative_ontology:cs_axiom_status(authors_as_initial_rightsholders_by_statute, holdable).
narrative_ontology:cs_axiom_grounding('3cb93a02-34ba-4acf-b006-90e2c9786a09', authors_as_initial_rightsholders_by_statute, conventional).
narrative_ontology:cs_reference_frame('3cb93a02-34ba-4acf-b006-90e2c9786a09', pre_statute_stationers_monopoly).
narrative_ontology:cs_drift_state('3cb93a02-34ba-4acf-b006-90e2c9786a09', post_statute_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3cb93a02-34ba-4acf-b006-90e2c9786a09', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_initially).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, public_domain_access_delay).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lost its perpetual monopoly over printing rights, which was a foundational element of its economic and political power. Forced to adapt to a new, time-limited system where rights originated with authors.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, trapped, national).

% Gained a statutory right to their works, a significant shift from the previous system where rights were held by printers. However, this right was often immediately assigned to publishers for economic necessity.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_initially, beneficiary,
    moderate, biographical, constrained, national).

% Quickly adapted to the new system by acquiring rights from authors through assignment, effectively maintaining control over the lucrative aspects of publishing, albeit under a new legal framework. They became the primary beneficiaries of the reallocated rights.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment, agenda_setter,
    powerful, generational, mobile, national).

% Enacted the statute to address perceived abuses of the Stationers' monopoly and to encourage learning. It established the new legal framework for copyright, shifting the locus of control.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% While the statute introduced a time limit on copyright, it also created a new form of exclusive right, delaying works entering the public domain compared to a system with no such rights. Their voice for broader public access was not fully realized.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, public_domain_advocates, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, statutory framework for literary property rights, replacing a guild-based monopoly with a system intended to incentivize authors and regulate the publishing trade.
% TRANSFER_FUNCTION: Transferred the initial grant of printing rights from the Stationers' Company to authors, which was then typically transferred from authors to publishers through assignment, along with the economic value derived from those rights.
% ABSENT_VOICES: The public, particularly those advocating for immediate and unfettered access to knowledge, were largely absent from the legislative process. Their interests in a robust public domain were partially addressed by the time limit but not prioritized over exclusive rights.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its principles vanished, the entire modern intellectual property system would collapse. The concept of authors' rights, the time-limited nature of copyright, and the legal framework for publishing would cease to exist, leading to a complete reorganization of creative industries.
% FOUNDING_PROBLEM: The Stationers' Company held a perpetual monopoly over printing, leading to high prices, censorship, and a lack of incentive for authors. There was no clear legal right for authors to control their works.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that the Stationers' monopoly was a significant problem. While the specific problem of the Stationers' Company is dead, the statute's legacy created new problems related to the balance between authorial rights and public access, which remain live and contested by legal academics and public interest groups.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).

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
 *   The extractiveness (0.6) reflects the continued ability of publishers to control and profit from literary works, albeit under a new legal guise. Suppression (0.7) is high due to the legal enforcement mechanisms that protected these new rights and excluded others from printing. The theater ratio (0.1) is low because the statute had a clear, functional impact on the legal and economic structure of publishing, rather than being merely performative. Accessibility collapse (0.4) is moderate; while the Stationers' monopoly was broken, a new form of exclusive right was established, still limiting public access, but with a time limit. Resistance (0.5) was present from the Stationers' Company, who fought to retain their old privileges, and later from those challenging the scope of the new rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Stationers' Company, the statute was a snare, stripping them of their established rights. From the perspective of authors, it was initially a rope, granting them new legal standing, but quickly became a tangled rope as economic realities forced assignment to publishers. Publishers viewed it as a new, more stable form of rope, allowing them to continue their business with clearer legal backing. Parliament saw it as a scaffold, a temporary measure to correct market failures and encourage learning, intended to transition to a more balanced system.
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company became a primary victim, losing their perpetual monopoly. Authors were initially beneficiaries, gaining statutory rights, but often became payers by assigning these rights to publishers. Publishers, through assignment, became the primary beneficiaries and agenda-setters, effectively controlling the new system. Parliament acted as the ultimate agenda-setter, establishing the framework. Public domain advocates were largely excluded, as the statute, while introducing limits, still created exclusive rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The statute's original mandate was to break the Stationers' monopoly and incentivize authors. While the monopoly was broken, the incentive for authors was often short-circuited by assignment practices. The constraint avoided becoming a piton because the reallocated rights remained actively enforced and economically significant for publishers. It is a tangled rope because it genuinely coordinated the publishing industry under a new legal framework while simultaneously extracting value through the publishers' control of authorial assignments. The founding problem is 'dead' in its original form, but the constraint persists due to the new economic structures it enabled, leading to ongoing debates about its 'live' status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_primacy,
    'Did the Statute of Anne primarily reallocate existing institutional rights, or did it fundamentally create a new conceptual category of ''copyright''?',
    'Detailed historical analysis of legal precedents and contemporary discourse: if the language and legal arguments primarily focused on re-framing existing privileges rather than inventing new ones, it supports the reallocation reading.',
    'If primarily reallocation, the constraint is a tangled rope, managing existing economic flows. If primarily conceptual emergence, it might be closer to a mountain (a new legal ''natural law'') or a rope (a new coordination mechanism for a new concept).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_primacy, conceptual, 'Ambiguity between institutional reallocation and conceptual emergence of copyright.').

omega_variable(
    authors_as_true_beneficiaries,
    'To what extent did authors truly benefit from the Statute of Anne, given the prevalence of assignment practices to publishers?',
    'Economic analysis of author contracts and income streams post-1710, compared to pre-1710 patronage and direct sales models. If authors'' economic position significantly improved, it supports their beneficiary status.',
    'If authors were largely compelled to assign rights for minimal compensation, their role shifts closer to a victim, increasing the overall extractiveness of the constraint. If they retained significant bargaining power, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authors_as_true_beneficiaries, empirical, 'The actual economic benefit to authors versus publishers.').

omega_variable(
    entanglement_of_change,
    'Can the institutional reallocation and conceptual emergence aspects of the Statute of Anne be meaningfully disentangled for analysis, or are they fundamentally inseparable?',
    'Philosophical and historical argument regarding the nature of legal change: if every institutional shift necessarily implies a conceptual re-framing, then disentanglement is artificial. If one can occur without the other, then they are separable.',
    'If inseparable, this ''institutional reallocation'' reading is incomplete, and the ''entangled event'' reading is more accurate, suggesting a more complex, hybrid classification. If separable, this reading stands as a valid, focused analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_of_change, conceptual, 'Whether institutional and conceptual changes are separable or entangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1738).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1717, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1717, 0.1).
narrative_ontology:measurement(stat_tr_t1724, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1724, 0.1).
narrative_ontology:measurement(stat_tr_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1731, 0.1).
narrative_ontology:measurement(stat_tr_t1738, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1738, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement(stat_be_t1717, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1717, 0.55).
narrative_ontology:measurement(stat_be_t1724, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1724, 0.58).
narrative_ontology:measurement(stat_be_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1731, 0.6).
narrative_ontology:measurement(stat_be_t1738, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1738, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.65).
narrative_ontology:measurement(stat_su_t1717, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1717, 0.68).
narrative_ontology:measurement(stat_su_t1724, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1724, 0.7).
narrative_ontology:measurement(stat_su_t1731, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1731, 0.7).
narrative_ontology:measurement(stat_su_t1738, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1738, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'statute_of_anne_ip_foundation' kernel, focusing on the institutional reallocation of rights. It is linked to sibling readings that emphasize conceptual emergence and the entanglement of both dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
