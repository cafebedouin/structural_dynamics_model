% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence â Thinkability Reading (1710)
 *   domain: legal/philosophical/intellectual_property
 *
 * SUMMARY:
 *   The Statute of Anne 1710 is conventionally read as the birth of modern
 *   copyright. The thinkability reading treats it not merely as a transfer of
 *   rights but as the moment when 'ownable expression' became a legally
 *   coherent category. Before 1710, disputes over printing were framed
 *   through guild privilege, royal patent, or censorship; after 1710, parties
 *   could argue about 'copy right' as a distinct juridical object. This
 *   reading foregrounds conceptual coherence over occupancy change: the
 *   category itself had to become thinkable before authors could enter the
 *   claimant set. The constraint coordinates by providing a shared vocabulary
 *   for adjudication, while it extracts by enclosing the intellectual commons
 *   and enabling asymmetric publisher capture.
 *
 * KEY AGENTS:
 *   - parliament (agenda setter, institutional, mobile): enacts the statutory frame and can alter it by legislation
 *   - published_authors (beneficiary, moderate, constrained): gain nominal rights but remain dependent on publishers
 *   - booksellers_and_printers (beneficiary, organized, mobile): capture the economic gains of the new property category
 *   - unauthorized_reproducers (payer, powerless, trapped): face enforcement for operating outside the new rights regime
 *   - reading_public (payer, powerless, constrained): bear higher prices and restricted access
 *   - legal_interpreters (observer, organized, analytical): adjudicate the boundaries of ownable expression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.45).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.5).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence â Thinkability Reading (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal/philosophical/intellectual_property").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '51ffc751-c4c9-49b6-bd73-9f891ca98e4d').
narrative_ontology:cs_kernel_codification('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', formalized).
narrative_ontology:cs_authority_grounding('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', lineage).
narrative_ontology:cs_interpretation_layer_present('51ffc751-c4c9-49b6-bd73-9f891ca98e4d').
narrative_ontology:cs_reading_relation('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', foundational, expression_juridically_choate).
narrative_ontology:cs_axiom_status(expression_juridically_choate, holdable).
narrative_ontology:cs_axiom_grounding('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', expression_juridically_choate, conventional).
narrative_ontology:cs_axiom('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', foundational, copy_right_distinct_from_guild_privilege).
narrative_ontology:cs_axiom_status(copy_right_distinct_from_guild_privilege, holdable).
narrative_ontology:cs_axiom_grounding('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', copy_right_distinct_from_guild_privilege, conventional).
narrative_ontology:cs_reference_frame('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', statute_of_anne_limited_term).
narrative_ontology:cs_drift_state('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', pre_donaldson_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51ffc751-c4c9-49b6-bd73-9f891ca98e4d', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, published_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, booksellers_and_printers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, unauthorized_reproducers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, reading_public).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, expression_as_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and amends the statutory framework that makes expression legally ownable; defines the term and scope of copy right; can alter the constraint by legislation but is lobbied by the book trade.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliament, agenda_setter,
    institutional, civilizational, mobile, national).

% Gain nominal statutory right in their expressions for limited terms; can assign or sell these rights to publishers; depend on publishers for printing and distribution; have limited bargaining power individually.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, published_authors, beneficiary,
    moderate, biographical, constrained, national).

% Adapt from guild monopoly to a market in tradable copy rights; purchase rights from authors and enforce them against competitors; the primary economic beneficiaries of the new category despite statutory limits.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, booksellers_and_printers, beneficiary,
    organized, generational, mobile, national).

% Reproduce books without license or statutory authorization; face civil and criminal penalties; Scottish and provincial printers fall into this category; structurally excluded from the legal market.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, unauthorized_reproducers, payer,
    powerless, immediate, trapped, regional).

% Purchase books at prices set by the rights-holding trade; lose access to cheap unauthorized editions; bear the cost of the new property regime without a direct voice in its design.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, reading_public, payer,
    powerless, generational, constrained, national).

% Judges and counsel who adjudicate disputes using the new vocabulary of copy right; develop the interpretive tradition that determines what counts as ownable expression.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_interpreters, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, booksellers_and_printers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a legal vocabulary in which disputes over the reproduction of expressive works can be adjudicated without resorting to guild privilege or censorship frameworks; makes 'copy right' a coherent juridical object distinct from printing monopoly.
% TRANSFER_FUNCTION: Moves control over expressive works from an unrestricted commons and guild monopoly to identified rights-holders, and moves licensing fees and higher purchase prices from readers and unauthorized copiers to authorized printers and authors.
% ABSENT_VOICES: Readers, annotators, and adaptors who treated texts as common material before 1710; non-metropolitan printers outside London; folk culture practitioners whose oral and expressive traditions were not captured by the 'ownable expression' frame.
% DISAPPEARANCE_RATIONALE: If the category of ownable expression vanished overnight, publishing contracts would lack a legal object, the author-publisher market would revert to raw guild power or unregulated copying, and the entire architecture of licensing and adjudication would reorganize around privilege or physical possession.
% FOUNDING_PROBLEM: Post-press and pre-1710 book trade was governed by the Stationers' Company monopoly and royal patent privileges, which concentrated control in a London guild and lacked a coherent legal category for authorial property; the Statute of Anne was constructed to break that monopoly and encourage learning by vesting rights in authors for limited terms.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary preamble attests the encouragement of learning. Economic historians (Feather, Patterson) corroborate the break from Stationers' monopoly. However, the publishers who became beneficiaries attest the problem differently than the excluded public and Scottish printers would; no corroboration exists from outside the benefiting trade and legislative actors.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).
:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the category enables genuine market coordination but also transfers value from readers and unauthorized copiers to the licensed trade. Suppression (0.5) reflects the active enforcement required to maintain the category against unauthorized reproduction. Theater ratio (0.25) captures the 'encouragement of learning' rhetoric that partially masks publisher capture. Accessibility collapse (0.6) is high because once the category exists, non-licensed copying becomes legally unintelligible. Resistance (0.4) comes from Scottish printers, unauthorized reprinters, and consumers of cheap editions. The measurement series show extraction, theater, and enforcement all rising as publishers consolidate control during the first four decades of the statute.
 *
 * PERSPECTIVAL GAP:
 *   The booksellers_and_printers seat experiences the constraint as enabling market order and tradable assets; the reading_public and unauthorized_reproducers seats experience it as enclosure and penalization. The engine computes this divergence from the same structural data: low directionality for the organized trade (beneficiary, mobile exit) and high directionality for trapped and constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the licensed book trade are structural beneficiaries (low d): they set and profit from the category. Published_authors are nominal beneficiaries but structurally constrained (moderate d). Unauthorized_reproducers and the reading_public are the targets (high d): they bear the costs of restricted access and enforcement. Legal_interpreters sit at analytical distance (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint could be misread as a pure rope (legal categories as neutral coordination) or a pure snare (copyright as pure publisher extraction). The classification gates require both genuine coordination (the category does solve a real adjudication problem) and declared victims (the public and unauthorized copiers bear asymmetric costs). This prevents either romanticizing the category as pure public coordination or reducing it to mere extraction â the engine detects the hybrid from the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_occupancy,
    'Does category emergence (thinkability) entail a change in who holds rights, or can the conceptual coherence exist independently of the first-holding mechanism?',
    'Comparative legal history: examine jurisdictions where expression was thinkable as property before authors were recognized as the primary rights-holders.',
    'If thinkability and first-holding are separable, this reading''s epsilon should be decomposed into two constraints; if inseparable, the thinkability reading absorbs the first-holding reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_occupancy, conceptual, 'Whether conceptual coherence and rights-holder occupancy are structurally independent.').

omega_variable(
    category_constructedness,
    'Is the legal coherence of ''ownable expression'' a constructed juridical artifact or a discovered logical entailment of authorship?',
    'Genealogy of legal concepts: trace whether ''copy right'' was invented or excavated in 1710 jurisprudence.',
    'If constructed, the constraint''s suppression and theater scores should rise (more active enforcement needed to sustain an artificial category); if discovered, they should fall (natural-law resonance reduces enforcement need).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_constructedness, conceptual, 'Whether the IP category is constructed or discovered.').

omega_variable(
    synchronic_diachronic_status,
    'Is the distinction between pre-1710 and post-1710 a real structural break or a temporal framing artifact that collapses under M4/M5 analysis?',
    'Apply the collapse test: check whether pre-1710 guild privilege and post-1710 copy right share the same structural epsilon when measured by identical observables.',
    'If the seam collapses, the thinkability reading''s epsilon is not independent of the synchronic reading, and the kernel should be reclassified as a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synchronic_diachronic_status, empirical, 'Whether the 1710 break is a real structural seam or a temporal artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_think_tr_t0, ip_category_emergence__thinkability_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ip_think_tr_t10, ip_category_emergence__thinkability_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(ip_think_tr_t20, ip_category_emergence__thinkability_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ip_think_tr_t30, ip_category_emergence__thinkability_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(ip_think_tr_t40, ip_category_emergence__thinkability_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(ip_think_be_t0, ip_category_emergence__thinkability_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ip_think_be_t10, ip_category_emergence__thinkability_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ip_think_be_t20, ip_category_emergence__thinkability_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ip_think_be_t30, ip_category_emergence__thinkability_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(ip_think_be_t40, ip_category_emergence__thinkability_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ip_think_su_t0, ip_category_emergence__thinkability_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ip_think_su_t10, ip_category_emergence__thinkability_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(ip_think_su_t20, ip_category_emergence__thinkability_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ip_think_su_t30, ip_category_emergence__thinkability_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(ip_think_su_t40, ip_category_emergence__thinkability_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ip_category_emergence kernel, which decomposes into three structurally distinct claims: thinkability (category coherence), first-holding (authorial occupancy), and synchronic/diachronic seam (temporal framing artifact). Each reading carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
