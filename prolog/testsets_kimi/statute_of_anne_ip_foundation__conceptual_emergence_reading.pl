% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne â Conceptual Emergence of Limited Copyright for Learning
 *   domain: legal/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as creating a new conceptual
 *   space in which copyright functions as a limited regulatory tool for
 *   promoting learning, rather than as perpetual property. Under this
 *   reading, the statute's primary structural achievement is cognitive and
 *   legal: it makes 'intellectual property' thinkable as a time-bounded
 *   statutory grant distinct from physical property or perpetual guild
 *   privilege. The beneficiary of this arrangement is the reading public (and
 *   authors who gain statutory standing), while the victim is the perpetual
 *   monopoly interest embodied by the Stationers' Company. This constraint
 *   story treats the statute's conceptual framing as the operative constraint
 *   â the cognitive architecture that enables the limited-term regime and
 *   suppresses perpetual-property claims.
 *
 * KEY AGENTS:
 *   - Parliament (agenda setter): Enacted the 1710 statute, establishing limited terms and the learning rationale.
 *   - Stationers' Company (payer/victim): Lost perpetual monopoly control over English printing; bears the cost of the new limitation.
 *   - Reading public (beneficiary): Gains guaranteed eventual access to works and cheaper learning materials.
 *   - Authors (beneficiary): Gained new statutory rights but limited to fixed terms.
 *   - Legal commentators (observer): Developed the analytical vocabulary that made IP a distinct legal category.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.45).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.6).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne â Conceptual Emergence of Limited Copyright for Learning").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '5ee35556-75e8-4857-9bd0-969c22123141').
narrative_ontology:cs_kernel_codification('5ee35556-75e8-4857-9bd0-969c22123141', formalized).
narrative_ontology:cs_authority_grounding('5ee35556-75e8-4857-9bd0-969c22123141', lineage).
narrative_ontology:cs_interpretation_layer_present('5ee35556-75e8-4857-9bd0-969c22123141').
narrative_ontology:cs_reading_relation('5ee35556-75e8-4857-9bd0-969c22123141', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_reading_relation('5ee35556-75e8-4857-9bd0-969c22123141', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('5ee35556-75e8-4857-9bd0-969c22123141', foundational, copyright_as_limited_regulatory_tool).
narrative_ontology:cs_axiom_status(copyright_as_limited_regulatory_tool, holdable).
narrative_ontology:cs_axiom_grounding('5ee35556-75e8-4857-9bd0-969c22123141', copyright_as_limited_regulatory_tool, instrumental).
narrative_ontology:cs_axiom('5ee35556-75e8-4857-9bd0-969c22123141', foundational, public_learning_as_primary_beneficiary).
narrative_ontology:cs_axiom_status(public_learning_as_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('5ee35556-75e8-4857-9bd0-969c22123141', public_learning_as_primary_beneficiary, deontological).
narrative_ontology:cs_reference_frame('5ee35556-75e8-4857-9bd0-969c22123141', limited_statutory_grant_for_learning).
narrative_ontology:cs_drift_state('5ee35556-75e8-4857-9bd0-969c22123141', post_donaldson_beckett_1774, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ee35556-75e8-4857-9bd0-969c22123141', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Statute of Anne in 1710, dissolving the Stationers' de facto perpetual monopoly and replacing it with a limited-term statutory grant justified as a means to promote learning and useful knowledge.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the statutory guarantee that printed works will enter the public domain after fixed terms, expanding access to affordable learning materials and preventing perpetual lock-in of knowledge.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    organized, generational, constrained, national).

% Gains a new statutory right to control reproduction for limited terms, replacing the Stationers' customary ownership of copy; the right is bounded by term limits and often assigned to publishers through contract.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Previously exercised a de facto perpetual monopoly over English printing through royal patents, guild regulation, and customary copy-right; loses the ability to claim perpetual control as the statute imposes fixed terms and opens the trade to competitors upon expiration.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    powerful, biographical, constrained, national).

% Interprets and records the conceptual shift from perpetual guild privilege to limited statutory right, supplying the analytical vocabulary in which intellectual property becomes a distinct legal and economic category.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_commentators, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balance incentives for authorship by granting limited exclusive rights while ensuring works eventually enter the public domain, creating a sustainable market for new works without perpetual private lock-in of knowledge.
% TRANSFER_FUNCTION: Moves control over the reproduction and sale of printed works from a perpetual guild monopoly held by the Stationers' Company to authors for limited statutory terms, with ultimate transfer to the public domain.
% ABSENT_VOICES: Common-law advocates of perpetual copy-right based on natural property principles; provincial and colonial printers excluded from London's regulatory framework; later Romantic-era authors who would claim perpetual moral rights independent of statutory limits.
% DISAPPEARANCE_RATIONALE: Without the conceptual framework of limited copyright, the English book trade would revert to guild-monopoly control, the public domain would cease to exist as a legal category, and the modern distinction between intellectual property and physical property would collapse.
% FOUNDING_PROBLEM: The Stationers' Company exercised a de facto perpetual monopoly over English printing, suppressing competition and public access; there was no statutory mechanism to incentivize new authorship or guarantee affordable learning materials.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and legal scholars (e.g., Patterson, Rose) document the Stationers' monopoly pricing and guild control from outside the beneficiary parties; contemporary pamphleteers and non-guild booksellers corroborate the access problem.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45-0.56) reflects the moderate but persistent extraction from the Stationers' Company of their previous perpetual monopoly surplus; the constraint is not maximally extractive because it coordinates a genuine market for authorship. Suppression (0.68 declining to 0.30) captures the active force needed to defeat common-law perpetual copyright claims, which diminished after judicial confirmation of limited terms. Theater ratio (0.18-0.25) is low because the learning rationale was structurally operative, though parliamentary rhetoric contained some performative justification. Accessibility collapse (0.70) is high because once the limited-term framework was legally settled, perpetual-property alternatives became almost unthinkable within English jurisprudence. Resistance (0.55) reflects the Stationers' sustained litigation and lobbying against term limits. The founding problem (Stationers' monopoly) is dead, but the arrangement evolved into a settled legal framework rather than expiring.
 *
 * PERSPECTIVAL GAP:
 *   From the Stationers' seat, the constraint is pure extraction: it destroys a property-like entitlement they had enjoyed for generations. From the reading public's seat, it is coordination: it creates a predictable path to public access and lowers barriers to learning. From the author's seat, it is mixed: new rights are granted, but their limitation is a cost. The engine computes these divergences from the structural data without adjudicating which seat is 'correct'.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament (agenda setter) sits near the beneficiary end: it does not collect rents but structures the coordination. The Stationers' Company is the primary target (high d): the constraint is designed to terminate their perpetual extraction. The reading public and authors are net beneficiaries (low d): they receive the coordination surplus. Legal commentators are analytical (d â 0.5, symmetric observation). No overrides are needed because the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by distinguishing the conceptual innovation (limited regulatory tool) from the institutional reallocation of rights. If the founding problem were misidentified as 'authors need stronger rights,' the constraint might be misread as a rope for authors. Instead, the founding problem is the Stationers' monopoly, and the constraint's persistence after 1774 is a conceptual settlement, not a piton â the theater ratio is low and the coordination function remains structurally operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_institutional_separability,
    'Is the conceptual emergence of ''limited copyright for learning'' separable from the institutional reallocation of rights from Stationers to authors, or are they analytically inseparable dimensions of a single event?',
    'Archival historiography examining whether parliamentary debates and pamphlet literature treated the conceptual innovation as distinct from the power shift, or as necessarily co-emergent.',
    'If inseparable, this reading overstates the autonomy of the conceptual dimension and should be subsumed under the entangled_event reading; if separable, the conceptual emergence reading captures a genuine structural novelty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether conceptual and institutional change in the Statute of Anne are separable.').

omega_variable(
    stationer_capture_of_author_rights,
    'Did authors actually benefit from the new statutory rights, or did the Stationers'' Company capture these rights through standard assignment contracts, preserving their effective monopoly?',
    'Empirical analysis of eighteenth-century publishing contracts and copyright assignments to determine whether authors or publishers controlled the statutory term in practice.',
    'If Stationers captured author rights through contract, the public-learning beneficiary structure is weakened and the constraint functions more as institutional reallocation than conceptual innovation for learning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationer_capture_of_author_rights, empirical, 'Whether Stationers captured statutory author rights through contract.').

omega_variable(
    perpetual_monopoly_natural_right_status,
    'Was the pre-1710 Stationers'' monopoly a natural common-law right unjustly extinguished, or a statutory privilege that had simply expired?',
    'Legal-historical examination of the origins of the Stationers'' copy-right claims in royal patent and guild regulation versus natural-law property theory.',
    'If the Stationers held a genuine natural right, the statute extracts unjustly from them; if their claim was purely statutory and conventional, the limitation is legitimate reallocation rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_monopoly_natural_right_status, conceptual, 'Nature of the pre-1710 monopoly right and its moral status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(stat_tr_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(stat_tr_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement(stat_tr_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 48, 0.23).
narrative_ontology:measurement(stat_tr_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 64, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stat_be_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(stat_be_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(stat_be_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement(stat_be_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 48, 0.5).
narrative_ontology:measurement(stat_be_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 64, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(stat_su_t12, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(stat_su_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(stat_su_t36, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(stat_su_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 48, 0.38).
narrative_ontology:measurement(stat_su_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 64, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is the conceptual_emergence_reading of the statute_of_anne_ip_foundation kernel. The kernel decomposes into three structurally distinct readings: conceptual_emergence (this file), institutional_reallocation, and entangled_event. Each reading assigns different epsilon and different beneficiary/victim structures to the same 1710 statute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
