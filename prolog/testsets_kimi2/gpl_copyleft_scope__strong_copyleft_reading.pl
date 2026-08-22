% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Reading: Dynamic Linking as Derivative Work
 *   domain: software licensing/intellectual property/open source governance
 *
 * SUMMARY:
 *   This constraint instantiates the strong copyleft reading of GPL Section
 *   2(b), which treats all forms of code couplingâincluding dynamic
 *   linkingâas creating a derivative work that must be licensed under the
 *   GPL. The reading is contested by a narrow-scope sibling (dynamic linking
 *   as mere aggregation) and an enforcement-vacuum sibling (constraint effect
 *   depends on who has litigation capacity). Under this reading, proprietary
 *   software vendors are structurally excluded from integrating GPL
 *   components without releasing their entire source codebase, while the
 *   free-software ecosystem receives a forced reciprocity guarantee. The FSF
 *   stewards the interpretation and enforcement infrastructure. Judicial
 *   systems worldwide have not provided definitive precedent, leaving the
 *   constraint's operation dependent on credible legal threat rather than
 *   settled law.
 *
 * KEY AGENTS:
 *   - fsf_and_stallman_legacy: Primary agenda-setter (institutional/analytical) â authors and enforces the strong interpretation
 *   - free_software_ecosystem: Primary beneficiary (organized/constrained) â gains source-code reciprocity guarantee
 *   - proprietary_vendors: Primary target (powerful/constrained) â bears extraction via forced source release or exclusion
 *   - commercial_integrators: Secondary target (moderate/constrained) â over-complies due to legal uncertainty
 *   - judicial_system: Analytical observer (institutional/analytical) â absence of definitive precedent sustains the vacuum
 *   - permissive_license_community: Excluded voice (organized/mobile) â advocates alternatives but is not in the FSF governance room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.82).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Reading: Dynamic Linking as Derivative Work").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software licensing/intellectual property/open source governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '1e71b5b2-8c6d-4e43-a6c4-ab162294ec27').
narrative_ontology:cs_kernel_codification('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', formalized).
narrative_ontology:cs_authority_grounding('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', lineage).
narrative_ontology:cs_interpretation_layer_present('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27').
narrative_ontology:cs_reading_relation('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', foundational, all_coupling_triggers_copyleft_obligation).
narrative_ontology:cs_axiom_status(all_coupling_triggers_copyleft_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', all_coupling_triggers_copyleft_obligation, conventional).
narrative_ontology:cs_axiom('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', foundational, proprietary_integration_defeats_commons).
narrative_ontology:cs_axiom_status(proprietary_integration_defeats_commons, holdable).
narrative_ontology:cs_axiom_grounding('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', proprietary_integration_defeats_commons, deontological).
narrative_ontology:cs_reference_frame('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', universal_source_reciprocity).
narrative_ontology:cs_drift_state('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', post_gplv3_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e71b5b2-8c6d-4e43-a6c4-ab162294ec27', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_ecosystem).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and stewards the GPL text, promotes the strong copyleft interpretation, and funds or coordinates legal actions to extend copyleft obligations to all forms of code coupling including dynamic linking. Derives institutional authority from continuity with the founding license text and its originator.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_and_stallman_legacy, agenda_setter,
    institutional, generational, analytical, global).

% Receives a structural guarantee that downstream integrators cannot proprietize contributions; benefits from source-code releases forced from proprietary vendors who choose compliance over exclusion. Dependent on the GPL network effect; migrating to permissive licensing would fragment reciprocity norms and prior contributions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_ecosystem, beneficiary,
    organized, generational, constrained, global).

% Commercial software vendors seeking to integrate or distribute GPL components without releasing their proprietary source code. Face credible litigation threats and compliance uncertainty under the strong reading. Must choose between costly source release, architectural exclusion of GPL code, clean-room reimplementation, or purchasing alternative proprietary licenses where available.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Small and mid-sized firms and systems integrators with limited legal resources to adjudicate derivative-work boundaries. Often over-comply by treating all dynamic linking as triggering copyleft, absorbing disproportionate compliance costs and forgoing integration opportunities that larger vendors might litigate or avoid through engineering resources.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% National courts and jurisprudence have not issued definitive precedent on whether dynamic linking creates a derivative work under copyright law in the context of the GPL. This interpretive vacuum allows the strong reading to operate through threat rather than settled adjudication.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, judicial_system, observer,
    institutional, civilizational, analytical, national).

% Advocates for BSD, MIT, Apache and other permissive licenses that permit proprietary reuse without source reciprocity. Structurally excluded from FSF-led governance forums and license interpretation processes; their preference for lower-friction collaboration is not represented in strong-copyleft agenda-setting.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, permissive_license_community, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_ecosystem).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents proprietary free-riding on communally developed code by requiring that any distributed work incorporating GPL-licensed components be available under the same license, thereby preserving a reciprocity loop that sustains contributor incentives and user freedoms.
% TRANSFER_FUNCTION: Moves the legal obligation to release complete corresponding source code from proprietary vendors and commercial integrators to the free-software ecosystem, transferring control over derivative and combined works.
% ABSENT_VOICES: Proprietary end-users who silently benefit from source availability but are not in the licensing conversation; judges who have not clarified the derivative-work boundary; and the permissive-license community, which is structurally excluded from FSF interpretive forums despite advocating viable alternative governance models.
% DISAPPEARANCE_RATIONALE: If the strong copyleft reading vanished overnight, proprietary vendors would integrate GPL components into closed systems without source release; the free-software ecosystem would lose its structural guarantee against proprietization; permissive licenses would likely dominate new projects; and the FSF's enforcement infrastructure would lose its primary target and ideological justification.
% FOUNDING_PROBLEM: In the 1980s, freely shared software was routinely appropriated by commercial entities who distributed improved binaries without source code, eroding the digital commons and disincentivizing collaborative development.
% FOUNDING_PROBLEM_CORROBORATION: Free-software historians and FSF advocates attest the free-rider problem was acute in early workstation and minicomputer cultures. Proprietary vendors and some empirical economists contest that the problem persists at founding severity, citing the success of permissive-ecosystem commons (e.g., Apache, BSD, Python) without mandatory reciprocity. No independent longitudinal audit conclusively resolves this dispute from outside the benefiting parties.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the strong reading leverages legal ambiguity to force source release across coupling boundaries that copyright doctrine traditionally treats as non-derivative. Suppression is high (0.82) because the constraint persists through credible litigation threat and the active exclusion of proprietary alternatives; alternatives (clean-room reimplementation, license purchase) are costly and imperfect. Theater is elevated (0.55) because a growing share of enforcement activity defends interpretive territory (dynamic linking) rather than user freedom per se. Accessibility collapse is high (0.75): once a developer understands the strong reading, the alternative paths (avoiding all GPL code, reimplementing) collapse to a narrow, expensive set. Resistance is moderate (0.60): proprietary vendors resist through license avoidance, lobbying, and creation of competing permissive ecosystems, but they cannot directly dismantle the GPL text itself.
 *
 * PERSPECTIVAL GAP:
 *   The FSF and free-software ecosystem experience this constraint as necessary defense of user freedom and communal reciprocity; the engine should compute their seat as low-extraction or beneficiary. Proprietary vendors and commercial integrators experience the same legal text as coercive extraction of their intellectual property; the engine should compute their seat as high-extraction target. The divergence is structural: the same license clause operates as guarantee to one seat and as snare to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (free_software_ecosystem) receive the extracted source code and freedom guarantees; their directionality is near the beneficiary pole. Victims (proprietary_vendors, commercial_integrators) bear the cost of forced source release or architectural exclusion; their directionality is near the target pole. The FSF, as agenda-setter, sits close to the beneficiary side though its primary gain is authority and ideological vindication rather than monetary rent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâproprietary free-riding on communal codeâwas genuine in the 1980s. Whether it persists at a severity justifying strong copyleft is contested. The constraint has not resolved into a piton because the beneficiary ecosystem actively maintains and enforces it, and the extraction remains credibly collectable. However, the elevated theater_ratio indicates some drift toward performative enforcement. The classification as snare rather than tangled_rope reflects the judgment that, for this specific reading, the coordination function (preventing free-riding) has become secondary to the extraction function (forcing source release from unwilling parties).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_linking_derivative_status,
    'Does dynamic linking, as a technical and legal matter, create a derivative work under copyright law such that the GPL strong reading is judicially enforceable?',
    'Definitive appellate or supreme-court precedent in a major jurisdiction, or legislative clarification of the derivative-work boundary for software linking.',
    'A negative precedent would deflate the strong reading''s extraction, shifting it toward a high-theater piton or a contested mountain of opinion; a positive precedent would entrench the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_linking_derivative_status, empirical, 'Judicial status of dynamic linking as derivative work').

omega_variable(
    enforcement_threat_vs_settled_law,
    'Is the constraint''s effect driven by actual judicial enforceability or by the credible threat of enforcement in a precedent vacuum?',
    'Systematic tracking of GPL litigation outcomes, settlement rates, and cost-of-defense estimates for targets.',
    'If enforcement relies primarily on threat rather than precedent, the suppression metric may overstate structural legal barrier and the true extraction is partly performative, raising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_threat_vs_settled_law, empirical, 'Whether extraction rests on enforceable precedent or threat').

omega_variable(
    free_rider_persistence,
    'Does the free-rider problem that founded the GPL still persist at a severity that justifies strong copyleft scope, or have alternative mechanisms (permissive ecosystems, corporate patronage, dual licensing) rendered it obsolete?',
    'Longitudinal empirical comparison of contributor sustainability, code-fork proliferation, and corporate capture rates across copyleft and permissive ecosystems.',
    'If the founding problem is dead, the constraint is a candidate for mandatrophy reclassification; if live, the coordination story retains legitimacy alongside the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_rider_persistence, conceptual, 'Whether the founding free-rider rationale remains live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t7, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 7, 0.22).
narrative_ontology:measurement(gpl__tr_t14, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(gpl__tr_t21, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 21, 0.48).
narrative_ontology:measurement(gpl__tr_t28, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 28, 0.52).
narrative_ontology:measurement(gpl__tr_t35, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 35, 0.55).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gpl__be_t7, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 7, 0.61).
narrative_ontology:measurement(gpl__be_t14, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 14, 0.74).
narrative_ontology:measurement(gpl__be_t21, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 21, 0.82).
narrative_ontology:measurement(gpl__be_t28, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 28, 0.79).
narrative_ontology:measurement(gpl__be_t35, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 35, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t7, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 7, 0.42).
narrative_ontology:measurement(gpl__su_t14, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement(gpl__su_t21, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 21, 0.8).
narrative_ontology:measurement(gpl__su_t28, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 28, 0.76).
narrative_ontology:measurement(gpl__su_t35, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 35, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
