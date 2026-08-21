% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Exogenous Coercion
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto, which officially
 *   suspended the practice of plural marriage in the Church of Jesus Christ
 *   of Latter-day Saints, from the perspective that it was a direct result of
 *   overwhelming federal coercion rather than an internal doctrinal
 *   reinterpretation. The federal government's actions, including
 *   imprisonment and property confiscation, are seen as forcing the
 *   abandonment of a divine requirement. This reading frames the Manifesto as
 *   a snare, where the coordination narrative (church's survival) masks a
 *   coercive extraction of religious autonomy.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda setter (institutional/arbitrage) — imposed coercion
 *   - practicing_polygamists: Payer (powerless/trapped) — bore the brunt of coercion
 *   - church_leadership_post_manifesto: Beneficiary/Agenda setter (institutional/constrained) — capitulated under duress, gained institutional survival
 *   - religious_freedom_advocates: Payer (moderate/constrained) — argued against federal overreach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.85).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Exogenous Coercion").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '01c04927-1886-447e-b678-b7751930592d').
narrative_ontology:cs_kernel_codification('01c04927-1886-447e-b678-b7751930592d', fixed_text).
narrative_ontology:cs_authority_grounding('01c04927-1886-447e-b678-b7751930592d', extraction).
narrative_ontology:cs_interpretation_layer_present('01c04927-1886-447e-b678-b7751930592d').
narrative_ontology:cs_reading_relation('01c04927-1886-447e-b678-b7751930592d', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('01c04927-1886-447e-b678-b7751930592d', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('01c04927-1886-447e-b678-b7751930592d', foundational, divine_command_immutable_by_secular_force).
narrative_ontology:cs_axiom_status(divine_command_immutable_by_secular_force, holdable).
narrative_ontology:cs_axiom_grounding('01c04927-1886-447e-b678-b7751930592d', divine_command_immutable_by_secular_force, deontological).
narrative_ontology:cs_axiom('01c04927-1886-447e-b678-b7751930592d', foundational, federal_power_cannot_dictate_religious_doctrine).
narrative_ontology:cs_axiom_status(federal_power_cannot_dictate_religious_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('01c04927-1886-447e-b678-b7751930592d', federal_power_cannot_dictate_religious_doctrine, deontological).
narrative_ontology:cs_reference_frame('01c04927-1886-447e-b678-b7751930592d', divine_mandate_supremacy).
narrative_ontology:cs_drift_state('01c04927-1886-447e-b678-b7751930592d', post_manifesto_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('01c04927-1886-447e-b678-b7751930592d', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, church_leadership_post_manifesto).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, religious_freedom_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposed severe legal and economic penalties (imprisonment, property seizure) to force the abandonment of plural marriage, viewing it as a challenge to federal authority and social norms. Benefited from achieving territorial conformity and asserting federal supremacy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faced imprisonment, loss of property, and social ostracization for continuing to practice plural marriage, which they believed was a divine commandment. Their options were to abandon their religious practice or endure severe state coercion.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, local).

% Issued the Manifesto, which this reading interprets as a forced capitulation to federal power. Benefited from the cessation of federal persecution, allowing the church to achieve statehood and integrate into mainstream American society, albeit at the cost of a core doctrine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_leadership_post_manifesto, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_leadership_post_manifesto, agenda_setter).

% Argued against federal overreach into religious practice, viewing the coercion as a violation of constitutional rights. Their efforts were largely unsuccessful against the overwhelming power of the federal government, bearing the cost of a diminished scope for religious liberty.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, religious_freedom_advocates, payer,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto, under this reading, did not solve an internal coordination problem but rather coordinated the church's practices with federal legal and social norms, under duress, to avoid further persecution.
% TRANSFER_FUNCTION: Transferred the authority over religious practice from the church's divine mandate to federal legal supremacy, and transferred the social and economic costs of non-conformity from the federal government to individual polygamists and the church as an institution.
% ABSENT_VOICES: Those who believed plural marriage was an unchangeable divine commandment and refused to abandon it were systematically silenced, excommunicated, or forced into hiding. Their voices were excluded from the official narrative of 'voluntary' compliance.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its coercive context vanished, the historical narrative of the church's adaptation would be fundamentally altered. The federal government's assertion of authority over religious practice would be seen as illegitimate, and the trajectory of religious freedom in the US might have been different. The church's internal doctrinal development would also be re-evaluated.
% FOUNDING_PROBLEM: The federal government viewed plural marriage as an immoral practice and an obstacle to Utah's statehood and the integration of the Latter-day Saint community into American society. The church faced existential threats from federal legislation and enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions (e.g., Reynolds v. United States), and contemporary accounts from both federal officials and dissenting church members corroborate the intense federal pressure and the existential threat faced by the church. This external corroboration supports the view that the problem was resolved through coercion, not internal doctrinal evolution.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85 at the Manifesto's issuance) because the federal government successfully compelled the abandonment of a deeply held religious practice, extracting conformity and asserting its authority. Suppression is extremely high (0.92) due to the severe legal and economic penalties, effectively eliminating alternatives for practicing polygamists. The theater ratio is high (0.70) because the 'voluntary' nature of the Manifesto was largely a performance to satisfy federal demands, masking the underlying coercion. Resistance was significant but ultimately overwhelmed by state power.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, the Manifesto was a legitimate assertion of secular law over a perceived social deviance. From the perspective of practicing polygamists, it was a devastating act of religious persecution. The church leadership's perspective, while acknowledging coercion, often emphasized the 'revelatory' aspect to maintain internal legitimacy. This story aligns with the victim's perspective, highlighting the coercive nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a clear beneficiary (d=0.0-0.1) as it achieved its policy goals and asserted national sovereignty. Practicing polygamists are full targets (d=0.9-1.0) as they bore the direct costs of forced abandonment. Church leadership, while appearing to be an agenda-setter, also acted as a constrained beneficiary (d=0.2-0.3) by securing the institution's survival, albeit under duress. Religious freedom advocates were targets (d=0.7-0.8) as their principles were undermined.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Manifesto as a 'rope' (pure coordination) or 'scaffold' (temporary support for transition) by emphasizing the coercive mechanisms and the identifiable victims. It highlights that the 'coordination' achieved was primarily through forced compliance, not mutual benefit, and that the 'transition' was imposed, not voluntarily chosen. The high theater ratio indicates that the public narrative of voluntary compliance was largely performative, masking the underlying snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_authenticity,
    'Was plural marriage truly a divine requirement, or was it a mutable doctrine?',
    'Theological and historical analysis of scriptural interpretation and prophetic authority within the religious tradition, acknowledging that this is a faith-based question not resolvable by empirical means alone.',
    'If definitively mutable, the federal coercion might be re-framed as a catalyst for a legitimate doctrinal evolution rather than an override. If immutable, the coercion is a direct violation of religious freedom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_authenticity, conceptual, 'Ambiguity regarding the immutability of the plural marriage doctrine.').

omega_variable(
    degree_of_agency_in_manifesto,
    'To what extent did church leadership retain agency in issuing the 1890 Manifesto, despite federal pressure?',
    'Detailed historical analysis of internal church deliberations, communications with federal authorities, and alternative strategies considered, weighing the perceived existential threat against available options.',
    'Greater perceived agency would shift the classification closer to a ''tangled_rope'' or ''scaffold'' (forced coordination with some internal buy-in), while minimal agency reinforces the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_agency_in_manifesto, empirical, 'The extent of voluntary choice vs. pure coercion in the Manifesto''s issuance.').

omega_variable(
    long_term_doctrinal_impact,
    'Did the exogenous override fundamentally alter the church''s theological understanding of revelation and authority, or was it a temporary pragmatic adjustment?',
    'Analysis of post-Manifesto doctrinal development, theological writings, and official statements regarding the nature of revelation and the relationship between divine command and secular law.',
    'If fundamental alteration, the ''snare'' classification''s long-term effects are more severe. If temporary, the constraint might be seen as a ''scaffold'' that enabled a return to core principles, albeit in a modified form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_doctrinal_impact, conceptual, 'The lasting theological consequences of the forced abandonment of plural marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1880, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1880, 0.3).
narrative_ontology:measurement(plur_tr_t1885, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1885, 0.5).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.7).
narrative_ontology:measurement(plur_tr_t1895, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1895, 0.65).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1900, 0.6).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1880, 0.6).
narrative_ontology:measurement(plur_be_t1885, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1885, 0.7).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(plur_be_t1895, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1895, 0.8).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1900, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(plur_su_t1885, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1885, 0.85).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.92).
narrative_ontology:measurement(plur_su_t1895, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1895, 0.88).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1900, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, utah_statehood_process).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, religious_freedom_precedent_in_us).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
