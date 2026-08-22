% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override of Eternal Marriage Covenant
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint describes the operation of the continuing revelation
 *   doctrine within a specific religious tradition, particularly as it
 *   enabled the living prophet to issue the 1890 Manifesto, effectively
 *   suspending the practice of plural marriage. This reading emphasizes the
 *   dynamic authority of the living prophet to supersede prior revelation
 *   when institutional survival is at stake, driven by external pressures. It
 *   is a Tangled Rope because it solves a coordination problem (church
 *   survival) through asymmetric extraction (from adherents of the prior
 *   practice) and requires active enforcement (excommunication, social
 *   pressure).
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda-setter (institutional/identity_locked) — issues and enforces new revelation.
 *   - church_institution: Beneficiary (institutional/identity_locked) — survives and thrives due to adaptation.
 *   - polygamous_adherents: Payer (powerless/identity_locked) — compelled to abandon practice, facing severe personal costs.
 *   - dissident_factions: Payer (organized/constrained) — excommunicated for non-compliance, forming separate groups.
 *   - federal_government: Agenda-setter (institutional/mobile) — external force compelling the override.
 *   - general_membership: Beneficiary (moderate/constrained) — benefits from institutional stability, avoids persecution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.78).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '4b824e22-662a-47f2-9263-4760a75c1185').
narrative_ontology:cs_kernel_codification('4b824e22-662a-47f2-9263-4760a75c1185', formalized).
narrative_ontology:cs_authority_grounding('4b824e22-662a-47f2-9263-4760a75c1185', lineage).
narrative_ontology:cs_interpretation_layer_present('4b824e22-662a-47f2-9263-4760a75c1185').
narrative_ontology:cs_reading_relation('4b824e22-662a-47f2-9263-4760a75c1185', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('4b824e22-662a-47f2-9263-4760a75c1185', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('4b824e22-662a-47f2-9263-4760a75c1185', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('4b824e22-662a-47f2-9263-4760a75c1185', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('4b824e22-662a-47f2-9263-4760a75c1185', secondary, institutional_survival_is_paramount).
narrative_ontology:cs_axiom_status(institutional_survival_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('4b824e22-662a-47f2-9263-4760a75c1185', institutional_survival_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('4b824e22-662a-47f2-9263-4760a75c1185', dynamic_prophetic_authority).
narrative_ontology:cs_drift_state('4b824e22-662a-47f2-9263-4760a75c1185', contemporary_church_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4b824e22-662a-47f2-9263-4760a75c1185', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamous_adherents).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissident_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, general_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The living prophet and apostles who receive and declare new revelation, including the 1890 Manifesto. They maintain institutional survival and doctrinal coherence by adapting practices while preserving core tenets. Their authority is paramount in this reading.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% The organizational body of the church, which benefits from avoiding federal prosecution, maintaining legal status, and ensuring its long-term survival and growth. The prophetic override directly enabled its continued existence in the United States.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, civilizational, identity_locked, global).

% Members who had entered into or believed in the practice of plural marriage as an eternal commandment. They were compelled to abandon the practice or face excommunication, legal penalties, and social ostracization. Their spiritual and social identities were deeply tied to the prior revelation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, polygamous_adherents, payer,
    powerless, biographical, identity_locked, local).

% Groups that refused to accept the prophetic override, viewing it as a capitulation to secular authority and a betrayal of eternal doctrine. They faced excommunication and formed independent communities, bearing the costs of separation and continued legal persecution.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissident_factions, payer,
    organized, generational, constrained, regional).

% Exerted legal and political pressure, including confiscation of church property and imprisonment of polygamists, forcing the church to abandon the practice of plural marriage to secure statehood and institutional survival. Its actions triggered the prophetic override.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, agenda_setter,
    institutional, generational, mobile, national).

% The majority of church members who accepted the prophetic override, allowing them to remain in good standing with the church and avoid legal persecution. They benefited from the church's continued institutional stability and social acceptance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, general_membership, beneficiary,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's response to external legal and political pressure, allowing for the adaptation of religious practice to ensure institutional survival while maintaining the authority of living prophets.
% TRANSFER_FUNCTION: Transfers the authority to define and enforce marriage practices from a fixed, prior revelation to the living prophet, enabling the church to avoid legal dissolution and maintain its property and membership. The cost is borne by adherents of the prior practice.
% ABSENT_VOICES: The voices of those who believed the prior revelation was truly immutable and eternal, and that the prophetic override was a betrayal, were suppressed or marginalized within the mainstream church. Their dissent led to excommunication and the formation of separate, often persecuted, communities.
% DISAPPEARANCE_RATIONALE: If the doctrine of continuing revelation and prophetic override vanished, the church would face an existential crisis when confronted with external pressures that contradict prior revelations. It would either fracture into immutable-doctrine factions or be forced to abandon core tenets, fundamentally altering its structure and survival strategy.
% FOUNDING_PROBLEM: The church faced severe legal and political persecution from the United States government due to its practice of plural marriage, threatening its existence, property, and the freedom of its members.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court documents, and contemporary academic analyses (outside the church's official narrative) corroborate that the immediate threat of federal intervention and institutional dissolution was the direct cause for the prophetic override. The specific legal threat is no longer live, though the principle of adapting to external pressure remains.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the override demanded a fundamental change in deeply held practices and beliefs from a segment of the membership, with severe consequences for non-compliance. Suppression (0.78) is also high, reflecting the institutional power to excommunicate and socially ostracize those who resisted, alongside the federal government's legal enforcement. The theater ratio (0.40) indicates that while the 'eternal principle' of plural marriage was not officially renounced, its practical suspension involved a degree of performative adaptation to external demands, maintaining a facade of continuity while fundamentally altering practice. The claimed type is 'rope' by the church's internal framing (coordinating divine will), but the metrics and structural analysis point to 'tangled_rope' due to the clear extraction from adherents and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership and the institution, the prophetic override was a necessary and divinely guided act of coordination for survival. From the perspective of polygamous adherents and dissident factions, it was an act of institutional extraction and suppression, forcing them to abandon a sacred covenant under duress. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and the institution are clear beneficiaries (d near 0.0) as the constraint ensures their survival and authority. The federal government, while an external actor, acts as an agenda-setter whose pressure directly benefits the church's institutional continuity. Polygamous adherents and dissident factions are targets (d near 1.0) as they bear the direct costs of compliance or excommunication. The general membership is a beneficiary, gaining stability at a lower personal cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival) was resolved by the override, but the mechanism of prophetic override itself became a precedent for future adaptations. The 'founding_problem_status' being 'dead' while 'disappearance_verdict' is 'world_rearranges' indicates that the specific problem is gone, but the constraint (the doctrine of continuing revelation) persists and is vital for the church's ongoing function, suggesting a potential for future mandatrophy if the mechanism is used for less existential threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_institutional_survival,
    'Was the prophetic override a genuine act of divine will, or primarily an instrumental response to federal pressure for institutional survival?',
    'Analysis of internal church records and prophetic statements from the period, compared with external historical accounts of federal pressure and legal threats. Examination of subsequent prophetic pronouncements for similar patterns of ''revelation'' coinciding with external challenges.',
    'If primarily instrumental, the ''divine will'' framing functions as a legitimizing cover story for an extractive institutional decision, increasing the effective extractiveness and suppression from the perspective of those who bore the costs. If genuinely divine, the extraction is reframed as a test of faith.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_survival, conceptual, 'Ambiguity between theological justification and pragmatic institutional response.').

omega_variable(
    identity_lock_mechanism,
    'To what extent was the ''identity_locked'' exit option for polygamous adherents a result of internalized belief versus structural coercion?',
    'Longitudinal studies of excommunicated polygamist communities: if adherence to the practice persists across generations despite severe external costs, it suggests a strong internalized identity lock. If abandonment is widespread after initial coercion, it points to structural suppression.',
    'If internalized, the suppression is more profound and persistent, as individuals carry the constraint within their self-concept. If purely structural, removing the external coercion would lead to a more rapid dissolution of the constraint''s power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').

omega_variable(
    scope_of_prophetic_authority,
    'What are the implicit boundaries of the prophetic override? Can it supersede any prior revelation, or only those deemed ''temporal'' or ''non-essential''?',
    'Analysis of subsequent prophetic pronouncements and doctrinal interpretations regarding other ''eternal'' commandments. Examination of internal church debates or dissenting theological arguments regarding the scope of prophetic authority.',
    'If the override is unlimited, it implies a highly dynamic and potentially arbitrary commitment system, increasing uncertainty for adherents. If limited, it suggests a more stable kernel, but raises questions about the criteria for limitation and who adjudicates them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_prophetic_authority, conceptual, 'Theological and practical limits on the power of continuing revelation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(eter_tr_t1894, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1894, 0.35).
narrative_ontology:measurement(eter_tr_t1898, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1898, 0.38).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.4).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(eter_be_t1894, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1894, 0.6).
narrative_ontology:measurement(eter_be_t1898, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1898, 0.63).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(eter_su_t1894, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1894, 0.75).
narrative_ontology:measurement(eter_su_t1898, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1898, 0.77).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, federal_anti_polygamy_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel, focusing on the prophetic override. It is linked to sibling readings that emphasize the immutability of the commandment or its temporal accommodation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
