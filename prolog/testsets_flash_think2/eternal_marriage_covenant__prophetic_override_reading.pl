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
 *   human_readable: Continuing Revelation Doctrine: Prophetic Override Reading
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'prophetic override' reading of
 *   the eternal marriage covenant kernel. This reading posits that the
 *   doctrine of continuing revelation allows the living prophet to issue new
 *   revelations that can supersede or modify prior divine commands,
 *   particularly when external circumstances (like federal pressure against
 *   polygamy) necessitate institutional adaptation for survival. The
 *   constraint functions as a mechanism for the church to navigate external
 *   challenges while maintaining its claim to divine authority, but at a
 *   significant cost to members who must reconcile conflicting truths.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation Doctrine: Prophetic Override Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '0766cfe0-f007-40d3-ac59-453cebc1908f').
narrative_ontology:cs_kernel_codification('0766cfe0-f007-40d3-ac59-453cebc1908f', formalized).
narrative_ontology:cs_authority_grounding('0766cfe0-f007-40d3-ac59-453cebc1908f', lineage).
narrative_ontology:cs_interpretation_layer_present('0766cfe0-f007-40d3-ac59-453cebc1908f').
narrative_ontology:cs_reading_relation('0766cfe0-f007-40d3-ac59-453cebc1908f', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('0766cfe0-f007-40d3-ac59-453cebc1908f', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('0766cfe0-f007-40d3-ac59-453cebc1908f', foundational, continuing_revelation_supersedes_prior_practice).
narrative_ontology:cs_axiom_status(continuing_revelation_supersedes_prior_practice, holdable).
narrative_ontology:cs_axiom_grounding('0766cfe0-f007-40d3-ac59-453cebc1908f', continuing_revelation_supersedes_prior_practice, theological).
narrative_ontology:cs_axiom('0766cfe0-f007-40d3-ac59-453cebc1908f', secondary, institutional_survival_justifies_doctrinal_adaptation).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_doctrinal_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('0766cfe0-f007-40d3-ac59-453cebc1908f', institutional_survival_justifies_doctrinal_adaptation, instrumental).
narrative_ontology:cs_reference_frame('0766cfe0-f007-40d3-ac59-453cebc1908f', prophetic_guidance_for_church_survival).
narrative_ontology:cs_drift_state('0766cfe0-f007-40d3-ac59-453cebc1908f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0766cfe0-f007-40d3-ac59-453cebc1908f', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, devout_members_adhering_to_prior_revelation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the living prophet and apostles, they are the sole recipients and interpreters of continuing revelation. They exercise the authority to supersede prior revelations, particularly when external pressures threaten the institution. They benefit from the doctrine's flexibility in ensuring church survival and maintaining their authority.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% The church as an organization benefits from the doctrine's ability to adapt to legal and social changes, preventing schism, persecution, or dissolution. This ensures its long-term survival and growth, even if it means altering foundational practices.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, civilizational, arbitrage, global).

% These members have deeply internalized prior revelations as immutable divine commands. The prophetic override requires them to reconcile conflicting truths, often leading to spiritual distress, cognitive dissonance, and the abandonment of deeply held personal practices or beliefs. Their identity is fused with the church, making exit extremely costly.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, devout_members_adhering_to_prior_revelation, payer,
    powerless, biographical, identity_locked, local).

% Members who openly question or resist the prophetic override, often facing social ostracism, ecclesiastical discipline, or excommunication. They bear the cost of non-compliance, but their identity lock is weaker than devout members, allowing for a more constrained exit.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissenting_members, payer,
    powerless, biographical, constrained, local).

% Exerts external pressure (e.g., legal sanctions, anti-polygamy laws) that necessitates the church's adaptation. While not directly part of the church's internal doctrine, its actions activate the prophetic override mechanism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Observes the church's doctrinal shifts and their social implications. Its evolving norms and legal frameworks often serve as the 'circumstances' that require new revelation, influencing the church's internal dynamics.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, secular_society, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the church to adapt its practices and interpretations of divine law to external legal and social pressures, ensuring institutional survival and maintaining a unified, compliant membership in a changing world.
% TRANSFER_FUNCTION: Transfers the burden of reconciling conflicting divine commands and adapting to new directives from the church institution to individual members, who must adjust their beliefs and practices.
% ABSENT_VOICES: Former members who left due to an inability to reconcile the changes, or those excommunicated for dissent. Their narratives of spiritual trauma and loss of community are often marginalized within the church's official discourse.
% DISAPPEARANCE_RATIONALE: If the doctrine of continuing revelation and prophetic override vanished, the church would face an existential crisis when its foundational revelations conflicted with secular law or social norms. It would be forced to either defy external authority (leading to persecution or dissolution) or abandon its claim to divine guidance (leading to schism), fundamentally reorganizing its structure and relationship with its members.
% FOUNDING_PROBLEM: How to maintain a divinely guided institution and its unique practices (e.g., polygamy) in a rapidly changing secular society, particularly when divine commands conflict with prevailing laws and social expectations.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion, sociologists studying new religious movements, and legal scholars examining church-state relations attest to the ongoing tension between religious doctrine and secular society, confirming the problem's continued relevance. Official church histories also document the historical pressures that led to the doctrine's application.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high because the doctrine demands significant adaptation and potential spiritual distress from members, who must abandon or reinterpret deeply held beliefs and practices. Suppression (0.75) is also high, stemming from the spiritual authority of the prophet and the social pressure within the community to conform; dissent can lead to excommunication. Theater ratio (0.40) reflects the performative aspect of presenting institutionally expedient changes as direct divine will, even when external pressures are clearly the catalyst. Accessibility collapse is high (0.80) for devout members due to identity lock-in, making exit unthinkable. Resistance is moderate (0.45) as some members do dissent or leave, but the majority comply.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, the prophetic override is a divinely ordained mechanism for a living church to adapt and thrive, a 'rope' for institutional survival. From the perspective of members who adhered to prior revelations, it can feel like a 'snare' that demands the sacrifice of deeply held beliefs for institutional expediency, enforced by spiritual authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The church leadership and institution are clear beneficiaries (d near 0.0) as the doctrine ensures their survival and perpetuates their authority. Devout and dissenting members are targets (d near 1.0) as they bear the costs of doctrinal shifts, often experiencing spiritual and social extraction. The federal government acts as an external agenda-setter, indirectly influencing the constraint's activation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (church survival through divine guidance) remains live, but its application involves a continuous re-evaluation of what constitutes 'divine guidance' versus 'institutional necessity.' The classification as a Tangled Rope prevents mislabeling it as pure extraction by acknowledging the genuine coordination function (church survival and member cohesion) while highlighting the asymmetric costs borne by members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''prophetic override'' reading of the eternal marriage covenant kernel?',
    'Analysis of theological texts, historical records, and member testimonies to confirm the specific interpretive framework and its structural implications.',
    'If misidentified, the analysis of reading relations and axioms would be incorrect, leading to an inaccurate commitment-system classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the kernel context.').

omega_variable(
    divine_will_vs_institutional_expediency,
    'To what extent is the ''continuing revelation'' genuinely divine will, versus an institutionally expedient adaptation to external pressures?',
    'Comparative theological analysis, historical examination of the timing and content of revelations relative to external events, and sociological studies of religious authority.',
    'If primarily expedient, the extractiveness and theater_ratio would be higher, and the ''theological'' grounding of axioms would be re-evaluated as ''instrumental'' or ''conventional'', potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_expediency, empirical, 'Ambiguity of the source of ''continuing revelation''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., social ostracism, excommunication) or internalized (e.g., spiritual guilt, identity fusion)?',
    'Post-exit suppression trajectory: if spiritual distress and identity issues persist after formal exit from the church, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measures suggest, as members carry the suppression with them even after leaving the formal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(eter_tr_t1980, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1980, 0.39).
narrative_ontology:measurement(eter_tr_t2020, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(eter_be_t1980, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1980, 0.67).
narrative_ontology:measurement(eter_be_t2020, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(eter_su_t1980, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1980, 0.74).
narrative_ontology:measurement(eter_su_t2020, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. Each reading represents a distinct structural claim about the covenant's nature and application, with differing ε values and stakeholder dynamics. They are linked to show their interpretive contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
