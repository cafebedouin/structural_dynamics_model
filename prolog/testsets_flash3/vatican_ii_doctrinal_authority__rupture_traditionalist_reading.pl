% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority (Rupture Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture traditionalist' reading
 *   of Vatican II's doctrinal authority. From this perspective, Vatican II
 *   represents a fundamental break with Catholic tradition, its documents
 *   containing ambiguities and errors that enabled heterodox interpretations
 *   and implementations. The post-conciliar period is seen as a predictable
 *   consequence of these flaws, leading to a decline in traditional liturgy,
 *   doctrinal clarity, and missionary zeal. The constraint's high
 *   extractiveness reflects the perceived loss of these traditional elements,
 *   while suppression is high due to institutional efforts to enforce the new
 *   norms and marginalize traditionalist dissent. This reading is one of
 *   several competing interpretations of the Council, each with distinct
 *   structural implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority (Rupture Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'f51f03cc-d608-4019-9aea-1cef1cbbd494').
narrative_ontology:cs_kernel_codification('f51f03cc-d608-4019-9aea-1cef1cbbd494', fixed_text).
narrative_ontology:cs_authority_grounding('f51f03cc-d608-4019-9aea-1cef1cbbd494', lineage).
narrative_ontology:cs_interpretation_layer_present('f51f03cc-d608-4019-9aea-1cef1cbbd494').
narrative_ontology:cs_reading_relation('f51f03cc-d608-4019-9aea-1cef1cbbd494', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f51f03cc-d608-4019-9aea-1cef1cbbd494', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f51f03cc-d608-4019-9aea-1cef1cbbd494', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('f51f03cc-d608-4019-9aea-1cef1cbbd494', foundational, doctrinal_infallibility_of_tradition).
narrative_ontology:cs_axiom_status(doctrinal_infallibility_of_tradition, holdable).
narrative_ontology:cs_axiom_grounding('f51f03cc-d608-4019-9aea-1cef1cbbd494', doctrinal_infallibility_of_tradition, deontological).
narrative_ontology:cs_axiom('f51f03cc-d608-4019-9aea-1cef1cbbd494', foundational, liturgical_stability_as_sacred_deposit).
narrative_ontology:cs_axiom_status(liturgical_stability_as_sacred_deposit, holdable).
narrative_ontology:cs_axiom_grounding('f51f03cc-d608-4019-9aea-1cef1cbbd494', liturgical_stability_as_sacred_deposit, theological).
narrative_ontology:cs_reference_frame('f51f03cc-d608-4019-9aea-1cef1cbbd494', pre_conciliar_magisterial_teaching).
narrative_ontology:cs_drift_state('f51f03cc-d608-4019-9aea-1cef1cbbd494', post_conciliar_implementation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f51f03cc-d608-4019-9aea-1cef1cbbd494', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the post-conciliar changes as a loss of sacred tradition, particularly in liturgical practice. They feel alienated and marginalized, with limited options for authentic expression within the official Church structures. Their identity is deeply tied to pre-conciliar forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents, payer,
    powerless, biographical, identity_locked, global).

% Perceive a weakening of clear doctrinal teaching and a rise of heterodoxy, directly attributable to the Council's ambiguities. They bear the cost of defending traditional interpretations against what they see as novelties, often facing institutional resistance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_conservatives, payer,
    moderate, generational, constrained, global).

% Believe the Council's emphasis on ecumenism and interreligious dialogue diluted the Church's unique salvific mission, leading to a decline in conversions and a loss of evangelistic fervor. They struggle to articulate a clear missionary imperative in the post-conciliar environment.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_advocates, payer,
    moderate, generational, constrained, global).

% Benefit from the Council's perceived opening to new theological methods and interpretations, which they see as liberating from rigid scholasticism. They gain influence and academic freedom, using conciliar texts to justify their progressive approaches.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, modernist_theologians, beneficiary,
    powerful, biographical, mobile, global).

% Implement the Council's reforms in parishes and dioceses, often interpreting its 'spirit' to justify changes in liturgy, pastoral practice, and social engagement. They gain authority and legitimacy for their initiatives, often at the expense of traditional forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy, beneficiary,
    organized, biographical, constrained, global).

% A faction within the Vatican bureaucracy that seeks to re-assert traditional doctrine and practice, often by re-interpreting Vatican II through a 'hermeneutic of reform in continuity.' They administer policies that attempt to curb perceived heterodoxy and restore traditional elements, but face significant internal resistance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia_traditionalist_faction, agenda_setter,
    institutional, generational, constrained, global).

% Believe that the post-conciliar popes are not legitimate, having departed from true Catholic faith. They are entirely outside the official Church structure and their voices are dismissed as schismatic, but their existence highlights the depth of perceived rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sedevacantists, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council aimed to coordinate the Church's response to modernity, fostering renewal and ecumenical engagement, and updating pastoral approaches for the contemporary world.
% TRANSFER_FUNCTION: This reading sees the Council as transferring doctrinal clarity and liturgical stability away from traditional adherents towards modernist interpretations and progressive pastoral practices, effectively extracting traditional forms of worship and belief.
% ABSENT_VOICES: Sedevacantists and other radical traditionalist groups are entirely excluded from the official discourse, their critiques dismissed as schismatic. Their absence allows the official Church to frame the debate as internal disagreement rather than fundamental rupture.
% DISAPPEARANCE_RATIONALE: If the Council's documents and their subsequent implementation were suddenly nullified, the entire structure of modern Catholicism would collapse. Liturgical practices, theological education, ecumenical relations, and the Church's self-understanding would undergo a profound and immediate reorganization, reverting to pre-conciliar forms or splintering into new factions.
% FOUNDING_PROBLEM: The Council was convened to address the Church's perceived isolation from the modern world, to promote Christian unity, and to renew Catholic life and mission.
% FOUNDING_PROBLEM_CORROBORATION: The official Church and progressive theologians attest that the problems of modernity and Christian disunity remain live, requiring ongoing conciliar engagement. Traditionalist critics, however, argue that the Council exacerbated these problems, and that the original issues were either misdiagnosed or poorly addressed, leading to new crises rather than solutions. Independent historians and sociologists of religion offer mixed corroboration, noting both positive and negative impacts on Catholic life and engagement with the world.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the perceived loss of traditional forms of worship, clear doctrine, and missionary focus is substantial and ongoing. Suppression (0.70) is significant due to the institutional power used to enforce post-conciliar norms and marginalize traditionalist voices, including restrictions on the Traditional Latin Mass. Theater ratio (0.40) reflects the ongoing efforts to present the Council as continuous with tradition, despite perceived ruptures, often through re-interpretations that obscure the actual changes. Resistance is high (0.80) as traditionalist groups actively oppose the perceived rupture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (modernist theologians, progressive clergy), Vatican II was a necessary and beneficial development, a 'rope' for renewal. From the perspective of victims (traditional liturgy adherents, doctrinal conservatives), it functions as a 'snare,' extracting their cherished traditions and imposing novelties. The Roman Curia's traditionalist faction, while an agenda-setter, experiences the constraint as a 'tangled rope,' trying to coordinate a return to tradition while being bound by the Council's documents and the institutional inertia of its implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist theologians and progressive clergy are beneficiaries, gaining legitimacy and freedom for their interpretations and practices. Traditional liturgy adherents, doctrinal conservatives, and missionary zeal advocates are victims, experiencing loss and marginalization. The Roman Curia's traditionalist faction acts as an agenda-setter, attempting to steer the Church back towards tradition but facing internal and external resistance. Sedevacantists are excluded, their position placing them outside the Church's recognized authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the Council's original mandate (renewal, unity) has been subverted, leading to a 'mandatrophy' where the constraint now serves to enforce a rupture rather than genuine development. The classification as a snare prevents mislabeling this as mere coordination, highlighting the identifiable victims and the active suppression of alternatives (traditional practices).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_vs_rupture,
    'Is Vatican II an organic development of Catholic doctrine (continuity) or a fundamental break with it (rupture)?',
    'Comprehensive historical-theological analysis of pre- and post-conciliar texts and practices, focusing on explicit contradictions versus implicit developments. This is a conceptual omega, as ''rupture'' and ''continuity'' are interpretive frames.',
    'If continuity is established, the extractiveness of this constraint would be re-evaluated downwards, as the perceived ''loss'' would be re-framed as ''development.'' If rupture is definitively proven, the snare classification would be strengthened, and the beneficiaries'' claims of ''renewal'' would be re-framed as ''extraction.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_rupture, conceptual, 'The core interpretive debate over Vatican II''s relationship to tradition.').

omega_variable(
    ambiguity_as_error_vs_pastoral_nuance,
    'Are the ambiguities in Vatican II documents genuine errors or intentional pastoral nuances designed to allow for diverse interpretations and applications?',
    'Analysis of conciliar drafting history, debates, and subsequent magisterial interpretations. This is an empirical omega, as it relies on historical evidence and textual analysis.',
    'If ambiguities are proven errors, it strengthens the traditionalist claim of a flawed Council. If they are intentional pastoral nuances, it weakens the claim of ''error'' but might still support the ''rupture'' thesis by demonstrating a shift in doctrinal methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_error_vs_pastoral_nuance, empirical, 'The nature and intent of ambiguities within the Council''s texts.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditionalist voices structural (institutional policies, legal restrictions) or internalized (social pressure, fear of marginalization)?',
    'Post-exit suppression trajectory: if traditionalist practices and communities flourish outside official structures, it suggests structural suppression. If dissent persists even after formal restrictions are lifted, it points to internalized suppression. This is an empirical omega.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as traditionalists carry the suppression with them after exit. If purely structural, removing institutional barriers would significantly reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Vatican II doctrinal authority' kernel. Its high extractiveness and snare classification reflect the traditionalist perception of rupture and loss, contrasting with the 'continuity' reading (lower extractiveness, rope/scaffold) and the 'rupture progressive' reading (high extractiveness, but positively valued as liberation). All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
