% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'continuationist' reading of a divine
 *   command for plural marriage within a specific religious tradition.
 *   According to this reading, the original command for polygamy remains
 *   doctrinally valid, and the 1890 Manifesto was a prudential suspension
 *   under duress (federal anti-polygamy laws), not a doctrinal rescission.
 *   This interpretation allows fundamentalist splinter groups to claim
 *   theological legitimacy for their continued practice of polygamy, while
 *   mainstream adherents grapple with the tension between historical doctrine
 *   and current institutional policy. The constraint is claimed as a
 *   'tangled_rope' because it coordinates theological continuity but extracts
 *   heavily from those who adhere to its full implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '870e212a-ab28-4d3f-b886-31712fb49615').
narrative_ontology:cs_kernel_codification('870e212a-ab28-4d3f-b886-31712fb49615', fixed_text).
narrative_ontology:cs_authority_grounding('870e212a-ab28-4d3f-b886-31712fb49615', lineage).
narrative_ontology:cs_interpretation_layer_present('870e212a-ab28-4d3f-b886-31712fb49615').
narrative_ontology:cs_reading_relation('870e212a-ab28-4d3f-b886-31712fb49615', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('870e212a-ab28-4d3f-b886-31712fb49615', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('870e212a-ab28-4d3f-b886-31712fb49615', foundational, plural_marriage_eternal_command).
narrative_ontology:cs_axiom_status(plural_marriage_eternal_command, holdable).
narrative_ontology:cs_axiom_grounding('870e212a-ab28-4d3f-b886-31712fb49615', plural_marriage_eternal_command, theological).
narrative_ontology:cs_axiom('870e212a-ab28-4d3f-b886-31712fb49615', foundational, manifesto_prudential_suspension).
narrative_ontology:cs_axiom_status(manifesto_prudential_suspension, holdable).
narrative_ontology:cs_axiom_grounding('870e212a-ab28-4d3f-b886-31712fb49615', manifesto_prudential_suspension, conventional).
narrative_ontology:cs_reference_frame('870e212a-ab28-4d3f-b886-31712fb49615', original_divine_command_polygamy).
narrative_ontology:cs_drift_state('870e212a-ab28-4d3f-b886-31712fb49615', post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('870e212a-ab28-4d3f-b886-31712fb49615', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_adherents).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamous_families).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, mainstream_church_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the theological legitimacy of plural marriage as a divine command, even if not actively practiced. Their identity is tied to the continuity of original revelation, which the Manifesto is seen as temporarily suspending, not rescinding. They benefit from the preservation of this doctrinal possibility.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_adherents, beneficiary,
    moderate, generational, identity_locked, local).

% Actively practice and advocate for plural marriage, viewing the Manifesto as a betrayal of divine command. They claim direct continuity with original revelation and enforce their interpretation within their communities, often facing legal and social penalties. They benefit from the continuationist reading providing a theological basis for their practices.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, agenda_setter,
    organized, generational, constrained, regional).

% Bear the direct legal and social costs of practicing polygamy in jurisdictions where it is illegal. They are often isolated, economically vulnerable, and face state intervention, yet remain committed due to deep religious conviction and identity lock-in. They pay the price for the continuationist reading's doctrinal stance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Administers the church's doctrine and policy, officially upholding the Manifesto as a binding directive against plural marriage. However, the continuationist reading challenges their authority by asserting the underlying doctrinal validity of polygamy, creating internal tension. They are caught between federal law and historical doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_leadership, agenda_setter,
    institutional, civilizational, constrained, global).

% Adhere to the church's current monogamous practice but may experience cognitive dissonance or internal conflict due to the historical and continuationist doctrinal claims. They bear the social cost of association with a contested historical practice, even if not personally engaging in it. Their identity is tied to the church's authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_members, payer,
    moderate, biographical, identity_locked, global).

% Enforces laws against polygamy, treating it as a criminal offense. From this seat, the Manifesto is seen as a pragmatic concession to legal authority, not a theological shift. It acts as an external constraint on the church's historical practices and on splinter groups.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the theological understanding of marriage within a religious tradition, preserving the historical divine command for plural marriage as doctrinally valid, even if not currently practiced, thereby maintaining continuity with founding revelations.
% TRANSFER_FUNCTION: Transfers theological legitimacy and historical continuity to adherents who believe in the enduring validity of plural marriage, while transferring social and legal costs to those who actively practice it or are associated with the historical doctrine.
% ABSENT_VOICES: Secular legal scholars and human rights advocates, who would argue that the 'divine command' justification for polygamy is a cover for patriarchal control and that the Manifesto was a necessary step towards gender equality, are excluded from the internal theological debate.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished, fundamentalist splinter groups would lose their primary theological justification, potentially leading to their dissolution or re-framing. Mainstream church leadership would face less internal doctrinal pressure, and the historical narrative of the church would be significantly altered, impacting member identity and institutional legitimacy.
% FOUNDING_PROBLEM: The founding problem was to establish a divinely sanctioned marriage practice that would allow for the rapid growth and sealing of families in the early days of the religious movement, understood as a direct command from God.
% FOUNDING_PROBLEM_CORROBORATION: Continuationist adherents and fundamentalist splinter groups attest the problem is live, citing scriptural interpretations and ongoing theological arguments for the necessity of plural marriage for exaltation. Mainstream church leadership acknowledges the historical command but asserts its current suspension, while federal legal systems view the 'problem' as a violation of secular law, not a theological imperative.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the doctrinal validity, while not universally practiced, imposes significant social and legal costs on those who do, and creates internal tension for those who don't. Suppression is very high (0.78) due to the combined pressure of federal law (external) and mainstream church policy (internal) against active polygamy, which this reading resists. Theater ratio is moderate (0.4) as the mainstream church performs adherence to the Manifesto while the underlying doctrinal validity is preserved by continuationists, creating a performative gap.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of polygamous families, this reading is a snare, trapping them between divine command and legal persecution. For continuationist adherents, it's a rope, preserving a vital aspect of their faith. Mainstream church leadership experiences it as a tangled rope, balancing historical doctrine with modern exigencies. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Continuationist adherents and fundamentalist splinter groups are beneficiaries (d near 0.0-0.2) as they gain theological legitimacy and identity from this reading. Polygamous families and mainstream church members are victims/payers (d near 0.8-1.0) as they bear the direct legal/social costs or internal conflict. Mainstream church leadership and the federal legal system act as agenda-setters, enforcing their respective interpretations, but the continuationist reading challenges the former's authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_rescission_ambiguity,
    'Is the 1890 Manifesto a doctrinal rescission of plural marriage (substitutionist reading) or a prudential suspension under duress (continuationist reading)?',
    'Further theological pronouncements from the highest ecclesiastical authority clarifying the nature of the Manifesto, or historical evidence revealing the intent of its authors beyond public statements.',
    'If rescission, the continuationist reading loses its theological grounding, reclassifying it as a snare for splinter groups. If suspension, the mainstream church''s current monogamous practice is further exposed as a pragmatic rather than divinely mandated shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_rescission_ambiguity, conceptual, 'Ambiguity regarding the theological status of the 1890 Manifesto.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by polygamous families structural (federal law, economic isolation) or internalized (identity lock-in, belief in divine command)?',
    'Post-exit suppression trajectory: if suppression persists after legal/economic barriers are removed (e.g., in a permissive jurisdiction), reclassify as partially internalized. If it vanishes, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them after exit. This would amplify the snare-like qualities for polygamous families.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for polygamous families.').

omega_variable(
    legitimacy_of_splinter_groups,
    'Are fundamentalist splinter groups genuinely continuing the original divine command, or are they a new formation leveraging historical doctrine for their own authority?',
    'Genealogical and historical analysis of leadership succession, doctrinal consistency, and community practices compared to the pre-Manifesto era, corroborated by independent historians.',
    'If they are a new formation, their claim to continuity is weakened, reducing the ''beneficiary'' aspect of the continuationist reading and reclassifying it as a snare for their members. If genuine, it reinforces the continuationist reading''s internal coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_splinter_groups, empirical, 'Authenticity of fundamentalist splinter groups'' claim to original revelation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel. This 'continuationist_reading' asserts the enduring doctrinal validity of plural marriage, interpreting the Manifesto as a suspension. It is linked to the 'substitutionist_reading' (monogamy as new doctrine) and the 'coercion_visibility_reading' (Manifesto as pragmatic response to coercion), as these interpretations are in direct theological and historical contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
