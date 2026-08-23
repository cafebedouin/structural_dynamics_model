% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture-Tradition-Magisterium Authority Structure
 *   domain: theological/religious/historical
 *
 * SUMMARY:
 *   This constraint story models the Catholic 'Tradition-Scripture' reading
 *   of the biblical_authority kernel: Scripture is not self-interpreting; it
 *   requires the living Tradition guarded by the magisterium. The arrangement
 *   claims divine institution (Mountain) but structurally operates as a
 *   Tangled Rope: it coordinates global doctrinal unity (genuine coordination
 *   function) while extracting interpretive authority, sacramental control,
 *   and material resources from the laity and lower clergy to the
 *   institutional hierarchy (asymmetric extraction). Active enforcement
 *   (canon law, censures, sacramental discipline) is required to maintain the
 *   arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.75).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.8).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture-Tradition-Magisterium Authority Structure").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theological/religious/historical").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6').
narrative_ontology:cs_kernel_codification('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', formalized).
narrative_ontology:cs_authority_grounding('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', extraction).
narrative_ontology:cs_interpretation_layer_present('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6').
narrative_ontology:cs_reading_relation('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', foundational, tradition_necessary_for_scriptural_interpretation).
narrative_ontology:cs_axiom_status(tradition_necessary_for_scriptural_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', tradition_necessary_for_scriptural_interpretation, deontological).
narrative_ontology:cs_axiom('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', foundational, magisterium_has_authoritative_guardianship_of_deposit).
narrative_ontology:cs_axiom_status(magisterium_has_authoritative_guardianship_of_deposit, holdable).
narrative_ontology:cs_axiom_grounding('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', magisterium_has_authoritative_guardianship_of_deposit, deontological).
narrative_ontology:cs_reference_frame('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', apostolic_magisterial_guardianship).
narrative_ontology:cs_drift_state('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', post_reformation_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5fcd157b-bd79-4a1c-8da3-b71b8ba25fb6', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, theologians).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, theologians).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, sacramental_mediation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The magisterium (pope and bishops in communion) defines and guards the deposit of faith, authoritatively interprets Scripture and Tradition, and controls sacramental mediation. It collects obedience, tithes, and institutional legitimacy. Exit is effectively impossible for the hierarchy itself; it is the structure.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Priests and bishops administer sacraments, teach authoritatively, and receive stipends/housing/status. They are bound by canon law, vow of obedience, and sacramental character. Exit means laicization, loss of livelihood and identity.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clergy, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, clergy, payer).

% Lay Catholics are required to assent to magisterial teaching, receive grace only through clerical sacraments, and have no authoritative interpretive voice. Their spiritual life depends on the hierarchy. Exit means leaving the Church, which for the identity-locked is existentially costly (loss of salvation framework, community, family).
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, identity_locked, global).

% Professional theologians explore the deposit but must submit to magisterial correction. They gain academic positions and intellectual vocation but risk censure (e.g., mandatum withdrawal, silencing). Exit means moving to secular academia or other traditions.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, theologians, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, theologians, beneficiary).

% Orthodox, Protestant, and other Christian bodies engage in dialogue but are structurally excluded from the magisterium's authoritative adjudication. Their interpretations are heard but not binding. They would object to the claim of exclusive interpretive authority.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ecumenical_dialogue_partners, excluded,
    organized, generational, mobile, global).

% Scholars using historical-critical methods study Scripture and Tradition as human texts. They see the magisterium's claims as historically conditioned. They neither collect nor pay; they analyze.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, historical_critical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, authoritative interpretation of divine revelation across time and cultures, preventing doctrinal fragmentation and preserving sacramental unity. Solves the coordination problem of 'what does God require?' by a single teaching office.
% TRANSFER_FUNCTION: Moves interpretive authority, sacramental gatekeeping, and definitional power from the laity and local communities to the centralized hierarchy. The laity surrender private judgment and receive doctrinal certainty and sacramental grace in return.
% ABSENT_VOICES: The laity as a collective interpretive subject, women barred from holy orders, historical dissenters (e.g., Huss, Luther, modern reform movements), and churches not in communion with Rome. They are excluded by canon law, sacramental theology, and the definition of the magisterium itself.
% DISAPPEARANCE_RATIONALE: If the magisterium's authoritative guard vanished overnight, the Catholic Church would lose its defining structural claim: a single, divinely guaranteed interpreter. Doctrinal fragmentation would accelerate, sacramental discipline would dissolve, and the institutional hierarchy would lose its primary legitimating function. The world of global Catholicism would rearrange into something resembling Anglicanism or Orthodoxy — or fragment further.
% FOUNDING_PROBLEM: After the apostolic age, the early church faced competing interpretations of Scripture (Gnosticism, Arianism, etc.) and needed a stable, authoritative rule of faith to maintain unity and orthodoxy across the Mediterranean world.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the problem is live (ongoing need for unity against relativism). Historians (e.g., Chadwick, Pelikan) attest the founding problem was real in the patristic era but argue the solution (monarchical episcopate/papacy) evolved, not given at once. Protestant and Orthodox scholars attest the problem was solved differently (councils, Scripture alone). No non-Catholic source corroborates the current magisterial form as the necessary solution.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the hierarchy monopolizes grace-conferring sacraments and authoritative teaching, demanding assent and material support. Suppression is high (0.8) because alternatives (private interpretation, rival magisteria, lay preaching) are canonically prohibited and historically punished. Theater ratio is moderate (0.4): the coordination function (unity, sacraments) is real but a growing share of enforcement defends institutional prerogatives (e.g., resistance to collegiality, sexual abuse cover-ups). Accessibility collapse is high (0.7) because the identity-locked laity cannot conceive exit without spiritual catastrophe. Resistance is moderate (0.5): historical schisms, modern dissent, and secularization show persistent but fragmented pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat, the constraint is a Mountain (divine law, no alternative). From the laity's seat, it is a Snare (coerced assent, no exit). The engine computes this divergence from the declared power/exit/role structure. The claimed_type (tangled_rope) reflects the author's structural judgment: genuine coordination (unity, sacraments) fused with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional hierarchy is the structural beneficiary (collects authority, resources, obedience — d near 0). Clergy are secondary beneficiaries but also payers (bound by obedience, celibacy, canonical penalties — d ~0.3). Lay interpretive agency is the primary victim (surrenders judgment, depends on clergy for grace, identity-locked exit — d near 1). Theologians sit near symmetric (gain vocation, risk censure). Excluded and observer seats are outside the extraction loop.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal unity against heresy) was live in the patristic era. The solution (monarchical episcopate, papal primacy) evolved over centuries. Today the problem is contested: the hierarchy says unity still requires this structure; critics say the structure now causes fragmentation (schisms, exits) and the coordination function is served by other means (ecumenism, Scripture scholarship). The mandate has not been formally sunset; the hierarchy claims it is irreformable. This is a classic mandatrophy candidate: the arrangement persists because the cost of reform (loss of authority, identity) exceeds the benefit to the only agents who could change it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tradition_necessity_empirical,
    'Is Tradition (as distinct from Scripture) empirically necessary for doctrinal unity, or do non-magisterial traditions (Orthodox, Anglican, Protestant) achieve comparable unity without a centralized magisterium?',
    'Comparative historical sociology of Christian communions: measure doctrinal fragmentation rates, schism frequency, and sacramental coherence across Catholic, Orthodox, and Protestant bodies over 500 years.',
    'If non-magisterial traditions show similar or lower fragmentation, the coordination function is not uniquely served by this constraint — extraction is not the price of coordination but an independent imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_necessity_empirical, empirical, 'Whether the magisterium''s coordination function is structurally unique or contested.').

omega_variable(
    sacramental_mediation_ontology,
    'Does the constraint''s extraction depend on the ontological claim that sacraments require ordained ministers to confer grace ex opere operato, or is this a sociological mechanism that could be restructured?',
    'Theological-historical analysis: trace the development of sacramental theology from patristic to medieval to Tridentine to Vatican II. Assess whether the ''necessity'' of ordained mediation is a development or a given.',
    'If the necessity is a historical development, the extraction is contingent and the constraint is a construct. If it is an irreducible ontological claim, the constraint approaches Mountain status for believers (though not for non-believers).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_mediation_ontology, conceptual, 'Ontological status of the sacramental mediation claim that underwrites clerical extraction.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''biblical_authority'' refer to the deposit of faith as a fixed content, or to the living process of interpretation? This reading treats it as fixed content guarded by a living magisterium; sola_scriptura treats it as fixed text self-interpreting; conciliar treats it as living process. The framing changes what counts as extraction.',
    'Meta-theological analysis of the kernel''s use in magisterial documents (Dei Verbum, Catechism) vs. Protestant confessions vs. Orthodox conciliar texts. Identify whether the kernel is a noun (content) or verb (process).',
    'If the kernel is a process, this reading''s claim to guard a fixed deposit is a category error — the constraint is a Snare freezing a living tradition. If the kernel is content, the guard may be a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Framing ambiguity of the kernel itself across sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t25, biblical_authority__tradition_scripture_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(bibl_tr_t50, biblical_authority__tradition_scripture_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(bibl_tr_t75, biblical_authority__tradition_scripture_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t25, biblical_authority__tradition_scripture_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(bibl_be_t50, biblical_authority__tradition_scripture_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(bibl_be_t75, biblical_authority__tradition_scripture_reading, base_extractiveness, 75, 0.78).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bibl_su_t25, biblical_authority__tradition_scripture_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(bibl_su_t50, biblical_authority__tradition_scripture_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(bibl_su_t75, biblical_authority__tradition_scripture_reading, suppression_requirement, 75, 0.85).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_authority kernel. The readings differ on the locus of interpretive authority (magisterium vs. individual vs. council) and the role of Tradition. This reading's high extraction and centralized enforcement structurally pressure the conciliar reading (which shares Tradition but rejects magisterial monopoly) and foreclose the sola_scriptura reading (which rejects Tradition's necessity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, institutional, 0.05).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, organized, 0.3).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
