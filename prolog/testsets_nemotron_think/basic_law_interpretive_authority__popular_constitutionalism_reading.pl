% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Distributed Constitutional Interpretive Authority (Popular Constitutionalism Reading)
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the popular_constitutionalism_reading
 *   of the basic_law_interpretive_authority kernel. The reading holds that
 *   constitutional meaning emerges from ongoing democratic contestation
 *   across multiple institutional and civil society venues, rather than from
 *   terminal adjudication by courts (judicial_supremacy_reading) or
 *   legislature (parliamentary_sovereignty_reading). The constraint is the
 *   distributed interpretive authority structure itself: no institution has
 *   the final word; constitutional disputes remain perpetually open to
 *   democratic challenge. The reading claims this is a coordination mechanism
 *   (rope) for democratic legitimacy; the metrics show low but non-zero
 *   extraction as organized actors benefit from perpetual contestation while
 *   the public bears gridlock costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.28).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Distributed Constitutional Interpretive Authority (Popular Constitutionalism Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '55d4b590-9d7f-4293-a280-b30e6915c314').
narrative_ontology:cs_kernel_codification('55d4b590-9d7f-4293-a280-b30e6915c314', distributed).
narrative_ontology:cs_authority_grounding('55d4b590-9d7f-4293-a280-b30e6915c314', distributed).
narrative_ontology:cs_reading_relation('55d4b590-9d7f-4293-a280-b30e6915c314', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('55d4b590-9d7f-4293-a280-b30e6915c314', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('55d4b590-9d7f-4293-a280-b30e6915c314', foundational, no_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('55d4b590-9d7f-4293-a280-b30e6915c314', no_terminal_interpretive_authority, deontological).
narrative_ontology:cs_axiom('55d4b590-9d7f-4293-a280-b30e6915c314', foundational, constitutional_meaning_tracks_democratic_will).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_democratic_will, holdable).
narrative_ontology:cs_axiom_grounding('55d4b590-9d7f-4293-a280-b30e6915c314', constitutional_meaning_tracks_democratic_will, empirically_contingent).
narrative_ontology:cs_reference_frame('55d4b590-9d7f-4293-a280-b30e6915c314', perpetual_constituent_power).
narrative_ontology:cs_drift_state('55d4b590-9d7f-4293-a280-b30e6915c314', contemporary_judicial_activism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55d4b590-9d7f-4293-a280-b30e6915c314', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, political_officeholders).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, interest_groups).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_litigators).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, the_public).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, courts).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_legitimacy_requires_contestation).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_adaptation_without_formal_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drive constitutional contestation through mobilization, litigation, and public argument. They set the agenda by forcing issues into democratic contestation. Their exit is forming new movements or shifting issue focus; they are not trapped in any single constitutional interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements, agenda_setter,
    organized, biographical, mobile, national).

% Use open constitutional meaning to justify policy innovations, resist unfavorable judicial rulings, and mobilize electoral support. They benefit from the flexibility to interpret the constitution in ways that serve their coalition. Exit means leaving office or accepting judicial/legislative settlements they dislike.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_officeholders, beneficiary,
    powerful, biographical, constrained, national).

% Litigate and lobby to shape constitutional meaning in their favor. The absence of terminal authority means multiple institutional venues (courts, legislatures, agencies, public opinion) are available for contestation. Exit means shifting venue or strategy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Build careers on perpetual constitutional contestation. The lack of settled meaning creates sustained demand for litigation across multiple forums. Exit means changing practice area; the field itself depends on the constraint's persistence.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_litigators, beneficiary,
    moderate, biographical, mobile, national).

% Bear the diffuse costs of constitutional gridlock: policy paralysis, institutional instability, rights left unresolved. Cannot exit the constitutional order; must live with the consequences of unresolved contests. The costs are distributed but fall hardest on those who need settled law for life planning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, the_public, payer,
    powerless, biographical, trapped, national).

% Denied the terminal interpretive authority they claim under judicial supremacy. Must issue rulings knowing they are provisional entries in ongoing contestation, not final settlements. Exit means abandoning the judicial role or accepting diminished authority — institutionally difficult.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, courts, payer,
    institutional, generational, constrained, national).

% Denied the final interpretive authority parliamentary sovereignty would grant. Must legislate under constitutional uncertainty and face judicial review that is not final. Exit means constitutional amendment (prohibitively difficult) or accepting the contested status quo.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, payer,
    institutional, generational, constrained, national).

% Analyze the contestation from outside; their work feeds back into the contestation but they do not bear its costs or collect its rents directly. Exit is intellectual — shifting focus to other kernels or comparative systems.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to track evolving democratic commitments without requiring formal amendment (which is prohibitively difficult) or judicial usurpation (which lacks democratic pedigree). Solves the problem of constitutional rigidity in a changing society.
% TRANSFER_FUNCTION: Moves interpretive authority from terminal institutions (courts under judicial supremacy, legislature under parliamentary sovereignty) to distributed democratic contestation across multiple venues. Gridlock costs move from concentrated institutional bearers (courts forced to decide, legislatures forced to amend) to the diffuse public who live with unresolved constitutional questions.
% ABSENT_VOICES: Those who need settled constitutional law for planning: businesses requiring regulatory certainty, rights-holders seeking final vindication of claims, future generations who inherit unresolved contests without having participated in them. They are structurally excluded because the contestation has no terminal moment — they can only enter the ongoing stream.
% DISAPPEARANCE_RATIONALE: If distributed interpretive authority vanished overnight, constitutional disputes would be resolved by whichever institution successfully claims terminal authority — either courts (judicial supremacy) or legislature (parliamentary sovereignty). The constitutional order would reorganize around a new terminal adjudicator, ending the perpetual contestation but concentrating interpretive power.
% FOUNDING_PROBLEM: The problem of constitutional rigidity: how to adapt constitutional meaning to changing social, technological, and moral circumstances without either (a) formal amendment processes that are structurally blocked by supermajority requirements, or (b) judicial review that substitutes unelected judges' policy preferences for democratic deliberation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians (Bruce Ackerman, Jack Balkin) and comparative constitutionalists (Rosalind Dixon, David Landau) attest the adaptation problem is real and persistent across constitutional systems. Judicial supremacists (e.g., originalist scholars) contest whether distributed contestation solves it or merely displaces the problem; legislative supremacists (e.g., UK constitutional theorists) argue parliamentary sovereignty already provides democratic adaptation. No consensus outside the popular constitutionalist tradition.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint does not systematically transfer resources from a defined victim class to a defined beneficiary class — the costs (gridlock, uncertainty) and benefits (flexibility, responsiveness) are both diffusely distributed. Suppression is very low (0.15) because the constraint's defining feature is the ABSENCE of suppression of alternative interpretations; courts, legislatures, and movements all actively contest. Theater is low (0.12) because the contestation is genuine, not performative — actors really do litigate, legislate, and mobilize. Accessibility collapse is low (0.22) because the sibling readings (judicial supremacy, parliamentary sovereignty) remain live and institutionally practiced. Resistance is moderate (0.45) because institutions (especially courts) resist the denial of their terminal authority claims.
 *
 * PERSPECTIVAL GAP:
 *   From the social movement seat, the constraint is pure coordination (rope) — it enables democratic voice. From the public seat, it extracts gridlock costs (tangled_rope). From the court seat, it is a snare — it denies the institutional authority the court's own legitimacy structure depends on. The engine computes this divergence; the claimed_type (rope) reflects the reading's self-understanding, not the computed seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Social movements are agenda_setters (d near 0.0) — they drive the contestation and face minimal structural extraction. Political officeholders, interest groups, and litigators are beneficiaries (d ~ 0.2-0.3) — they gain strategic flexibility and venue-shopping opportunities. The public is a trapped payer (d ~ 0.8) — bears gridlock costs with no exit. Courts and legislature are constrained payers (d ~ 0.6-0.7) — institutionally powerful but denied the final authority they structurally claim. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional rigidity) remains live — formal amendment is still nearly impossible in most systems, and judicial review still substitutes policy judgment for democratic deliberation. The constraint has not outlived its function. However, the extraction metrics show a slight upward trend as organized actors learn to exploit perpetual contestation, suggesting mandatrophy risk if the coordination function degrades further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contestation_stability,
    'Is perpetual democratic contestation structurally stable, or does it inevitably collapse into de facto judicial or legislative supremacy as actors tire of uncertainty?',
    'Longitudinal study of constitutional systems with distributed interpretive authority (e.g., Canada''s dialogic model, South Africa''s transformative constitutionalism): do they stabilize as genuine popular constitutionalism or drift toward judicial supremacy?',
    'If contestation collapses into de facto terminal authority, the constraint''s claimed_type (rope) is false — it is a transitional scaffold toward judicial/legislative supremacy. If stable, the low extraction metrics are sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contestation_stability, empirical, 'Structural stability of distributed interpretive authority over time').

omega_variable(
    gridlock_cost_distribution,
    'Are the gridlock costs of perpetual contestation genuinely diffuse, or do they concentrate on vulnerable groups who cannot afford constitutional uncertainty?',
    'Empirical analysis of rights-adjudication outcomes under distributed vs. terminal authority: do marginalized groups fare better when constitutional meaning is settled (even imperfectly) or when it remains perpetually contestable?',
    'If costs concentrate on vulnerable groups, the constraint''s extraction is asymmetric (tangled_rope or snare) despite diffuse appearance. If genuinely diffuse, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_distribution, empirical, 'Distributional incidence of constitutional gridlock costs').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (popular_constitutionalism_reading) of the basic_law_interpretive_authority kernel. The sibling readings (judicial_supremacy_reading, parliamentary_sovereignty_reading) instantiate different constraints with different ε, beneficiaries, and classifications.',
    'Author separate constraint stories for each sibling reading. Link via network.affects_constraints. The engine will compute each reading''s classification independently.',
    'Ensures ε-invariance: each reading gets its own constraint_id, its own metrics, its own stakeholders. The kernel_id and reading_id are recorded here for provenance; they do not affect this story''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committee frame: this story is one reading of a contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.08).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story and its two siblings form the basic_law_interpretive_authority constraint family. All three instantiate the same kernel (the question of final interpretive authority) but with different structural parameters: judicial_supremacy_reading concentrates authority in courts (high extraction from legislature/public), parliamentary_sovereignty_reading concentrates authority in legislature (high extraction from courts/minorities), popular_constitutionalism_reading distributes authority (low extraction, high coordination cost). The ε values differ because the coordination/extraction structures differ — not because of measurement basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
