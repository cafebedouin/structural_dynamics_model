% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural Hybrid Reading (Due Process Guarantees)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the procedural_hybrid_reading of UDHR Article 3:
 *   a constraint that guarantees habeas corpus and torture prohibition as
 *   universal procedural protections, while deliberately NOT resolving the
 *   substantive contest between negative liberty (freedom from state
 *   interference) and positive entitlement (state obligation to provide
 *   welfare/healthcare/housing). The reading occupies the institutional
 *   middle ground — it is the procedural infrastructure that both sibling
 *   readings presuppose and invoke. The kernel is UDHR Article 3 ('Everyone
 *   has the right to life, liberty and security of person'); this reading
 *   takes the procedural guarantees (habeas corpus, torture prohibition,
 *   judicial review) as the Article's operational core, treating the
 *   substantive liberty/welfare divide as a separate contest that the
 *   procedural layer does not settle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.25).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.15).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural Hybrid Reading (Due Process Guarantees)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '1dc9bd3d-da1a-4dd5-af1a-685ed47c9037').
narrative_ontology:cs_kernel_codification('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', formalized).
narrative_ontology:cs_authority_grounding('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', lineage).
narrative_ontology:cs_interpretation_layer_present('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037').
narrative_ontology:cs_reading_relation('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', foundational, procedural_floor_operational_priority).
narrative_ontology:cs_axiom_status(procedural_floor_operational_priority, holdable).
narrative_ontology:cs_axiom_grounding('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', procedural_floor_operational_priority, conventional).
narrative_ontology:cs_axiom('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', foundational, substantive_contest_remains_open).
narrative_ontology:cs_axiom_status(substantive_contest_remains_open, holdable).
narrative_ontology:cs_axiom_grounding('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', substantive_contest_remains_open, conventional).
narrative_ontology:cs_reference_frame('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', postwar_procedural_consensus).
narrative_ontology:cs_drift_state('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', post_9_11_security_paradigm, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dc9bd3d-da1a-4dd5-af1a-685ed47c9037', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detainees).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, persons_at_risk_of_torture).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_institutions).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, states_under_security_pressure).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_principle).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, torture_absolute_prohibition).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, judicial_review_availability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty who rely on habeas corpus and torture prohibition as their primary procedural shield. They cannot exit the state's custodial power; the constraint's protections are their only structural recourse against indefinite detention or abusive treatment.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detainees, beneficiary,
    powerless, biographical, trapped, universal).

% Individuals in custody or state control who face credible risk of torture or cruel treatment. The absolute prohibition operates as a non-derogable floor; they have no exit from the situation that makes them vulnerable, only the constraint's enforcement.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, persons_at_risk_of_torture, beneficiary,
    powerless, biographical, trapped, universal).

% Courts and tribunals that administer habeas corpus review and adjudicate torture claims. They gain institutional authority and legitimacy from being the designated enforcement mechanism, but also bear the operational burden of reviewing executive detention decisions under pressure.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judicial_institutions, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, judicial_institutions, beneficiary).

% Governments facing terrorism, insurgency, or public order crises who must maintain detention and interrogation capabilities within procedural limits. The constraint forces procedural compliance (judicial review, prohibition of certain methods) that raises operational costs and constrains emergency flexibility. Exit means withdrawing from the treaty regime or declaring derogation — politically costly and internationally visible.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_under_security_pressure, payer,
    powerful, biographical, constrained, national).

% UN treaty bodies, regional courts, and NGOs that monitor compliance, receive individual communications, and issue authoritative interpretations. They do not directly benefit or pay but shape the constraint's practical meaning through jurisprudence and reporting.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_monitoring_bodies, observer,
    organized, generational, analytical, global).

% Groups (suspected militants, irregular migrants, populations in conflict zones) whom states place outside procedural protections via derogation, legal black holes, or factual exclusion. They would object to suspension of habeas corpus or tolerance of torture but have no voice in the derogation decision.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, excluded_populations_in_emergencies, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal procedural floor for state custody: habeas corpus ensures no one disappears into detention without judicial oversight; torture prohibition establishes an absolute, non-derogable limit on state power over bodies. These coordinate expectations between states and persons under their control without resolving the substantive contest between negative liberty and positive entitlement readings.
% TRANSFER_FUNCTION: Transfers procedural compliance costs from detainees (who would bear the cost of lawless detention) to states (which must fund judicial review, monitoring, training, and foregone interrogation methods). Also transfers legitimating authority to judicial institutions that administer the review.
% ABSENT_VOICES: Populations in declared emergencies, conflict zones, or migration detention systems where states suspend or ignore procedural guarantees. They are structurally excluded from the conversation about derogation scope and necessity.
% DISAPPEARANCE_RATIONALE: If habeas corpus and torture prohibition vanished overnight, states would detain without judicial review as a default, torture would become a routine investigative tool in security contexts, and judicial institutions would lose their primary constitutional role in custody oversight. The relationship between state and detained person would revert to raw power.
% FOUNDING_PROBLEM: Post-WWII consensus that state power over life and liberty required procedural containment after the experience of arbitrary detention, disappearance, and state torture under fascist and totalitarian regimes. The procedural floor was the minimum agreement possible across ideological divides about substantive rights.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UDHR drafting history (Morsink, Glendon) and the travaux préparatoires of the ICCPR — sources outside any current beneficiary group. The procedural floor remains live because arbitrary detention and torture persist globally; the contest is over scope, not the problem's existence.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.25) because the constraint imposes real compliance costs on states (judicial review infrastructure, monitoring, training, foregone interrogation methods) but these costs are bounded and proportional to the coordination function — they are the price of a universal procedural floor, not open-ended extraction. Suppression is low (0.15) because the constraint's persistence relies on treaty obligation and judicial enforcement, not on suppressing alternatives; states can and do derogate in emergencies (ICCPR Art. 4), and the constraint's design anticipates this. Theater ratio is low (0.10) — the procedural protections are functionally real, not performative, though post-2001 practices (extraordinary rendition, black sites, enhanced interrogation) created a theater spike where states maintained formal compliance while outsourcing violations. Accessibility collapse is moderate (0.40) — alternatives (lawless detention, torture as policy) remain structurally possible and are occasionally realized, but the constraint makes them legally and politically costly. Resistance is moderate (0.30) — states resist through derogation, narrow interpretation, and factual non-compliance, but the constraint's core commands (habeas corpus, torture prohibition) remain widely accepted as jus cogens norms.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee's seat, the constraint is a lifeline (rope/scaffold). From the state's seat under pressure, it is a costly coordination requirement (tangled_rope at the margin). From the judicial seat, it is an institutional mandate with resource implications. The engine computes these per-seat divergences from the structural data; the claimed_type 'rope' reflects the reading's self-understanding as a genuine coordination mechanism with moderate, bounded extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees and persons at risk of torture are structural beneficiaries (d near 0.0) — the constraint subsidizes their protection at state expense. They are trapped (no exit from state custody). Judicial institutions are agenda-setters who also benefit institutionally (d ~ 0.2) — they gain authority but bear enforcement burden. States under security pressure are payers (d ~ 0.7) — they bear compliance costs and operational constraints, but have constrained exit (derogation is available but costly). Human rights bodies are observers (analytical). Excluded populations in emergencies are structural victims of the constraint's suspension — they would be beneficiaries if the constraint held, but are placed outside it by state action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary detention and torture after WWII) remains live globally. The constraint has not atrophied into a piton — its procedural core is actively enforced and litigated. The post-2001 theater spike represents a real degradation (states performing compliance while outsourcing violations), but the subsequent partial recovery (court rulings against black sites, renewed treaty body scrutiny) shows the constraint retains functional teeth. Mandatrophy is not resolved — the constraint still serves its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_substantive_boundary,
    'Where does the procedural guarantee end and the substantive contest begin? Does ''judicial review availability'' require courts to assess the substantive grounds of detention, or only the procedural regularity?',
    'Comparative analysis of habeas corpus jurisprudence across ICCPR parties: whether courts review the factual basis of detention (substantive) or only the procedural authorization (formal).',
    'If procedural review collapses into substantive review, this reading converges toward negative_liberty_reading; if it remains purely formal, the substantive contest stays open and the reading''s moderate epsilon is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_substantive_boundary, conceptual, 'Whether the procedural hybrid reading has a stable boundary or inevitably absorbs substantive adjudication.').

omega_variable(
    derogation_scope_ambiguity,
    'How far can states derogate from habeas corpus and torture prohibition under ICCPR Article 4 before the procedural floor collapses? Is the torture prohibition truly non-derogable in practice?',
    'Tracking state practice and treaty body jurisprudence on derogation notifications since 2001, particularly regarding detention review and interrogation standards.',
    'If derogation becomes routine and unchallenged, the constraint''s effective suppression rises and its rope classification degrades toward tangled_rope (coordination function compromised by systemic non-compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derogation_scope_ambiguity, empirical, 'Whether the procedural floor''s non-derogable core holds under sustained security pressure.').

omega_variable(
    kernel_reading_identity,
    'Is the procedural_hybrid_reading a distinct structural reading of UDHR Article 3, or merely a descriptive summary of the overlap between negative_liberty_reading and positive_entitlement_reading?',
    'Identify institutional actors (courts, treaty bodies, states) that explicitly endorse the procedural floor while rejecting both the negative liberty limitation and the positive entitlement expansion — i.e., actors for whom this reading is the operative commitment, not a compromise.',
    'If no actors hold this reading as their primary commitment, it is a taxonomic artifact rather than a live constraint — the engine would treat it as a coordination overlay on the two substantive constraints rather than a kernel reading with independent structural force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the procedural hybrid reading has independent institutional uptake or is only a theoretical midpoint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1966, 0.06).
narrative_ontology:measurement(udhr_tr_t1984, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1984, 0.08).
narrative_ontology:measurement(udhr_tr_t2001, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(udhr_tr_t2025, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1966, 0.12).
narrative_ontology:measurement(udhr_be_t1984, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1984, 0.18).
narrative_ontology:measurement(udhr_be_t2001, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2001, 0.28).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(udhr_be_t2025, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.08).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1966, 0.1).
narrative_ontology:measurement(udhr_su_t1984, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1984, 0.12).
narrative_ontology:measurement(udhr_su_t2001, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(udhr_su_t2025, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.1).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_4_derogation_regime).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, un_cat_torture_prohibition).

% DUAL FORMULATION NOTE:
% UDHR Article 3 kernel decomposes into three constraint stories: negative_liberty_reading (substantive non-interference, low epsilon for state restraint), positive_entitlement_reading (substantive provision, high epsilon for state obligation), and this procedural_hybrid_reading (procedural floor, moderate epsilon for compliance infrastructure). The procedural reading is the shared infrastructure both substantive readings depend on — habeas corpus and torture prohibition are invoked by both sides. Network edges point from procedural to substantive because the procedural floor's degradation (e.g., torture normalization) undermines both negative liberty and positive entitlement claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.2).
constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
