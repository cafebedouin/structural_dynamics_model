% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint story models the judicial supremacy reading of
 *   constitutional interpretive authority: courts possess final authority to
 *   interpret the constitution and may nullify legislative acts that violate
 *   constitutional/fundamental rights. The arrangement is presented as a
 *   coordination mechanism protecting minority rights against majoritarian
 *   overreach (rope function). Simultaneously, it concentrates interpretive
 *   power in an unelected, life-tenured judiciary that benefits from the
 *   monopoly and whose rulings often entrench preferences aligned with
 *   professional/elite networks rather than democratic majorities (extraction
 *   function). The constraint requires active enforcement — judicial review
 *   institutions, precedent systems, enforcement of judgments against
 *   coordinate branches — and its persistence depends on suppressing
 *   legislative self-interpretation and alternative constitutional visions.
 *   The claimed_type is tangled_rope, reflecting the dual
 *   coordination/extraction structure. Metrics are authored independently:
 *   extractiveness has risen over the interval as judicial review expanded
 *   from clear textual violations to contested normative judgments; theater
 *   has increased as procedural rituals (standing, ripeness, tiers of
 *   scrutiny) increasingly manage legitimacy rather than decide cases;
 *   suppression has risen as coordinate branches' pushback (court-packing
 *   threats, jurisdiction stripping, non-compliance) has been met with
 *   institutional hardening.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '729b72a6-d55a-4298-9cbc-b104e337892e').
narrative_ontology:cs_kernel_codification('729b72a6-d55a-4298-9cbc-b104e337892e', formalized).
narrative_ontology:cs_authority_grounding('729b72a6-d55a-4298-9cbc-b104e337892e', lineage).
narrative_ontology:cs_interpretation_layer_present('729b72a6-d55a-4298-9cbc-b104e337892e').
narrative_ontology:cs_reading_relation('729b72a6-d55a-4298-9cbc-b104e337892e', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('729b72a6-d55a-4298-9cbc-b104e337892e', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('729b72a6-d55a-4298-9cbc-b104e337892e', foundational, judiciary_as_final_rights_guardian).
narrative_ontology:cs_axiom_status(judiciary_as_final_rights_guardian, holdable).
narrative_ontology:cs_axiom_grounding('729b72a6-d55a-4298-9cbc-b104e337892e', judiciary_as_final_rights_guardian, deontological).
narrative_ontology:cs_axiom('729b72a6-d55a-4298-9cbc-b104e337892e', secondary, constitutional_text_requires_judicial_enforcement).
narrative_ontology:cs_axiom_status(constitutional_text_requires_judicial_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('729b72a6-d55a-4298-9cbc-b104e337892e', constitutional_text_requires_judicial_enforcement, conventional).
narrative_ontology:cs_reference_frame('729b72a6-d55a-4298-9cbc-b104e337892e', original_judicial_review_mandate).
narrative_ontology:cs_drift_state('729b72a6-d55a-4298-9cbc-b104e337892e', contemporary_rights_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('729b72a6-d55a-4298-9cbc-b104e337892e', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_branch_institution).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_networks).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, elected_lawmakers).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, policy_preferences_of_majoritarian_coalitions).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, fundamental_rights_entrenched_against_majority).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, countermajoritarian_difficulty_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold final interpretive authority over constitutional meaning. Life tenure and institutional prestige insulate them from electoral accountability. Their rulings shape policy across all domains. Exit is near-arbitrage: they can retire strategically, influence successors, and their interpretive legacy persists. They collect the constraint's gains directly — the monopoly on constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, supreme_court_justices, beneficiary,
    institutional, biographical, arbitrage, national).

% Administers and enforces the judicial review system. Controls docket, precedent, and remedial powers. Institutional survival and expansion depend on maintaining interpretive supremacy. Collects structural benefits: budget authority, institutional prestige, recruitment pipeline from elite law schools. Sets the agenda for what constitutional questions are decided and how.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_branch_institution, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_branch_institution, beneficiary).

% Civil society organizations, public interest law firms, and academic networks that litigate rights claims. They gain a structural veto point: courts can invalidate legislation that majoritarian politics would sustain. Their exit is constrained — they depend on the judicial forum; alternative venues (legislatures, referenda) are less favorable for minority-protective claims. They do not administer the constraint but benefit from its operation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_networks, beneficiary,
    organized, generational, constrained, national).

% Elected lawmakers who command legislative majorities. Their policy agenda is subject to judicial nullification on constitutional grounds. They bear the extraction: loss of final authority over the constitutional bounds of their power, policy defeats on issues where they represent majority will. Exit is constrained: constitutional amendment requires supermajorities; court-curbing (jurisdiction stripping, court packing) is politically costly, institutionally dangerous, and historically unreliable.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% Individual legislators whose legislative work can be nullified. They face career incentives to anticipate judicial preferences (self-censorship, strategic drafting) rather than represent constituents. The constraint extracts their effective lawmaking authority. Exit is constrained — they operate within the system; leaving office ends the extraction but also ends their agency.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, elected_lawmakers, payer,
    organized, biographical, constrained, national).

% The aggregate policy preferences of electoral majorities that are filtered through judicial review. Not an agent but a structural payee: the constraint systematically blocks majoritarian policies that conflict with judicially-recognized rights. No exit — preferences exist only within the constraint's filtering.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, policy_preferences_of_majoritarian_coalitions, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(constitutional_interpretive_authority__judicial_supremacy_reading, policy_preferences_of_majoritarian_coalitions).

% Academic commentators who analyze, critique, and theorize the constraint. They do not collect rents or bear costs directly but shape the legitimating discourse. Their exit is analytical — they can adopt any interpretive frame.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative settlement of constitutional meaning that protects fundamental rights against legislative majorities, preventing rights from becoming majoritarian bargaining chips. Solves the credible-commitment problem: rights guarantees would be meaningless if the legislature could interpret them away.
% TRANSFER_FUNCTION: Transfers final constitutional interpretive authority from elected legislatures to unelected courts. Moves policy-determination power on constitutional questions from majoritarian politics to judicial doctrine. The transfer runs from legislative majorities and majoritarian coalitions to the judiciary and rights-advocacy networks.
% ABSENT_VOICES: Citizens in jurisdictions without judicial review (e.g., UK parliamentary sovereignty model) who experience rights protection through political rather than judicial mechanisms. Also absent: the coordinate_construction vision where no branch has final say — its proponents (political constitutionalists, popular constitutionalism scholars) are excluded from the institutionalized interpretive monopoly.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures would become the final interpreters of constitutional bounds. Rights protection would shift to political safeguards (entrenchment statutes, supermajority requirements, political accountability). Majoritarian policies currently blocked would take effect. The constitutional order would reorganize around legislative supremacy or coordinate construction — a fundamental regime change.
% FOUNDING_PROBLEM: The countermajoritarian difficulty: how to entrench fundamental rights against temporary majoritarian overreach in a democratic system. The founding generation (varies by polity: Marbury v. Madison in US, post-war constitutional courts in Europe) built judicial review as the solution — an unelected guardian that could say 'no' to the majority when rights were at stake.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (judiciary, rights networks, liberal legal academia) attest the founding problem remains live: majoritarian overreach is a permanent risk, rights need judicial guardianship. Critics (political constitutionalists, originalists critical of judicial activism, legislative supremacists) attest the problem is dead or transformed: the constraint now serves judicial policy preferences, not rights protection; the countermajoritarian difficulty has become a countermajoritarian practice. Corroboration from outside the beneficiary set: political scientists documenting judicial ideology-policy alignment; historians of court-curbing movements; comparative constitutionalists showing rights protection without judicial supremacy (e.g., New Zealand, UK, Canada pre-1982).
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42 at interval end) reflects that the constraint transfers substantial interpretive authority from elected legislatures to unelected courts. The transfer is not total — legislatures retain vast lawmaking power — but the final-say monopoly on constitutional meaning is a high-value asset. Suppression (0.65) is significant because the constraint's persistence depends on: (a) institutionalizing judicial review so that legislative self-interpretation is structurally disabled; (b) precedent and stare decisis locking in interpretive gains; (c) professional/credential gatekeeping limiting who may authoritatively interpret. Theater ratio (0.28) is moderate: the rights-protection function is genuine (low theater at core), but an expanding penumbral zone uses elaborate doctrinal frameworks to reach policy-preferred outcomes. Accessibility collapse (0.58) reflects that once judicial supremacy is accepted, alternative constitutional visions (parliamentary supremacy, coordinate construction) become politically difficult to instantiate — but not impossible, as regime changes demonstrate. Resistance (0.45) is moderate: coordinate branches resist intermittently (court-curbing legislation, non-acquiescence, appointments politics) but rarely mount sustained structural challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the constraint appears as a necessary coordination mechanism (rope) protecting the constitutional order from legislative overreach. From the legislative seat, it appears as an extractive imposition (snare/tangled_rope) that subordinates democratic will to unelected preferences. From the rights-advocacy seat, it appears as a protective scaffold (transitional until rights are fully legislated) or a permanent guardian. The engine computes per-seat types from the structural data; this commentary documents the structural asymmetry that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary (supreme_court_justices, judicial_branch_institution) is the primary beneficiary: it collects the monopoly on final constitutional meaning, which translates to policy influence, institutional prestige, and career capital for the professional networks that staff and argue before it. Rights_advocacy_networks are secondary beneficiaries: they gain a veto point for rights claims that would lose in majoritarian politics. Legislatures (legislative_majorities, elected_lawmakers) are the primary payers: they lose final authority over the constitutional bounds of their own power, and their policy preferences are subject to veto by a body they cannot directly control. Policy_preferences_of_majoritarian_coalitions are diffuse payers: the constraint systematically filters out majoritarian preferences that conflict with judicially-recognized rights. Directionality derivation: beneficiaries (judiciary, rights networks) have low d (subsidized by constraint); payers (legislatures, majoritarian coalitions) have high d (extracted from); exit options for payers are constrained (constitutional amendment is arduous; court-curbing is politically costly and often fails).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (countermajoritarian protection of fundamental rights) is contested: proponents say it remains live; critics say the constraint has metastasized into judicial policy-making beyond the founding mandate. The constraint persists because the beneficiary coalition (judiciary + rights networks) is institutionally entrenched and the payer coalition (legislatures) lacks a coordinated exit strategy. Mandatrophy is unresolved: the constraint's original coordination function has been layered with extraction that no sunset clause addresses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Is the constraint ''courts possess final interpretive authority'' a natural feature of constitutional governance or a constructed arrangement that benefits identifiable agents?',
    'Compare the structural position of the judiciary under this reading against sibling readings: judicial_supremacy_reading makes the judiciary a beneficiary of interpretive monopoly; parliamentary_supremacy_reading makes the legislature the beneficiary; coordinate_construction_reading distributes interpretive authority without a final arbiter. The natural-law vs. constructed ambiguity is irreducible within any single reading.',
    'If natural-law: the beneficiary structure is incidental to a necessary coordination function (mountain/rope). If constructed: the beneficiary structure is the point, making the constraint a tangled_rope (coordination + asymmetric extraction). This reading''s authored metrics and claimed_type assume the constructed reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether judicial interpretive supremacy is a natural constitutional necessity or a constructed arrangement benefiting the judiciary and allied networks.').

omega_variable(
    extraction_boundary_ambiguity,
    'Where does the genuine coordination function (rights protection, constitutional stability) end and asymmetric extraction (judicial policy-making, entrenchment of elite preferences) begin?',
    'Empirical study of judicial nullification patterns: proportion of struck-down laws that clearly violate enumerated rights text vs. those that reflect contested normative judgments. Track whether nullification rates correlate with judicial ideology shifts rather than textual clarity.',
    'If extraction is inseparable from coordination, the constraint is a tangled_rope with high effective extraction for legislatures. If coordination dominates, it trends toward rope. If extraction dominates with cover story, it trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_ambiguity, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    democratic_legitimacy_deficit,
    'Does the constraint''s legitimating story (rights-compliance over democratic will) contain an irreducible democratic legitimacy deficit that no amount of doctrinal elaboration can resolve?',
    'Longitudinal study of public acceptance of judicial nullification across regime types: does acceptance track perceived rights-protection efficacy, or does it track alignment with majority preferences? Measure whether the deficit widens when courts strike down popular legislation on contested grounds.',
    'A persistent legitimacy deficit that widens under countermajoritarian exercise suggests the constraint''s enforcement depends on suppression of democratic contestation — supporting snare/tangled_rope classification. A stable or narrowing deficit supports the rope/coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, preference, 'Whether the constraint carries an irreducible democratic legitimacy deficit that enforcement must manage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_tr_t25, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_tr_t75, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_tr_t100, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_be_t25, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_be_t75, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_be_t100, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_su_t25, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_su_t75, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(constitutional_interpretive_authority__judicial_supremacy_reading_su_t100, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three in the constitutional_interpretive_authority kernel family. The judicial_supremacy_reading instantiates a constraint with high extraction (judiciary as beneficiary, legislature as payer) and active enforcement. The parliamentary_supremacy_reading would instantiate a constraint with legislature as beneficiary and minority rights-holders as payers. The coordinate_construction_reading would instantiate a lower-extraction, distributed-authority constraint. All three share the same referent (the constitutional interpretive arrangement) but author different ε, beneficiaries, and victims per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, powerful, 0.85).
constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
