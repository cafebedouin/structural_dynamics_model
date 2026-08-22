% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 (Feudal Prerogative Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta's Clause 39 reads: 'No free man shall be seized or
 *   imprisoned, or stripped of his rights or possessions, except by the
 *   lawful judgment of his equals or by the law of the land.' Under the
 *   feudal prerogative reading, this clause preserves narrow procedural
 *   rights within the established hierarchical order of crown and nobility,
 *   not universal individual rights. The constraint operates on the feudal
 *   class alone—elite peers subject to crown authority—and vindicates the
 *   doctrine that even absolute crown prerogative is bounded by feudal
 *   reciprocal obligation and peer judgment. This reading interprets 'law of
 *   the land' as feudal custom and 'equals' as literal peers of the same
 *   noble rank, not all free persons. The constraint coordinates crown and
 *   nobility by formalizing procedures the crown historically observed; it
 *   does not generate a universal mandate against arbitrary power.
 *   Extractiveness is low because the constraint preserves authority
 *   relations rather than challenging them; suppression is minimal because
 *   feudal hierarchy is already the operating framework; theater is modest
 *   because the procedural formalities have real, if narrow, function. This
 *   reading stands in contested relation to the liberal due-process reading
 *   (which universalizes beneficiaries) and the originalist limitation
 *   reading (which treats it as a narrow fix for documented 1215 abuses
 *   only).
 *
 * KEY AGENTS:
 *   - Crown: maintains prerogative authority subject to feudal reciprocal obligation and the procedural requirement to judge peers by their equals.
 *   - Nobility/Peers: constrained by the requirement of peer judgment but elevated above arbitrary seizure within the elite class.
 *   - Common freemen and peasants: excluded from the clause's protections—'free man' in 1215 feudal context meant a noble tenant, not the general population.
 *   - Legal tradition/clergy: observers and interpreters, recording and conserving the constraint's text across re-issues and reigns.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.15).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 (Feudal Prerogative Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'c1feee6f-c105-495e-9763-4b506015962d').
narrative_ontology:cs_kernel_codification('c1feee6f-c105-495e-9763-4b506015962d', fixed_text).
narrative_ontology:cs_authority_grounding('c1feee6f-c105-495e-9763-4b506015962d', lineage).
narrative_ontology:cs_interpretation_layer_present('c1feee6f-c105-495e-9763-4b506015962d').
narrative_ontology:cs_reading_relation('c1feee6f-c105-495e-9763-4b506015962d', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1feee6f-c105-495e-9763-4b506015962d', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('c1feee6f-c105-495e-9763-4b506015962d', foundational, feudal_hierarchy_is_legitimate_order).
narrative_ontology:cs_axiom_status(feudal_hierarchy_is_legitimate_order, holdable).
narrative_ontology:cs_axiom_grounding('c1feee6f-c105-495e-9763-4b506015962d', feudal_hierarchy_is_legitimate_order, conventional).
narrative_ontology:cs_axiom('c1feee6f-c105-495e-9763-4b506015962d', foundational, elite_procedure_is_constraint_sufficient).
narrative_ontology:cs_axiom_status(elite_procedure_is_constraint_sufficient, overridden).
narrative_ontology:cs_axiom_grounding('c1feee6f-c105-495e-9763-4b506015962d', elite_procedure_is_constraint_sufficient, deontological).
narrative_ontology:cs_reference_frame('c1feee6f-c105-495e-9763-4b506015962d', feudal_reciprocal_obligation_framework).
narrative_ontology:cs_drift_state('c1feee6f-c105-495e-9763-4b506015962d', early_modern_liberal_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c1feee6f-c105-495e-9763-4b506015962d', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_and_nobility).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_hierarchy_legitimacy).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, crown_procedural_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The crown sets and enforces feudal procedure; nobles accept the constraint on crown authority in exchange for formalized peer judgment rights. The crown benefits from the constraint because it stabilizes elite expectation and reduces the risk of noble rebellion through arbitrary seizure. The nobility benefits because they gain assurance of procedural formality and peer judgment. Both are coordinated within the same hierarchy and do not experience the constraint as imposed extraction, but as mutual obligation formalized.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_and_nobility, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, crown_and_nobility, beneficiary).

% Record, interpret, and preserve the clause across re-issues and subsequent reigns. Their role is to maintain the text's integrity and transmit the constraint's authority through institutional lineage. They are neither payers nor beneficiaries of the feudal procedure itself, but they serve as the interpretive mediators between the text and its application.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_clergy_and_scribes, observer,
    organized, generational, constrained, national).

% Excluded from Clause 39's protections in the feudal prerogative reading. In medieval feudal context, 'free man' referred to a noble tenant, not the general population. Commoners have no procedural rights under this clause and no standing to claim peer judgment. They are not named as victims because the constraint was never intended to protect them; the constraint preserves hierarchy rather than challenging it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, common_freemen_and_peasants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, crown_and_nobility).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes feudal reciprocal obligation between crown and nobility by institutionalizing peer judgment as a constraint on arbitrary crown seizure. Solves the coordination problem of elite expectation instability: without formalized procedure, nobles live in fear of sudden seizure and may rebel; with it, they accept crown authority as bounded by feudal reciprocity.
% TRANSFER_FUNCTION: Does not transfer wealth or resources from one party to another. Instead, it transfers authority: it assigns the judgment function to peers rather than the crown alone, and assigns the crown the obligation to accept peer judgment in matters of elite property and liberty.
% ABSENT_VOICES: Common freemen and peasants, whose status is outside the clause's scope and who would object if they were consulted that they receive no protections. Subsequent liberal interpreters, whose reading universalizes the clause's scope, are anachronistic absences (not present in 1215) but eventually become present and contest the feudal reading's narrowness.
% DISAPPEARANCE_RATIONALE: From the feudal prerogative standpoint, if Clause 39 disappeared overnight, the world rearranges because elite procedure becomes uncertain and noble rebellion becomes more likely—crown authority loses its procedural legitimacy and reverts to pure force. From the liberal standpoint, the disappearance would signal the death of universal rights. The contest is about what the disappearance would actually entail: does it remove a real constraint on crown power (feudal reading) or symbolically abandon universal protections (liberal reading)?
% FOUNDING_PROBLEM: Arbitrary crown seizure of elite lands and persons without judgment, and the resulting instability in feudal hierarchy as nobles cannot rely on crown respect for reciprocal obligation. The 1215 context saw King John using feudal incidents (wardship, relief, marriage rights) to extract from nobles beyond traditional limits, and confiscating lands without judgment when nobles displeased him.
% FOUNDING_PROBLEM_CORROBORATION: The feudal prerogative reading attests the founding problem remains live: nobles must still fear arbitrary crown action and require procedural formality. Contemporary chronicles and noble testimony from the 1215 rebellion (Henry of Huntingdon, Roger of Wendover) corroborate the original problem. However, the liberal reading's testimony contests both the founding problem status (claiming the original problem is dead, superseded by universal rights concerns) and the narrow scope of the solution (claiming Clause 39 addresses a broader individual-rights problem). The corroboration is intra-feudal: other feudal theorists and practitioners, but not the commoner population or the liberal tradition.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28 because the constraint is presented as coordination (formalized feudal reciprocal obligation) rather than extraction; the crown receives no additional rents from the clause, only the stability of predictable elite procedure. However, extractiveness creeps upward slightly over the interval (0.18 to 0.28) as subsequent monarchs and legal interpreters gradually expand the language 'law of the land' beyond feudal procedure, lifting the extractiveness of the constraint by making it seem to promise more universal protections while the feudal reading still claims it as its own text. This represents a mild form of mandatrophy: the original feudal function (formalizing noble obligation) persists but the clause's language gets reinterpreted toward universal scope, creating a gap between the feudal beneficiary structure and the expanding rhetorical scope. Suppression is low (0.15 at interval end) because the feudal hierarchy is the operating default—no active suppression machinery is needed to keep commons excluded, because they were never in the domain of the clause. Theater is low (0.22) because the procedural forms have genuine feudal function: peers do judge each other, and the crown does (mostly) honor the obligation. The metrics reflect that the feudal reading describes a real coordination within the elite class, not a false front. Accessibility collapse is moderate (0.65) because once the feudal reading is understood as the constraint's true reference frame, alternatives (universal rights, absolute prerogative unconstrained even by feudal custom) are partly foreclosed by the text itself, though new readings (liberal expansion) remain live. Resistance is low (0.18) because the feudal hierarchy itself is unchallenged in the 1215-1350 interval; the constraint operates within the accepted framework.
 *
 * PERSPECTIVAL GAP:
 *   The feudal prerogative reading and the liberal due-process reading look at the exact same text and compute radically different classifications from the same structural data. From the feudal seat (crown and nobility), the constraint is coordination—a reasonable formalization of existing reciprocal obligation. From the liberal seat (the universalizing interpreter), the same text is an inchoate promise of universal rights that feudal hierarchy violates. The engine computes per-seat classifications from power, exit, and beneficiary/victim declarations; the two readings author different beneficiary/victim sets and therefore produce different directionalities and different effective extractions. The perspectival gap IS the contest. This story describes the feudal seat's reading; the liberal reading is a separate constraint story with its own beneficiary set, its own ε, and its own metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading names no victim seats and one beneficiary seat: crown_and_nobility. The constraint is symmetrically positioned for crown and nobility—both coordinate within it, both benefit from formalized procedure as an alternative to arbitrary assertion. Directionality is symmetric (d ≈ 0.5) because the constraint coordinates a reciprocal relationship: the crown accepts the obligation to judge by equals, nobility accepts the crown's ultimate authority. There is no asymmetric extraction within this reading's frame. Commoners are outside the constraint's scope, so they are not named as stakeholders; the constraint is not extractive against them within the feudal prerogative reading, because they were never protected to begin with. The liberal reading, by contrast, would name commoners as victims and claim high extractiveness precisely because it universalizes the beneficiary class and measures extraction against that wider frame. The two readings differ fundamentally in their victim/beneficiary sets, which is why they are separate constraints with different ε values.
 *
 * MANDATROPHY ANALYSIS:
 *   The feudal prerogative reading faces a specific mandatrophy scenario: the original founding problem (formalizing feudal reciprocal obligation between crown and nobles) remains live through the medieval period (1215–1350 interval), but the constraint's language is gradually co-opted by liberal interpreters who expand 'law of the land' toward universal scope. By the early modern period, the same text is cited as the foundation of due-process rights it was never intended to ground. The feudal reading does not resolve this through its own framework—it insists the founding problem is still live (nobles still need protection from arbitrary crown action) while the text gets reinterpreted as solving a different problem (individual rights against the state). This is not a false summit because the feudal reading does not claim naturalness; Clause 39 is clearly a human arrangement (a charter). But it is a genuine instance of constraint capture: the liberal reading successfully colonizes the feudal text and leverages its authority for a different purpose. The measured increase in theater_ratio (0.08 to 0.22) over the interval reflects this: as the feudal procedural function becomes vestigial and the clause's rhetorical function (symbolizing universal rights) grows, theater rises. The constraint becomes more about performing universality than delivering feudal reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_vs_liberal_reading_boundary,
    'Does Clause 39''s mandate to govern ''by the law of the land'' and ''judgment of peers'' establish protective rights universal to all free persons, or merely formalize existing feudal procedures for the elite peer class only?',
    'Historical-textual analysis of the 1215 charter''s original beneficiary set, the vocabulary of ''liber homo'' (free man) in feudal context, and the charter''s explicit exemptions and narrow ratification history. Cross-reference with subsequent re-issue language (1217, 1225) and how medieval courts interpreted the scope clause.',
    'If the feudal reading is correct, the constraint applies protections only to a narrow elite and vindicates feudal hierarchy; if the liberal reading is correct, the same text instantiates universal individual rights and forecloses the feudal reading within a single legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_vs_liberal_reading_boundary, conceptual, 'Whether Clause 39 addresses feudal procedure for elites or establishes universal rights.').

omega_variable(
    kernel_codification_stability,
    'How stable is the 1215 text as a fixed kernel? Is Clause 39 the same textual constraint across its re-issues (1217, 1225, 1290 version), or does the changing text dissolve the identity of the kernel itself?',
    'Textual comparison of the three major re-issues; analysis of whether the scribal changes represent substantive legal reframing or notarial variation. Examination of contemporary glosses and judicial interpretation to identify whether the medieval common law tradition treated the clause as a coherent constraint across re-issues.',
    'If the text is stable, a feudal reading can claim the kernel itself; if the text shifted fundamentally, the kernel identity dissolves and each version is a separate constraint with separate readings. This affects the scope of the constraint-family network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_stability, empirical, 'Whether the Clause 39 kernel survives across re-issues or fragments into distinct constraints.').

omega_variable(
    extraction_measurement_frame_ambiguity,
    'Who is the victim of this constraint? In the feudal prerogative reading, the crown and nobility are coordinated in a hierarchy and not extracted from by each other through this clause. But is the constraint extractive against the commoners excluded from its protections, or is it simply non-protective (a negative fact, not extraction)?',
    'Distinguish extraction (active coercion pulling value out) from exclusion (passive non-inclusion). If Clause 39 actively suppresses commoners'' attempts to claim procedural rights, it is extractive; if it merely leaves them unprotected while defining elite procedure, it is exclusionary but not extractive. The distinction pivots on whether feudal procedure actually transferred resources from commoners or merely preserved existing hierarchies.',
    'If extracted against commoners, the constraint''s extractiveness is higher and it sits nearer snare; if it is pure coordination within the elite class, extractiveness stays low and the rope reading holds. This affects classification and the scope of the beneficiary/victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_frame_ambiguity, conceptual, 'Whether Clause 39 actively extracts from commoners or merely excludes them from elite protections.').

omega_variable(
    medieval_vs_early_modern_authority_grounding,
    'What grounds the authority of Clause 39 in medieval context: the crown''s voluntary concession (extraction-based authority, where the crown retains the power to revoke), feudal custom (practice-based authority, where noble consensus sustains the constraint), or lineage to a prior legal tradition (authority grounded in tradition)?',
    'Examine the 1215 charter''s framing as a peace settlement versus a legally binding constitutional provision; analyze whether the crown treated the clause as binding on itself or contingent; determine whether nobles enforced it through collective action (practice) or crown honor (extraction-based hierarchy).',
    'If extraction-based (the crown preserves the clause to maintain noble loyalty), authority could erode if the crown''s interest shifts; if practice-based, it depends on noble enforcement capacity; if tradition-based, it persists through institutional lineage. This affects the reference_frame/drift_state analysis and the cs_structure.authority_grounding classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_vs_early_modern_authority_grounding, empirical, 'What grounds the authority structure of medieval Clause 39.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(magn_tr_t50, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t150, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement_basis(magn_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(magn_be_t50, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.26).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t150, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 150, 0.28).
narrative_ontology:measurement_basis(magn_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 50, 0.11).
narrative_ontology:measurement_basis(magn_su_t50, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 100, 0.14).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t150, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 150, 0.15).
narrative_ontology:measurement_basis(magn_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__feudal_prerogative_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 is a contested kernel with three co-existing readings instantiated as separate constraints. The feudal_prerogative_reading (this story) addresses the clause from the standpoint of medieval feudal hierarchy and elite procedure. The liberal_due_process_reading and originalist_limitation_reading are sibling constraints sharing the same text but instantiating different ε values, beneficiary/victim structures, and authority-grounding assumptions. All three are live interpretive positions; the readings coexist across different scholarly and political communities. See constraint family documentation for the kernel contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
