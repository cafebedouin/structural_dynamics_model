% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Authority Absorbed into Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the parliamentary sovereignty reading of
 *   Magna Carta's constraint authority: the charter's restraints survive only
 *   as enacted into statute law by Parliament, which inherits the Crown's
 *   constrained authority but retains unlimited power to revise or repeal any
 *   charter-derived provision. The reading is one of three declared readings
 *   of the magna_carta_constraint_authority kernel. It claims tangled_rope
 *   type: a genuine coordination function (democratic legislative authority,
 *   legal continuity) coexists with asymmetric extraction (majorities can
 *   remove minority protections). The engine computes per-seat
 *   classifications from the structural data; the claimed_type is author's
 *   structural judgment, independent of metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.45).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.35).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Authority Absorbed into Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '7b57562a-57a3-41eb-aa66-ebf9075f8b36').
narrative_ontology:cs_kernel_codification('7b57562a-57a3-41eb-aa66-ebf9075f8b36', fixed_text).
narrative_ontology:cs_authority_grounding('7b57562a-57a3-41eb-aa66-ebf9075f8b36', lineage).
narrative_ontology:cs_interpretation_layer_present('7b57562a-57a3-41eb-aa66-ebf9075f8b36').
narrative_ontology:cs_reading_relation('7b57562a-57a3-41eb-aa66-ebf9075f8b36', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b57562a-57a3-41eb-aa66-ebf9075f8b36', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_axiom('7b57562a-57a3-41eb-aa66-ebf9075f8b36', foundational, parliament_cannot_bind_successors).
narrative_ontology:cs_axiom_status(parliament_cannot_bind_successors, holdable).
narrative_ontology:cs_axiom_grounding('7b57562a-57a3-41eb-aa66-ebf9075f8b36', parliament_cannot_bind_successors, conventional).
narrative_ontology:cs_axiom('7b57562a-57a3-41eb-aa66-ebf9075f8b36', foundational, charter_provisions_are_statutory_not_constitutional).
narrative_ontology:cs_axiom_status(charter_provisions_are_statutory_not_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('7b57562a-57a3-41eb-aa66-ebf9075f8b36', charter_provisions_are_statutory_not_constitutional, conventional).
narrative_ontology:cs_reference_frame('7b57562a-57a3-41eb-aa66-ebf9075f8b36', glorious_revolution_settlement).
narrative_ontology:cs_drift_state('7b57562a-57a3-41eb-aa66-ebf9075f8b36', post_human_rights_act_1998, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b57562a-57a3-41eb-aa66-ebf9075f8b36', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legislative_agenda_setters).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, political_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unpopular_groups).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rights_claimants_without_majority_support).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legislative_absolutism).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, democratic_legitimacy_through_representation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the legislative agenda and can enact, amend, or repeal any law including those derived from Magna Carta. Inherits the constraint authority of the Crown but exercises it through democratic majoritarian procedures. Collects political capital from being the ultimate constitutional authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Cabinet ministers, party leaders, and committee chairs who shape legislative priorities. Benefit from the flexibility to address policy problems without charter-based judicial vetoes. Their power derives from controlling the parliamentary timetable and whipping votes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legislative_agenda_setters, beneficiary,
    institutional, biographical, mobile, national).

% Groups lacking numerical or institutional influence who depend on constitutional restraints for protection. When Parliament absorbs and can revise charter provisions, these groups lose the fixed protections that surviving as higher law would provide. Exit is effectively impossible — they cannot leave the polity and have no alternative protector.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, political_minorities, payer,
    powerless, biographical, trapped, national).

% Groups subject to majority hostility (dissidents, minority religions, ethnic minorities, political opponents). Parliamentary sovereignty means their protections exist only at the majority's sufferance. Historical examples: wartime internment, sedition laws, anti-terror legislation passed with majoritarian support.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unpopular_groups, payer,
    powerless, biographical, trapped, national).

% Individuals or groups asserting rights claims that lack popular backing. Can access courts but face legislative override. Their exit options are litigation (costly, uncertain), political mobilization (diffuse, slow), or emigration (high barrier). The constraint extracts their security by making rights contingent on legislative grace.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rights_claimants_without_majority_support, payer,
    moderate, immediate, constrained, national).

% Academic observers who analyze the tension between parliamentary sovereignty and rights protection. Do not bear costs or collect benefits from the constraint directly. Their analysis informs but does not determine constitutional outcomes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of legitimate authority after revolution: provides a recognized, continuous legislative body that can make binding decisions for the polity without requiring fresh consent for each act. Absorbs the Crown's constraint authority into a democratically accountable institution.
% TRANSFER_FUNCTION: Transfers constitutional constraint authority from an immutable charter (Magna Carta as higher law) to a revisable legislative body (Parliament). Moves the power to define rights and limits from a fixed text to a majoritarian process. The extraction is the loss of entrenched protections for those who cannot command legislative majorities.
% ABSENT_VOICES: Future generations who will inherit the constitutional framework but have no vote in current parliamentary decisions. Colonized peoples and subjects of the Crown/Parliament historically excluded from representation. The dead — original parties to the charter — whose understanding of its binding force is overridden by living legislators.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over Magna Carta provisions vanished overnight — i.e., if charter provisions became judicially entrenched higher law immune to legislative repeal — the UK constitutional order would fundamentally restructure. Courts would gain veto power over legislation touching charter-derived rights. The legislative agenda would shift from policy choice to constitutional compliance. Minority protections would strengthen but democratic responsiveness would narrow.
% FOUNDING_PROBLEM: The problem of legitimate authority after the Crown's absolute power was contested: how to constrain arbitrary rule while maintaining a single, final lawmaking authority that can act decisively for the common good. Magna Carta began as a baronial check on the King; the Glorious Revolution and subsequent evolution transferred that checking function to Parliament, making the legislature both the heir to and the master of the charter's restraints.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty proponents (Dicey, modern government ministers) attest the founding problem is live: decisive democratic governance requires unconstrained legislative power. Rights theorists (Dworkin, Allan, international human rights bodies) attest it is dead: the problem of arbitrary power has migrated from the Crown to the legislative majority, and the solution (entrenchment) has been rejected by this reading. The European Convention on Human Rights and the Human Rights Act 1998 represent an external corroboration that the founding problem persists in modified form — the UK accepted external constraint while maintaining formal parliamentary sovereignty.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).
:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects moderate extraction: Parliament extracts the security of entrenched rights from minorities while providing coordination (stable legislative authority). Suppression (0.35) is moderate: no active coercion beyond normal legislative process, but victims are structurally trapped. Theater (0.25) is low-moderate: the sovereignty claim is genuinely exercised, not merely performed. Accessibility collapse (0.4) reflects that alternatives (entrenched rights, judicial review) exist conceptually but are politically foreclosed. Resistance (0.55) is significant: judicial pushback (common law constitutionalism), international pressure (ECHR), and academic critique persist. Measurements show extractiveness declining from feudal peak (1215) through Glorious Revolution (1689) and Parliament Acts (1911), with slight uptick in late 20th century as legislative volume expands.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majority and agenda-setters are beneficiaries (d near 0): they control the constraint, collect political capital, face no exit barrier. Political minorities and unpopular groups are victims (d near 1): bear extraction, trapped exit, powerless. Rights claimants without majority support are intermediate payers (d ~0.7): constrained exit via courts/politics, moderate power. Constitutional scholars are analytical observers (d = 0.5). The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining arbitrary Crown power) is contested: partly solved (Crown constrained), partly migrated (arbitrary power now resides in legislative majority). The arrangement persists not because the original problem is live in its original form, but because the solution (parliamentary sovereignty) created a new beneficiary class (legislative majorities) with incentive to maintain it. This is classic mandatrophy: the constraint's function has shifted from checking power to enabling majoritarian power, but the charter's symbolic authority legitimizes the arrangement. The theater_ratio rise over time reflects this — more performance of charter reverence, less substantive restraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''magna_carta_constraint_authority'', and does the parliamentary_sovereignty_reading instantiate a distinct constraint with its own ε, beneficiary/victim structure, and type?',
    'Commitment-system analysis: compare the structural outputs (ε, directionality, classification) of this reading against the sibling readings (living_constitutionalism_reading, feudal_obsolescence_reading). If ε values differ substantially and beneficiary/victim sets are non-overlapping, the readings are distinct constraints per ε-invariance.',
    'If confirmed as distinct constraints, each reading gets its own classification and the kernel''s contestation is modeled as a constraint family linked by network.affects_constraints. If not distinct, the readings are interpretive variants of a single constraint and the family model collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the parliamentary_sovereignty_reading is a structurally distinct constraint from its sibling readings of the same kernel.').

omega_variable(
    living_constitutionalism_foreclosure,
    'Does the parliamentary_sovereignty_reading''s core premise (Parliament can revise/repeal any charter provision) logically foreclose the living_constitutionalism_reading''s core premise (Magna Carta establishes inherited due process binding all subsequent rulers) within a single legal framework?',
    'Analyze whether a single constitutional framework can simultaneously hold: (a) Parliament has unlimited revisionary power over charter provisions, and (b) Charter provisions bind Parliament through juridical precedent. If the framework must choose, the relation is forecloses. If both can operate in different domains or at different levels, the relation is coexists_with or influences.',
    'Determines cs_structure.reading_relations entry for living_constitutionalism_reading. Foreclosure would mean the readings cannot be held by the same authority structure; coexistence means they occupy different institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_constitutionalism_foreclosure, conceptual, 'Structural relationship between parliamentary sovereignty and living constitutionalism readings of Magna Carta authority.').

omega_variable(
    feudal_obsolescence_influence,
    'Does the parliamentary_sovereignty_reading create structural downstream pressure on the feudal_obsolescence_reading by absorbing the charter''s authority into statute law?',
    'Trace whether parliamentary absorption of Magna Carta provisions (e.g., Habeas Corpus Acts, Bill of Rights 1689, Parliament Acts) renders the feudal_obsolescence_reading''s claim (charter has no binding authority) partially satisfied or structurally transformed. The reading may influence by making obsolescence the operational reality for most provisions while preserving symbolic authority.',
    'If influences, the parliamentary reading''s legislative activity creates the conditions the feudal reading describes — a form of self-fulfilling structural pressure. If coexists_with, the readings address different questions (current authority vs. historical origin).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_obsolescence_influence, conceptual, 'Whether parliamentary absorption of charter authority structurally pressures the feudal obsolescence reading.').

omega_variable(
    minority_protection_gap,
    'Is the victim set (political minorities, unpopular groups, rights claimants without majority support) an irreducible feature of parliamentary sovereignty, or does the Human Rights Act 1998 / ECHR membership create a structural modification that the base_properties metrics do not capture?',
    'Compare pre-1998 and post-1998 extraction/suppression metrics for the victim stakeholders. If the HRA creates a de facto (though formally repealable) entrenchment that substantially reduces effective extraction for victims, the current metrics may overstate extraction. The omega documents whether the reading''s ε should reflect the formal sovereignty or the operational reality.',
    'If the HRA/ECHR layer meaningfully constrains parliamentary revision in practice, the constraint''s effective type may shift toward rope or scaffold for victim seats, even while the formal reading remains tangled_rope. This would show as seat divergence in engine computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_gap, empirical, 'Whether the Human Rights Act and ECHR membership modify the victim extraction profile under parliamentary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.2).
narrative_ontology:measurement(magn_tr_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1911, 0.22).
narrative_ontology:measurement(magn_tr_t1949, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(magn_tr_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1972, 0.23).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1215, 0.65).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.55).
narrative_ontology:measurement(magn_be_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1911, 0.42).
narrative_ontology:measurement(magn_be_t1949, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1949, 0.38).
narrative_ontology:measurement(magn_be_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1972, 0.44).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.5).
narrative_ontology:measurement(magn_su_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1911, 0.35).
narrative_ontology:measurement(magn_su_t1949, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(magn_su_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1972, 0.32).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, human_rights_act_1998).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, european_convention_on_human_rights).

% DUAL FORMULATION NOTE:
% This constraint and its two sibling readings form the Magna Carta constraint family. The kernel 'magna_carta_constraint_authority' decomposes into three structurally distinct constraints with different ε values and victim sets. Parliamentary sovereignty reading has moderate extraction (0.45) and tangible victims; living constitutionalism reading would show lower extraction (charter as shield) but higher suppression (judicial enforcement); feudal obsolescence reading would show near-zero extraction (no living authority) but high theater (symbolic invocation). The family is linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
