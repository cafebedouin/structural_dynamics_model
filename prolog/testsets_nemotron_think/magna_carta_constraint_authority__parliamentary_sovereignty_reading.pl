% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Restraints as Parliamentary Statute Absorption
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   Magna Carta's restraints survive in the UK constitution only as absorbed
 *   into parliamentary statute law. Parliament inherits the constraint
 *   authority of the charter but retains the power to revise or repeal any
 *   provision by ordinary legislative process. This reading instantiates the
 *   parliamentary sovereignty interpretation: the charter provides symbolic
 *   legitimacy and historical narrative for legislative supremacy, while its
 *   substantive restraints exist only at Parliament's sufferance. The
 *   constraint coordinates the transfer of authority from Crown to Parliament
 *   but extracts from minorities whose protections depend on majoritarian
 *   grace. The claimed type is tangled_rope — genuine coordination function
 *   (stable legal framework) with asymmetric extraction (minorities
 *   unprotected by majoritarian legislation) requiring active enforcement
 *   (parliamentary legislative machinery).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.45).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.38).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraints as Parliamentary Statute Absorption").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '355c307b-5fec-4702-802d-1e893f5c9a9e').
narrative_ontology:cs_kernel_codification('355c307b-5fec-4702-802d-1e893f5c9a9e', formalized).
narrative_ontology:cs_authority_grounding('355c307b-5fec-4702-802d-1e893f5c9a9e', lineage).
narrative_ontology:cs_interpretation_layer_present('355c307b-5fec-4702-802d-1e893f5c9a9e').
narrative_ontology:cs_reading_relation('355c307b-5fec-4702-802d-1e893f5c9a9e', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('355c307b-5fec-4702-802d-1e893f5c9a9e', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('355c307b-5fec-4702-802d-1e893f5c9a9e', foundational, parliamentary_supremacy_over_charter).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_charter, holdable).
narrative_ontology:cs_axiom_grounding('355c307b-5fec-4702-802d-1e893f5c9a9e', parliamentary_supremacy_over_charter, conventional).
narrative_ontology:cs_axiom('355c307b-5fec-4702-802d-1e893f5c9a9e', foundational, statute_absorbs_and_supersedes_charter).
narrative_ontology:cs_axiom_status(statute_absorbs_and_supersedes_charter, holdable).
narrative_ontology:cs_axiom_grounding('355c307b-5fec-4702-802d-1e893f5c9a9e', statute_absorbs_and_supersedes_charter, conventional).
narrative_ontology:cs_reference_frame('355c307b-5fec-4702-802d-1e893f5c9a9e', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('355c307b-5fec-4702-802d-1e893f5c9a9e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('355c307b-5fec-4702-802d-1e893f5c9a9e', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, general_citizenry).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, statutory_supremacy_over_charter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits the constraint authority of Magna Carta through statutory absorption; can revise or repeal any charter provision by simple legislative majority. Enacts, amends, and repeals the very restraints that nominally bind it. Collects institutional legitimacy from the charter's symbolic authority while retaining unconstrained revisionary power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of majoritarian legislation unchecked by entrenched charter protections. Religious minorities, ethnic communities, and political dissidents find their protections contingent on parliamentary majorities rather than constitutional guarantee. Exit requires emigration or political mobilization against structural disadvantages.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, biographical, constrained, national).

% Historically constrained by Magna Carta's restraints on prerogative; now operates within parliamentary statute law that mediates popular will. Gains predictability and democratic legitimacy from statutory framework but loses autonomous prerogative. Can influence legislation through government majority but cannot claim charter immunity from parliamentary revision.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive, payer).

% Interprets and applies statutory absorptions of Magna Carta provisions. Cannot strike down primary legislation; develops common law principles in dialogue with statute. Provides the analytical seat that reads the constraint's operation across cases but lacks revisionary authority over the constraint itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Receives stable legal framework and predictable governance through parliamentary statute. Benefits from the coordination function of absorbed charter restraints — due process, habeas corpus, property protections — but holds no entrenched claim against parliamentary majorities. Electoral participation is the primary exit/check mechanism.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, general_citizenry, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal framework mediating between Crown prerogative and popular will through parliamentary statute law, solving the coordination problem of legitimate authority transfer from feudal charter to democratic legislature.
% TRANSFER_FUNCTION: Moves legislative supremacy from Crown to Parliament while constraining executive power through statutory absorption; minorities bear the cost of protection gaps where majoritarian legislation does not entrench their rights.
% ABSENT_VOICES: Minority communities (religious, ethnic, political dissidents), colonized peoples historically excluded from parliamentary representation, future generations who inherit the statutory framework without consent. These voices would object to the contingency of their protections on parliamentary majorities but are structurally excluded from the constraint's revisionary mechanism.
% DISAPPEARANCE_RATIONALE: If parliamentary absorption of Magna Carta vanished overnight, the UK constitution would lose its primary historical legitimating narrative for legislative supremacy. Common law would revert to either Crown prerogative or judicial activism as the restraint on executive power, and the statutory framework protecting civil liberties would lack its foundational charter authority — triggering constitutional crisis and rearrangement of the legitimacy architecture.
% FOUNDING_PROBLEM: 13th century baronial grievance against arbitrary royal power: need for predictable legal process, protection against arbitrary seizure, and constraint on executive prerogative through lawful procedure.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Maitland, Pollard, Holdsworth) attest the original feudal grievances are resolved; constitutional scholars outside the parliamentary sovereignty tradition (Dicey critics, commonwealth comparativists) attest the principle of restraint survives only as parliamentary grace, not charter right. No beneficiary of parliamentary sovereignty corroborates the founding problem as live.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is moderate (0.45) because Parliament extracts revisionary control over the charter's restraints while providing coordination benefit of stable statute law. Suppression is moderate (0.38) because majoritarian legislation can override minority protections without constitutional barrier, but common law and political convention provide partial resistance. Theater ratio (0.28) reflects performative invocation of Magna Carta in political discourse while substantive restraints remain statutorily contingent. Accessibility collapse (0.55) is moderate — alternatives (entrenched bill of rights, judicial review) exist but are politically inaccessible under current constitutional architecture. Resistance (0.42) reflects historical and ongoing contestation (Chartists, suffragists, human rights advocates) but has not produced structural revision. The measurement series tracks the long drift from feudal charter (low extraction) to modern parliamentary sovereignty (moderate extraction with rising theater).
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat, the arrangement is genuine coordination — it built and maintains the statutory framework that absorbs charter authority. From minority group seats, the same structure operates as enforced extraction — protections are contingent on majorities that need not represent them. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament is the structural beneficiary (d near 0.0) — collects institutional legitimacy from charter symbolism while retaining unconstrained revisionary power. Minority groups are the primary targets (d near 1.0) — bear costs of protection gaps with constrained exit. Crown/executive sits near symmetric (d ~0.5) — constrained by statute but gains democratic legitimacy. Judiciary is analytical (d = 0.5) — observes and interprets but lacks revisionary authority. General citizenry are net beneficiaries (d ~0.2) — receive coordination benefits with electoral exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal power) is dead — resolved by centuries of constitutional evolution. Yet the arrangement persists and has accumulated extraction (Parliament's unconstrained revisionary power over charter-derived rights). This is mandatrophy: the constraint's mandate has outlived its function, but the structure persists because Parliament benefits from the symbolic authority of the charter while bearing no cost for the protection gaps it creates for minorities. The coordination function (stable statute law) is real but the extraction asymmetry (minority vulnerability) is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint properly a distinct reading of the magna_carta_constraint_authority kernel, or does parliamentary sovereignty represent the kernel''s resolution rather than one reading among others?',
    'Comparative constitutional analysis: if other commonwealth systems (Canada, Australia, India) treat Magna Carta as living constitutional precedent rather than parliamentary grace, the kernel admits multiple stable readings. If all systems converge on parliamentary sovereignty, the reading may be the kernel''s resolution.',
    'If this is the kernel''s resolution, the sibling readings are not competing constraints but historical errors or foreign transplants. If it is one reading among others, the kernel structure is genuinely contested and the extraction profile varies by reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether parliamentary sovereignty is a reading of the kernel or the kernel''s settlement.').

omega_variable(
    structural_delta_vs_siblings,
    'What specific structural elements do the sibling readings change relative to this reading''s beneficiary/victim structure and extraction profile?',
    'Author the sibling constraint stories and compare: feudal_obsolescence_reading should show near-zero extraction (no living constraint), living_constitutionalism_reading should show lower extraction on minorities (entrenched protections) but higher on Parliament (judicial constraint). The delta in victim sets and directionality profiles across readings maps the kernel''s structural ambiguity.',
    'If sibling readings produce substantially different victim sets and extraction profiles, the kernel''s structural ambiguity is irreducible — each reading instantiates a different constraint. If profiles converge, the kernel may be less contested than the discourse suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_vs_siblings, empirical, 'Structural differences between this reading and its siblings on the kernel.').

omega_variable(
    minority_protection_mechanism_ambiguity,
    'Is the minority protection gap structural (parliamentary sovereignty necessarily leaves minorities vulnerable) or contingent (political culture, convention, and international law provide de facto protection)?',
    'Comparative analysis of minority outcomes in parliamentary sovereignty systems vs. entrenched bill of rights systems. Track specific legislative episodes where minority protections were overridden by parliamentary majority vs. protected by convention/international obligation.',
    'If structural, the extraction on minorities is necessary to the constraint''s operation — a tangled_rope with irreducible asymmetric extraction. If contingent, the measured extraction overstates the constraint''s inherent extractiveness; the constraint coordinates without necessarily extracting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_mechanism_ambiguity, empirical, 'Whether minority vulnerability is inherent to parliamentary sovereignty or politically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 809).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(magn_tr_t250, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 250, 0.12).
narrative_ontology:measurement(magn_tr_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(magn_tr_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 600, 0.24).
narrative_ontology:measurement(magn_tr_t809, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 809, 0.28).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(magn_be_t250, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 250, 0.3).
narrative_ontology:measurement(magn_be_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(magn_be_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 600, 0.42).
narrative_ontology:measurement(magn_be_t809, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 809, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(magn_su_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement(magn_su_t250, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 250, 0.22).
narrative_ontology:measurement(magn_su_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 400, 0.28).
narrative_ontology:measurement(magn_su_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 600, 0.34).
narrative_ontology:measurement(magn_su_t809, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 809, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliamentary_sovereignty_doctrine).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, human_rights_act_1998_constraint).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_constitutionalism).

% DUAL FORMULATION NOTE:
% This constraint is one reading in the magna_carta_constraint_authority kernel family. The feudal_obsolescence_reading treats the charter as historically exhausted (near-zero extraction, mountain-like). The living_constitutionalism_reading treats charter principles as binding precedent (lower extraction on minorities, higher on Parliament). All three share the kernel but instantiate different constraints with different ε and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, institutional, 0.05).
constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
