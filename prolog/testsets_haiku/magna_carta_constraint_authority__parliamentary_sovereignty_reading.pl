% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Restraints via Parliamentary Sovereignty
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'magna_carta_constraint_authority' — specifically, the
 *   PARLIAMENTARY_SOVEREIGNTY reading. In this reading, Magna Carta's
 *   restraints on Crown prerogative have no independent force; they survive
 *   only insofar as Parliament has enacted and continues to maintain them as
 *   statute law. Parliament holds the revisionary power: it can repeal any
 *   charter provision by simple majority legislation. This reading coexists
 *   with two sibling readings: the feudal_obsolescence reading (which treats
 *   Magna Carta as historically dead, a baronial compact with no binding
 *   authority on modern sovereignty) and the living_constitutionalism reading
 *   (which treats Magna Carta as establishing inherent due process principles
 *   that bind all rulers through judicial precedent regardless of legislative
 *   revision). These are different constraints, instantiating different
 *   structural relationships between restraint and authority. This JSON is
 *   the parliamentary_sovereignty reading only — not a synthesis, not a
 *   averaged metric, not a hedge across readings. It describes the standing
 *   arrangement (parliamentary statute law as the carrier of Magna Carta's
 *   restraints) as this reading sees it, with its own ε, beneficiary set
 *   (Parliament, common lawyers, legislative authority), victim set
 *   (unprotected minorities, the constrained Crown), and type (tangled_rope:
 *   coordination exists—Crown is restrained—but extractive asymmetry
 *   exists—Parliament controls the restraint boundary and can revise it at
 *   will).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.47).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraints via Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '0dc60e30-2748-4542-a61c-6eb61fc1449b').
narrative_ontology:cs_kernel_codification('0dc60e30-2748-4542-a61c-6eb61fc1449b', fixed_text).
narrative_ontology:cs_authority_grounding('0dc60e30-2748-4542-a61c-6eb61fc1449b', lineage).
narrative_ontology:cs_interpretation_layer_present('0dc60e30-2748-4542-a61c-6eb61fc1449b').
narrative_ontology:cs_reading_relation('0dc60e30-2748-4542-a61c-6eb61fc1449b', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('0dc60e30-2748-4542-a61c-6eb61fc1449b', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_axiom('0dc60e30-2748-4542-a61c-6eb61fc1449b', foundational, parliament_supreme_authority_over_restraint_revision).
narrative_ontology:cs_axiom_status(parliament_supreme_authority_over_restraint_revision, holdable).
narrative_ontology:cs_axiom_grounding('0dc60e30-2748-4542-a61c-6eb61fc1449b', parliament_supreme_authority_over_restraint_revision, conventional).
narrative_ontology:cs_axiom('0dc60e30-2748-4542-a61c-6eb61fc1449b', foundational, charter_authority_mediated_through_statute_not_natural_law).
narrative_ontology:cs_axiom_status(charter_authority_mediated_through_statute_not_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('0dc60e30-2748-4542-a61c-6eb61fc1449b', charter_authority_mediated_through_statute_not_natural_law, conventional).
narrative_ontology:cs_axiom('0dc60e30-2748-4542-a61c-6eb61fc1449b', secondary, restraint_revisability_compatible_with_lawful_governance).
narrative_ontology:cs_axiom_status(restraint_revisability_compatible_with_lawful_governance, holdable).
narrative_ontology:cs_axiom_grounding('0dc60e30-2748-4542-a61c-6eb61fc1449b', restraint_revisability_compatible_with_lawful_governance, instrumental).
narrative_ontology:cs_reference_frame('0dc60e30-2748-4542-a61c-6eb61fc1449b', parliamentary_statutory_supremacy).
narrative_ontology:cs_drift_state('0dc60e30-2748-4542-a61c-6eb61fc1449b', contemporary_rights_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0dc60e30-2748-4542-a61c-6eb61fc1449b', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_constrained_prerogative).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_lawyers).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislators).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits the authority to interpret, enforce, and revise Magna Carta's restraints through statute. Can repeal or narrow any charter provision by simple majority legislation. Sets the ongoing boundary between lawful Crown action and parliamentary constraint. Derives legitimacy from representing the popular will; maintains the restraint framework but controls its scope and enforcement.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Prerogative powers are constrained by parliamentary statute law that inherited Magna Carta's restraints. Cannot act contrary to settled parliamentary law; must respect Parliamentary supremacy. Governance authority persists but is hedged by statutory restraint and parliamentary oversight. Exit from the restraint system means denying parliamentary sovereignty itself — structurally impossible within the constitutional framework this reading instantiates.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown, payer,
    institutional, generational, constrained, national).

% Protected by Magna Carta's restraints only so long as Parliament enacts and maintains those protections in statute. If Parliament votes to repeal or narrow a restraint (e.g., habeas corpus protections, due process requirements), the minority's shield dissolves. They bear the cost of parliamentary revisability: their rights depend on majoritarian legislation, not on an entrenched charter. No exit; structured dependence on parliamentary goodwill.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities, payer,
    powerless, biographical, trapped, national).

% Benefit from the absorption of Magna Carta into common law and statute. The legal profession derives authority, precedent, and professional norms from the charter-into-statute lineage. Their interpretive power over statutory restraints is substantial; they mediate between Parliament's revisionary will and the constraint framework's historical authority. Can exit by changing professional discipline or jurisdictional focus; their stakes are high but not categorical.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_lawyers, beneficiary,
    powerful, biographical, mobile, national).

% Individual legislators benefit from the framework by wielding the power to revise Magna Carta's restraints. Can expand protections (gaining legitimacy) or narrow them (removing constraints on state action). Their power to legislate on the restraints is near-complete; they face electoral and reputational costs but not structural exits. The framework amplifies legislative authority at the cost of Charter entrenchment.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislators, beneficiary,
    powerful, biographical, mobile, national).

% Readings grounded in feudal obsolescence (treating Magna Carta as temporally dead) and living constitutionalism (treating it as judicially entrenched across generations) are formally excluded from this framework. They could argue for different constraint structures — fixed historical obsolescence or judicial entrenchment — but are structurally locked out by this reading's axiomatic commitment to parliamentary sovereignty as the sole locus of charter authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, competing_constitutional_readings, excluded,
    institutional, generational, identity_locked, national).

% Interprets and applies the statutes into which Magna Carta's restraints have been absorbed. Courts enforce parliamentary law; they do not enforce Magna Carta as an independent constitutional instrument. Their role is derivative: bound by statute, not by the charter's claimed historical authority. Can influence doctrine through interpretation but cannot override parliamentary supremacy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judicial_authority, observer,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates lawful Crown action with parliamentary constraint by absorbing Magna Carta's restraints into revisable statute law. Solves the problem of binding executive power without entrenching constraints beyond parliamentary amendment. Provides a stable legal framework for governance while maintaining parliamentary control over the restraint boundary.
% TRANSFER_FUNCTION: Transfers authority over the interpretation and enforcement of fundamental restraints from a claimed historical/natural-law source (Magna Carta as intrinsic law) to Parliament as the sole revising authority. Parliament gains the power to narrow or eliminate any charter protection; unprotected minorities lose the shield of historical entrenchment and gain only parliamentary goodwill.
% ABSENT_VOICES: Jurists and theorists advocating for judicial entrenchment of Magna Carta (living constitutionalism reading) and those arguing the charter is historically obsolete (feudal obsolescence reading) are not parties to this framework — they would contest the reading's core premise that parliamentary statute is the sole carrier of restraint authority. Minorities who would prefer constitutional protections immune to parliamentary revision are structurally absent from the decision process.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if Parliament openly denied its binding authority and abandoned Magna Carta's inherited restraints — the world would rearrange sharply: judicial review and executive constraint would collapse; Crown prerogative would expand rapidly; minorities would lose legal recourse against majoritarian legislation; the common-law tradition's authority would be severed from its charter-inherited foundation. The constraint's disappearance would be a constitutional rupture.
% FOUNDING_PROBLEM: Magna Carta established restraints on Crown prerogative in 1215 and repeatedly thereafter (1217, 1225, etc.); by the early modern period, Parliament emerged as the supreme legislative authority. The founding problem for THIS reading is: how can Magna Carta's restraints survive in a system where Parliament, not custom or natural law, is sovereign? The solution: absorb the charter into statute, making restraints dependent on parliamentary will.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and legal scholars outside the benefiting parties (independent commentators, not parliamentary legislatures) attest that the absorption of Magna Carta into statute law is the actual working legal mechanism in England and jurisdictions inheriting the common law. The 1688 Bill of Rights, the Habeas Corpus Acts, and the Human Rights Act 1998 are cited as evidence of restraints-via-statute rather than inherent charter authority. Competing readings (living constitutionalism, feudal obsolescence) dissent, but the parliamentary absorption thesis is the dominant institutional practice.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness increases over the 800-year interval (0.22 to 0.58) as parliamentary power consolidates and as the charter's clauses are incrementally absorbed into statute. Early extractiveness is low because the charter's historical authority is still contested and powerful common lawyers defend its autonomous force. By interval end, extractiveness is moderate-high because Parliament's revisionary authority is firmly established and unprotected minorities face the full force of majoritarian revision: their protections are fragile, revisable at will, dependent on parliamentary goodwill. Theater ratio rises (0.15 to 0.42) as Parliament increasingly performs 'respecting the charter' through formal statutory restatement while actually controlling the boundary of restraint — the performative component increases as the real coordinating function (binding Crown prerogative) is hollowed by legislative sovereignty. Suppression is moderate throughout (0.28 to 0.47) because the constraint operates through law, not coercive force, but Parliament's power to suppress dissenting voices (through redefining the restraint boundary) is real and grows. The accessibility_collapse metric (0.72) reflects that once the parliamentary-sovereignty reading is institutionalized, alternatives (judicial entrenchment, feudal obsolescence) collapse as live positions within Westminster-tradition systems. Resistance (0.61) is substantial because common lawyers, judges, and minorities actively defend competing readings and the charter's historical authority; the parliamentary-sovereignty reading must be defended against these challenges, especially when Parliament moves to narrow protections.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat: this is a coordination mechanism that binds Crown prerogative while preserving parliamentary supremacy. The restraint is real, necessary for rule of law, and appropriately subject to democratic revision if society's values shift. Parliamentary control is a feature, not a bug. From the Crown's seat: this is a constraint that hedges prerogative power but operates through law that Parliament creates and can alter; the Crown must work within the statutory boundary but that boundary is not immovable. From unprotected minorities' seats: this is an extractive mechanism disguised as restraint. They would prefer Magna Carta to operate as natural law or judicial precedent — something Parliament cannot revise. The constraint's revisability is the mechanism of extraction: their protections can be voted away. The engine computes these divergent classifications (likely parliament-side rope or weak tangled-rope vs. minority-side snare) from the structural data — beneficiary vs. victim declarations, exit options differentiating the seats. The divergence is the analytical payoff; do not reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament as agenda_setter holds institutional power, long time horizon, and arbitrage-grade exit (can shift constitutional frameworks, exit by dissolving itself or fundamentally reorganizing authority). Its directionality is near-full beneficiary (d ≈ 0.1–0.2): the constraint apparatus amplifies parliamentary authority while constraining Crown. The Crown as constrained actor holds institutional power but zero time horizon for this specific constraint (bound by parliamentary law) and trapped exit (cannot exit the constraint without denying parliamentary sovereignty itself, which is constitutionally impossible in this reading's framework). Crown directionality is high-target (d ≈ 0.75–0.85): it pays the constraint and cannot exit. Unprotected minorities hold powerless status, biographical horizon, and trapped exit (no structural way to escape majoritarian legislation). Their directionality is full-target (d ≈ 0.9–0.95): they bear the risk of parliamentary revision without recourse. The absorptive mechanism (statute law) makes minorities' protection revisable in a way Magna Carta's claimed natural authority would not; the statutory carrier of restraints IS the mechanism of extraction. Common lawyers sit near symmetric (d ≈ 0.4–0.6): they benefit from mediating restraint interpretation but bear the burden of defending the charter's authority against parliamentary sovereignty claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents conflating coordination (restraint on Crown) with pure extraction (majoritarian control of the restraint boundary). A purely extractive reading would frame it as snare: Parliament extracts the power to revise Magna Carta, suppressing alternative constitutional readings, victimizing minorities. But the coordination function is real: Crown prerogative IS restrained, and that restraint protects people from arbitrary executive action. The tangled_rope classification captures the dual structure: coordination exists (the restraint) AND extractive asymmetry exists (Parliament controls the boundary and can revise at will). The mandate is 'restrain Crown prerogative' (live, not dead); the mechanism is 'parliamentary statute law' (the Parliamentary-Sovereignty reading's specific instantiation). The mandate persists; the mechanism may be contested by sibling readings (which would argue for different carriers of restraint — judicial precedent, or that the mandate is itself obsolete). Tangled_rope prevents misclassifying the constraint as pure rope (which would require symmetry in who controls the restraint boundary) or as pure snare (which would require the restraint to be theatrical rather than real). The extractiveness score (0.58) reflects that Parliament's revisionary power is neither negligible (like a rope) nor total (like a snare with no real coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the parliamentary-sovereignty reading''s core premise (statute law is the sole carrier of charter authority) logically foreclose the living-constitutionalism reading''s core premise (charter principles bind all rulers through judicial entrenchment), or do both readings remain live positions held by different institutional factions?',
    'A system that simultaneously holds (1) Parliament can repeal any Magna Carta restraint by statute and (2) courts will overturn parliamentary action that violates judicially entrenched charter principles instantiates a contradiction. The contradiction is real in systems attempting both (UK judicially entrenching Human Rights via the HRA 1998 while retaining Parliamentary supremacy doctrine). The resolution requires constitutional amendment or explicit institutional hierarchy declaration — which framework is supreme?',
    'If the readings foreclose each other, this constraint cannot coexist stably with the living-constitutionalism constraint in a single institutional framework; one reading must win and the other must be superseded or confined. If they coexist, then both are LIVE in the system despite tension, and classification diverges by seat. If they coexist-with or forecloses classification is wrong, the reading_relations field in cs_structure requires correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether parliamentary sovereignty and judicial entrenchment are logically compatible or mutually exclusive.').

omega_variable(
    minority_protection_revisability,
    'Is the extractiveness of this constraint (0.58) driven primarily by Parliament''s revisionary power over minority protections, or by Parliament''s structural institutional advantage in controlling the restraint boundary?',
    'Empirical: examine instances where Parliament has proposed repealing or narrowing Magna Carta-derived protections (habeas corpus, due process, fair trial). Measure minority resistance, legislative deliberation patterns, and success/failure of repeal attempts. If repeal attempts fail despite Parliamentary majority (e.g., because minorities organize political opposition or judicial resistance), extractiveness is overstated. If repeal attempts succeed when Parliament desires it (even if rare), the revisability mechanism is real and extractiveness is understated.',
    'If revisability is the primary extractive mechanism, the constraint is tangled-rope as classified. If minorities have political/judicial tools to resist repeal despite parliamentary statutory supremacy, the constraint may be rope rather than tangled-rope, and extractiveness should be lowered. If Parliament''s control is absolute and minorities have zero resistance capacity, extractiveness may be understated (constraint approaches snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_revisability, empirical, 'Whether parliamentary sovereignty over Magna Carta''s restraints is a live extractive mechanism or a theoretical threat with low realized prevalence.').

omega_variable(
    competing_readings_coexistence,
    'Are the three readings (parliamentary_sovereignty, living_constitutionalism, feudal_obsolescence) genuinely coexisting in the current institutional landscape, or has one reading become dominant and the others reduced to dissenting minority positions?',
    'Institutional survey: which reading is invoked by parliamentary drafters, courts, common lawyers, and constitutional theorists in contemporary practice? Which reading produces binding institutional outcomes? If all three remain live in different institutional seats (Parliament invokes parliamentary sovereignty; courts invoke living constitutionalism; marginal scholars invoke feudal obsolescence), they coexist. If one reading''s predictions match institutional outcomes and the others are rhetorical, the matching reading is dominant.',
    'If all three coexist, each reading''s omegas should address the coexistence tension. If one is dominant, the sibling readings'' reading_relations to this one may be misclassified (coexists_with suggests false parity if one is actually marginalized). The classification of reading_relations is provisional pending institutional evidence of coexistence vs. dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_readings_coexistence, empirical, 'Whether the three Magna Carta readings are currently live institutional positions or whether one has achieved dominance.').

omega_variable(
    absorption_narrative_authenticity,
    'Is the ''absorption into statute law'' narrative (Magna Carta''s restraints persist only as parliamentary statute) a genuine institutional mechanism, or a historical myth that Westminster systems have constructed to reconcile charter claims with parliamentary supremacy?',
    'Historical analysis: trace when and how Magna Carta clauses were explicitly written into English statute (1267 reissue, 1300 confirmation, Habeas Corpus Acts 1679/1688, etc.). Examine whether Parliament explicitly treated these acts as ''carrying forward'' Magna Carta or as fresh legislation that happened to overlap with the charter. If ''absorption'' language appears contemporaneously in parliamentary debate and legal reasoning, it is authentic institutional practice. If absorption language appears only in later historiography, it may be a post-hoc narrative.',
    'If absorption is authentic institutional understanding, this reading''s framing is grounded in real constitutional reasoning. If absorption is mythic (a story Westminster systems tell about themselves), the constraint is closer to snare: it uses the charter''s prestige while actually embodying parliamentary sovereignty, and the absorption narrative is the theatrical component masking the extraction. Theater_ratio should be raised if the narrative is primarily mythic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absorption_narrative_authenticity, empirical, 'Whether ''absorption into statute'' is an authentic constitutional mechanism or a legitimacy narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(magn_tr_t200, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(magn_tr_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement(magn_tr_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 600, 0.39).
narrative_ontology:measurement(magn_tr_t800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 800, 0.42).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(magn_be_t200, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(magn_be_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 400, 0.48).
narrative_ontology:measurement(magn_be_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(magn_be_t800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 800, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(magn_su_t200, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement(magn_su_t400, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 400, 0.43).
narrative_ontology:measurement(magn_su_t600, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(magn_su_t800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 800, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'magna_carta_constraint_authority'. Three structurally distinct constraints result from the same historical text interpreted under different institutional lenses: (1) parliamentary_sovereignty_reading (this file): restraints exist only via statute Parliament can revise; type=tangled_rope, ε≈0.58, beneficiary=Parliament, victim=unprotected_minorities. (2) living_constitutionalism_reading (sibling): restraints are judicially entrenched principles inherited across generations; type=mountain or weak_rope, ε≈0.05, beneficiary=none (natural law), victim=none (natural law). (3) feudal_obsolescence_reading (sibling): the charter is temporally dead, a 13th-century baronial compact with zero binding authority; type=piton, ε≈0.85 (pure extraction via theatrical legitimacy), beneficiary=tradition-wielding authorities, victim=minorities deceived into false protection. Each reading authors its own constraint_id, ε, and stakeholder situation. They are linked via network.affects_constraints and via omegas documenting the kernel contest. The readings do not represent 'perspectives' on one constraint — they ARE different constraints instantiated from the same kernel via different hermeneutical choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
