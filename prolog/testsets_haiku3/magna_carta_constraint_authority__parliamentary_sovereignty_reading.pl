% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_parliamentary_sovereignty, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Over Magna Carta Restraints
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This reading instantiates parliamentary sovereignty over Magna Carta: the
 *   charter's restraints survive only insofar as Parliament enacts them into
 *   statute law, and Parliament retains full power to revise, repeal, or
 *   suspend any provision by ordinary legislation. The Crown's prerogative is
 *   constrained by inherited charter principles now embodied in statute, but
 *   the constraint's persistence depends entirely on Parliament's ongoing
 *   choice to maintain it. This reading transforms Magna Carta from a binding
 *   external check on sovereign power into a parliamentary power
 *   resource—Parliament inherits the authority to decide which restraints
 *   bind the Crown and which do not. Minorities and subjects outside
 *   parliamentary coalitions become structurally vulnerable: their
 *   protections exist only as long as the legislature chooses to maintain
 *   them, with no entrenched constitutional bar to majoritarian suspension.
 *   The constraint is tangled_rope because it coordinates genuine restraint
 *   on prerogative (that function is real) while simultaneously enabling
 *   legislators to extract the revisionary power—control over whether
 *   restraints apply—from minorities with no counter-voice.
 *
 * KEY AGENTS:
 *   - Parliamentary Legislature: Agenda-setter. Holds interpretive and revisionary authority over all charter restraints; can maintain, modify, or repeal any protection by statute.
 *   - Protected Minorities: Payer seat (powerless, trapped, biographical). Depend on inherited restraints but have no structural protection against majoritarian legislative revision.
 *   - Crown Prerogative: Institutional payer (constrained by statute). Bounded by parliamentary-enacted restraints but benefits from any parliamentary revision that loosens them.
 *   - Governing Coalition: Organized beneficiary. Gains flexibility to override charter protections for coalition priorities through legislative action.
 *   - Common Law Courts: Observer seat (institutional). Enforce and interpret parliamentary statute embodying charter language but do not author the constraint.
 *   - Living Constitutionalist Excluded: Would argue the courts or inherited principles should constrain Parliament itself; structurally excluded from this reading's framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Over Magna Carta Restraints").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '17887583-3de4-4f1f-ad53-f9240708fd30').
narrative_ontology:cs_kernel_codification('17887583-3de4-4f1f-ad53-f9240708fd30', formalized).
narrative_ontology:cs_authority_grounding('17887583-3de4-4f1f-ad53-f9240708fd30', extraction).
narrative_ontology:cs_interpretation_layer_present('17887583-3de4-4f1f-ad53-f9240708fd30').
narrative_ontology:cs_reading_relation('17887583-3de4-4f1f-ad53-f9240708fd30', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_reading_relation('17887583-3de4-4f1f-ad53-f9240708fd30', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('17887583-3de4-4f1f-ad53-f9240708fd30', foundational, parliament_holds_ultimate_revisionary_authority).
narrative_ontology:cs_axiom_status(parliament_holds_ultimate_revisionary_authority, holdable).
narrative_ontology:cs_axiom_grounding('17887583-3de4-4f1f-ad53-f9240708fd30', parliament_holds_ultimate_revisionary_authority, conventional).
narrative_ontology:cs_axiom('17887583-3de4-4f1f-ad53-f9240708fd30', foundational, inherited_charter_constrains_prerogative_through_statute_only).
narrative_ontology:cs_axiom_status(inherited_charter_constrains_prerogative_through_statute_only, holdable).
narrative_ontology:cs_axiom_grounding('17887583-3de4-4f1f-ad53-f9240708fd30', inherited_charter_constrains_prerogative_through_statute_only, conventional).
narrative_ontology:cs_reference_frame('17887583-3de4-4f1f-ad53-f9240708fd30', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('17887583-3de4-4f1f-ad53-f9240708fd30', contemporary_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17887583-3de4-4f1f-ad53-f9240708fd30', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_coalition).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, protected_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extra_parliamentary_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extra_parliamentary_subjects).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_prerogative).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legislative_positivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and maintains the statutory framework embodying charter restraints. Controls which inherited protections are codified into law and which are allowed to lapse or be overridden. Acts through legislative procedure, claiming legitimacy from both inherited charter authority and democratic mandate.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Depend on parliamentary statutes embodying charter protections—due process, legal procedure, protection against arbitrary seizure. They cannot exit the jurisdiction, cannot influence legislative majorities, and have no independent veto over statutory revision. Their security is conditioned on ongoing parliamentary choice to maintain restraints.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, protected_minorities, payer,
    powerless, biographical, trapped, national).

% Crown's executive authority is constrained by statutes embodying charter restraints—cannot seize property, arrest without lawful cause, or impose taxation without consent, insofar as Parliament maintains the statutes that restrain prerogative. Crown benefits or is burdened by parliamentary revision of these statutes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_prerogative, payer,
    institutional, generational, constrained, national).

% Benefits from legislative control over charter restraints: can adjust protections to serve coalition priorities, override inherited restraints when they obstruct coalition goals through statutory override, and use parliamentary procedures to reshape legal framework while maintaining legitimacy through inherited language.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, governing_coalition, beneficiary,
    organized, biographical, mobile, national).

% Interpret and apply parliamentary statutes and invoke Magna Carta language in specific disputes. Do not author the constraint but enforce it. Maintain the performance that they discover charter principles through case law while actually enforcing parliamentary-enacted statute law.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_courts, observer,
    institutional, generational, analytical, national).

% Bound by parliamentary statute whether or not they participate in legislative deliberation. Receive procedural protections when Parliament chooses to maintain them; bear costs of legal instability when Parliament revises restraints. Most benefit from stable inherited law; many depend on continued parliamentary choice to protect them.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extra_parliamentary_subjects, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extra_parliamentary_subjects, beneficiary).

% Would argue courts or inherited principles should constrain Parliament itself—that charter restraints bind all, including the legislature. Their voice is excluded from the parliamentary_sovereignty_reading's framework because the reading's core premise is that Parliament holds ultimate revisionary authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, living_constitutionalist_advocates, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal apparatus for restraining executive prerogative through inherited charter language: by codifying restraints in statute, the arrangement solves the commitment problem of binding subsequent rulers and successive legislatures to inherited procedural limits.
% TRANSFER_FUNCTION: Transfers revisionary authority over inherited restraints from a feudal-baronial framework to the parliamentary legislature. The legislature collects the power to maintain, modify, or repeal charter protections; protected minorities and subjects outside parliamentary coalitions surrender unconditional restraint security and become dependent on ongoing majority forbearance.
% ABSENT_VOICES: Living constitutionalist jurists (would argue courts should enforce charter principles against Parliament); extra-parliamentary populations unrepresented in legislative deliberation; Crown interests preferring direct prerogative; feudal aristocrats whose settlement the reading displaces. In contemporary democracies, systematic minorities subject to majoritarian override without structural protection.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over charter restraints vanished—if courts could strike down legislation violating inherited principles, or if charter language reverted to binding status regardless of parliamentary revision—the entire apparatus of parliamentary statute law would be reinterpreted against a new supremacy locus. Either courts would hold veto power (judicial supremacy), or Parliament would be formally constrained by unamendable constitution, hardening the legal system. Legislative flexibility would be structurally eliminated.
% FOUNDING_PROBLEM: Magna Carta addressed 13th-century feudal extraction: baronial grievances about arbitrary royal seizure, taxation without consent, and absence of legal procedure. The charter solved the coordination problem of binding even the King to procedural restraint.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional scholars agree the feudal extraction problem is structurally obsolete: feudal property relations no longer dominate; the Crown's need for extraordinary taxation from landed barons no longer drives governance; baronial assembly evolved into parliamentary representation. Modern constitutional scholars corroborate that the founding problem is dead while the apparatus persists—by choice of Parliament (legislative tradition, precedent value, legitimacy performance) rather than by binding force. The corroboration is external to benefiting parties: historians, comparative constitutionalists, and judicial theorists all agree feudalism is gone and the charter's original binding force is not what maintains the restraint today.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.58 at interval end) because the legislative revisionary power over restraints functions as a resource the legislature collectively extracts—minorities cannot exit and depend on majority forbearance. The measurement series show a rising trajectory from 0.38 to 0.58 over the interval: early in the period (feudal to early-modern transition), the inherited charter language retained more binding force and parliamentary discretion was constrained by deference to precedent; over time, as parliamentary supremacy doctrine crystallized, the legislature's perceived revisionary power increased, extractiveness rose. Theater ratio rises from 0.18 to 0.31 (then plateaus): legislators increasingly invoke Magna Carta language to legitimize their own acts while simultaneously claiming sovereign power to override it—the rhetoric performs continuity with inherited restraint while the practice centers on legislative flexibility. Suppression requirement rises to 0.42 and plateaus: maintaining parliamentary sovereignty over minorities requires active enforcement (legislation must be passed to override inherited protections; resistance must be overcome when courts or minorities appeal to charter language), but suppression is not maximal because the system operates through procedural legality—no secret police are needed, only statutory override.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary agenda-setter's seat, the constraint is a coordination mechanism (restraint on prerogative is genuine) that Parliament legitimately controls through statute; from the powerless-minority payer seat, the same constraint is extraction—they are held to inherited restraints that could be revoked at any moment by a legislative majority they cannot influence. The engine computes this divergence from power differentials (institutional vs. powerless), exit options (arbitrage vs. trapped), and the beneficiary/victim declarations. The payer seat experiences the constraint as extractive threat; the agenda-setter seat experiences it as coordinated flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament's directionality is near the beneficiary end (low d): it collects revisionary authority and benefits from the flexibility to override restraints. Minorities' directionality is near the target end (high d): they depend on the constraint for protection but have no veto over its revision—trapped, powerless, without exit options or alternative forums. The Crown's directionality is moderate: it is constrained by statute but not victimized (the Crown is an institutional actor with its own power resources and can benefit from or adapt to statutory change). This is a textbook tangled_rope asymmetry: coordination function (restraining prerogative, solving the commitment problem) paired with extractive asymmetry (legislative control over whether minorities remain protected).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (feudal baronial extraction) is definitively dead, yet the constraint persists and has accumulated new functions. This creates a mandatrophy candidate: the charter language persists as inherited authority, but the substantive restraint problem it solved no longer exists. However, the constraint has not become purely theatrical—parliamentary statutes genuinely limit prerogative power, and minorities depend on statutory protections. The theater ratio shows this: it rises but plateaus below 0.35, indicating persistent functional content (genuine restraint, not pure performance). The classification remains tangled_rope rather than piton because the legislature still derives real benefit from maintaining the apparatus (it provides legitimacy and continuity for parliamentary action, dressing new legislation in inherited language) and minorities still depend on it for protection—neither seat is indifferent. Mandatrophy is partial: the founding problem is dead, but the structural relationships are active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence_and_extraction_persistence,
    'If Magna Carta''s founding problem (feudal baronial extraction) is dead, why does the parliamentary legislature continue to maintain the inherited restraint apparatus rather than sweeping it away and replacing it with purely utilitarian statute?',
    'Historical analysis of legislative debates and constitutional theory arguments: is the maintenance of charter language (a) performative legitimacy (theater), (b) genuine path-dependence (high switching cost to pure utilitarian rewriting), or (c) legislative recognition that inherited restraints provide real constraint value even after the original problem obsolesces?',
    'If (a), the constraint is closer to piton than the metrics suggest. If (b) or (c), the constraint is genuinely tangled_rope—legislatures value the restraint apparatus even while claiming power to override it, indicating real coordination benefit. The classification depends on whether charter maintenance is constraint or artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_and_extraction_persistence, empirical, 'Why inherited charter restraints persist after their original binding problem is dead.').

omega_variable(
    living_constitutionalism_foreclosure_question,
    'Does the parliamentary_sovereignty_reading foreclose the living_constitutionalism_reading, or do they coexist as competing claims held by different institutional seats?',
    'Examine whether a single coherent legal framework can hold both: parliament has revisionary power AND courts can strike down legislation violating inherited principles. In Anglo-American constitutionalism, courts invoke parliamentary supremacy while defending constitutional limits (the apparent logical contradiction is real; frameworks coexist through institutional practice rather than unified theory).',
    'If forecloses: this reading''s core premise (Parliament is the ultimate authority) directly contradicts the living reading''s core premise (inherited principles bind all, including Parliament). If coexists_with: the readings occupy different institutional seats (legislatures claim supremacy; courts claim constitutional constraint) and maintain incoherence through divided sovereignty. Affects the engine''s reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_constitutionalism_foreclosure_question, conceptual, 'Logical relationship between parliamentary sovereignty and living constitutionalism.').

omega_variable(
    minority_protection_dependence_mechanism,
    'What sustains parliamentary forbearance in maintaining restraints on the majority''s power to suppress minorities—is it institutional norm, democratic reciprocity (today''s minority is tomorrow''s majority), or is there no deep mechanism and suppression is contingent on current distribution of power?',
    'Comparative constitutional study of democracies with parliamentary supremacy: where minorities have been unprotected (colonial India pre-constitutional entrenchment, Weimar Germany, contemporary example jurisdictions), what explains the absence or removal of constraints? Norm failure, rational calculation shift, or deliberate majoritarian choice?',
    'If institutional norm is deep, suppression may be lower than the metrics suggest and extractiveness may be overstated. If contingent, the metrics understate vulnerability of minorities to majoritarian override. High-minority-protection democracies tend to adopt entrenched constitutions precisely to prevent parliamentary supremacy over minority rights—the constraint may be inherently high-extractiveness under this reading because it leaves minorities unprotected by structural law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_dependence_mechanism, empirical, 'What mechanisms sustain minority protections in parliamentary-sovereignty systems.').

omega_variable(
    kernel_reading_distinction_from_sibling_readings,
    'What structural evidence distinguishes this parliamentary-sovereignty reading from the feudal-obsolescence and living-constitutionalism readings?',
    'The readings are distinguished by their answers to: (1) Who holds the authority to interpret/revise Magna Carta? Parliament (this reading) vs. the Crown through feudal precedent (feudal_obsolescence) vs. the courts through inherited principles (living_constitutionalism). (2) Can the inherited restraint be overridden? Parliament can override any provision by statute (this reading) vs. the constraint is dead and replaced by modern statute (feudal_obsolescence) vs. the constraint binds all rulers including Parliament (living_constitutionalism). These are not empirical questions to be resolved by data—they are distinct normative commitments about which authority grounds constitutional legitimacy.',
    'This omega documents the irreducibly conceptual nature of the kernel dispute. No measurement can tell you which reading is ''correct''—each reading invokes a different grounding authority (parliamentary statute, feudal history, inherited judicial precedent). The readings coexist because they appeal to different legitimacy sources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction_from_sibling_readings, conceptual, 'The kernel''s reading dispute is grounded in alternative authority sources, not empirical disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(magn_tr_t10, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t30, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(magn_tr_t30, observed).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(magn_tr_t40, observed).
narrative_ontology:measurement(magn_tr_t50, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(magn_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(magn_be_t10, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t30, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(magn_be_t30, observed).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(magn_be_t40, observed).
narrative_ontology:measurement(magn_be_t50, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(magn_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t10, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(magn_su_t10, observed).
narrative_ontology:measurement(magn_su_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(magn_su_t20, observed).
narrative_ontology:measurement(magn_su_t30, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(magn_su_t30, observed).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(magn_su_t40, observed).
narrative_ontology:measurement(magn_su_t50, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(magn_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Magna Carta kernel: parliamentary_sovereignty_reading (this story), feudal_obsolescence_reading, and living_constitutionalism_reading. Each reading instantiates different ε values and stakeholder structures because they invoke different authority sources. Parliamentary sovereignty treats Magna Carta as absorbed into statute law and Parliament as the authority that revises it (moderate extraction, tangled_rope). Feudal obsolescence treats Magna Carta as a dead settlement with no binding force on modern sovereignty (low extraction, mountain-adjacent). Living constitutionalism treats Magna Carta as a binding inherited principle that constrains even Parliament (higher extraction for majorities, rope or scaffold). The readings are linked through their shared kernel; each is a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, powerless, 0.82).
constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
