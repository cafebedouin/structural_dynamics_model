% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Magna Carta Authority via Parliamentary Sovereignty
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint embodies one reading of Magna Carta's enduring authority:
 *   the parliamentary sovereignty reading. Under this framing, the charter's
 *   original medieval restraints on Crown prerogative have been absorbed into
 *   English statute law over centuries. Parliament, not the historical
 *   charter text itself, is now the authoritative source and ultimate arbiter
 *   of those restraints. Parliament can revise, narrow, or repeal any charter
 *   provision through ordinary statute. This reading distinguishes itself
 *   from the living constitutionalism reading (which treats charter
 *   restraints as entrenched and binding across generations) and from the
 *   feudal obsolescence reading (which treats Magna Carta as a historical
 *   document with no binding force on modern sovereignty). The parliamentary
 *   sovereignty reading is the dominant institutional framing in UK
 *   constitutional practice: Parliament is supreme, and charter principles
 *   survive only as Parliament maintains or permits them through statute.
 *
 * KEY AGENTS:
 *   - parliamentary_legislature: institutional agenda-setter with generational time horizon; controls revisionary authority over all charter provisions
 *   - general_property_holding_classes: powerful beneficiaries with arbitrage exit; historically shaped Parliament's absorption of charter authority
 *   - crown_executive: institutional payer trapped by statute law; operates under inherited restraints but cannot revise them unilaterally
 *   - excluded_minorities: powerless payers trapped with no legislative voice; experience constraint as mediated through majoritarian statute
 *   - non_propertied_subjects: powerless payers trapped; historically excluded from charter benefit structures
 *   - juridical_interpreters: institutional observers who apply charter principles through statutory interpretation but lack override authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.41).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Authority via Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'd45bb2e6-da5f-47a2-a23d-95c8ef2b58e6').
narrative_ontology:cs_kernel_codification('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', fixed_text).
narrative_ontology:cs_authority_grounding('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', extraction).
narrative_ontology:cs_interpretation_layer_present('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6').
narrative_ontology:cs_reading_relation('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', foundational, parliamentary_supremacy_over_historical_texts).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_historical_texts, holdable).
narrative_ontology:cs_axiom_grounding('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', parliamentary_supremacy_over_historical_texts, conventional).
narrative_ontology:cs_axiom('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', foundational, charter_restraints_as_statutory_not_entrenched).
narrative_ontology:cs_axiom_status(charter_restraints_as_statutory_not_entrenched, holdable).
narrative_ontology:cs_axiom_grounding('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', charter_restraints_as_statutory_not_entrenched, deontological).
narrative_ontology:cs_reference_frame('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', parliamentary_legislative_supremacy).
narrative_ontology:cs_drift_state('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', contemporary_rights_entrenchment_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d45bb2e6-da5f-47a2-a23d-95c8ef2b58e6', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, general_property_holding_classes).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, excluded_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, non_propertied_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the agenda by defining which charter restraints remain law through statute and which are revised, repealed, or narrowed. Parliament administers the constraint through ordinary legislation and can alter it at will. The legislative body benefits from the legitimacy and authority the inherited charter framework provides — it can claim to represent the people's will while protecting established law and order — while retaining the power to change any provision.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the charter's inherited restraints on arbitrary Crown action and contract violation. Historically, this class dominated Parliament through property qualifications on suffrage and legislative membership, ensuring that charter restraints protected their interests — predictable property law, enforceable contracts, and protection from capricious taxation. They retain the political power to influence Parliament's revision decisions and can exit constraint vulnerability by building legislative coalitions that protect their interests.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, general_property_holding_classes, beneficiary,
    powerful, generational, arbitrage, national).

% Must operate within the framework of statutory law that Parliament maintains as the embodiment of charter restraints. The Crown cannot unilaterally revise these constraints; it cannot claim prerogative exemption from statute. However, the Crown is not a unified individual — it is the state apparatus itself, and it cannot exit the realm. The Crown's restraint is permanent and non-negotiable at any given moment, though Parliament can revise it over time. This is genuine restraint but ultimately revisable by the legislature.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive, payer,
    institutional, generational, trapped, national).

% Excluded from effective parliamentary representation or hold minority status within the legislature. They may nominally benefit from rule of law against arbitrary Crown action, but the real substance of charter protections is determined by Parliament — a body they cannot effectively influence. Parliament, controlled by propertied majorities with different interests, can legislate to narrow or eliminate protections for minorities without constitutional check. They bear the cost of majoritarian revisionary power and have no exit: they cannot leave the jurisdiction or change their identity status as minorities.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, excluded_minorities, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, excluded_minorities, observer).

% Historically excluded from the property-owning class that shaped Parliament's absorption and interpretation of charter authority. They experience the constraint entirely as mediated through legislative acts designed to protect property, contract, and commercial order — social classes to which they do not belong. They have minimal or zero influence over Parliament's exercise of revisionary power and cannot exit the national jurisdiction. They are trapped subjects of a legislature that controls their nominal protections.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, non_propertied_subjects, payer,
    powerless, biographical, trapped, national).

% The executive machinery that historically operated under feudal prerogative privilege is now structurally excluded from operating outside statute law. This exclusion is maintained entirely by Parliament's choice to enforce it through statute. The apparatus would benefit from reduced restraint and expanded prerogative power, but it has no voice in the constraint's design — it is excluded from the conversation about whether restraints should persist or be narrowed. The apparatus can only accept Parliament's decisions.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_prerogative_apparatus, excluded,
    institutional, generational, trapped, national).

% Judges and legal scholars who interpret inherited charter principles as they appear in statutory law and apply them in adjudication. They serve as analytical observers reading the constraint's operation, but they lack ultimate authority — Parliament is supreme, and if a statute contradicts charter principles, judges apply the statute. They can articulate that charter principles bind, but they cannot override Parliament. Their role is to apply Parliament's will, not to constrain it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, juridical_interpreters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_legislature).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative legal framework (statute law incorporating charter principles) in place of competing claims to legitimate restraint authority (feudal custom, royal prerogative, common law tradition). This solves the coordination problem of multiple incompatible authority claims by placing Parliament at the apex as the ultimate source and arbiter of restraint legitimacy.
% TRANSFER_FUNCTION: Transfers the power to define and revise Magna Carta restraints from the historical charter text (treated as fixed in other readings) to Parliament. Grants the legislature the authority to adjust restraint scope through ordinary statute, moving decisional power from a text to a revisable body controlled by shifting political majorities.
% ABSENT_VOICES: Excluded minorities and non-propertied subjects lack effective parliamentary representation and cannot voice objection to legislative narrowing of charter protections. Rival authority claims — feudal legitimacy, judicial entrenchment of charter principles, popular constitutionalism, living constitutional theory — are absent from this reading's institutional framework. Those who would assert that charter restraints should be judicially enforced against Parliament or constitutionally entrenched are structurally excluded from this constraint's definition.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over charter restraints vanished overnight, the Crown would immediately assert prerogative powers absent statute law restraint, property holdings would lose predictable legal protection, and contract enforcement would depend on executive will. Within weeks, alternative authority structures would emerge or Parliament would reassert control — the arrangement is fundamental to modern governance. The world would rearrange into chaos or alternative governance structures, not remain unchanged.
% FOUNDING_PROBLEM: Magna Carta in 1215 addressed baronial restraints on Crown prerogative. By the early modern period (16th–17th centuries), the problem evolved: feudal custom was weakening, royal prerogative was expanding, and authority claims from different sources (monarchy, Parliament, common law) were in conflict. The solution was to absorb charter restraints into parliamentary statute law, making Parliament the ultimate source of restraint authority and eliminating competing authority claims.
% FOUNDING_PROBLEM_CORROBORATION: Parliament and the property-holding classes attest the founding problem remains live — Crown power requires legal restraint and Parliament is the legitimate source through statute. Juridical interpreters (judges and legal scholars) attest that charter principles are functionally embedded in statute and remain operative. However, living constitutionalism scholars, reform movements, and excluded minorities attest the problem Parliament solved was narrow and class-specific — protecting property and contract for elites, not establishing equal restraint on majoritarian power. The most significant corroboration from outside the benefiting parties comes from historical reform movements and judicial opinions claiming that statutory entrenchment is insufficient and charter protections should be judicially enforced or constitutionally entrenched against Parliament itself.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.52) because the constraint serves both genuine restraint on Crown power AND parliamentary majority control. From Parliament's position, the arrangement is legitimate authority inheritance; from excluded minorities' position, it is majoritarian revisionary power over their protections. Suppression is moderate-low (0.41) because the constraint does not require heavy enforcement apparatus — statute law enforcement is ordinary governance. Theater ratio is low-moderate (0.28) because the constraint involves real functional restraint on prerogative, but a growing historical record shows Parliament's willingness to legislate narrowing of charter protections suggests some performative maintenance of charter legitimacy alongside strategic revision. Accessibility collapse is moderate (0.62) — alternatives (feudal custom, prerogative privilege) are theoretically available but institutionally closed off by Parliament's statutory framework. Resistance is moderate-high (0.58) — reform movements regularly contest parliamentary revisionary power and claim charter restraints should be entrenched or enforced by courts independently. The measurement series shows extractiveness rising gradually over the interval (0.38 to 0.52) as Parliament's sovereign revisionary power becomes more explicitly theorized and used; theater also rises as Parliament must justify narrow statutes in charter language.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary legislature and propertied beneficiaries experience this constraint as legitimate authority (restraint that they control and can adjust). The Crown experiences it as binding but unilaterally unchangeable (genuine restraint). Excluded minorities experience it as arbitrary majoritarian power dressed in charter language (their protections can be legislatively repealed). Juridical interpreters experience it as subordinate to Parliament (they apply charter principles only insofar as Parliament permits through statute). The engine should compute different types across these seats: beneficiary/agenda-setter seats may compute as rope (coordination with revisionary authority); payer seats, especially powerless minorities, compute as snare (majority tyranny with an entrenchment illusion). The authored claim (tangled_rope) represents Parliament's own framing; the metrics support the payer-seat analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary legislature: d approaches 0.0 (full beneficiary) — controls the restraint, sets the rules, collects no direct rents but gains authority and legitimacy. General property-holding classes: d approaches 0.15 (weak beneficiary) — benefit from restraint on arbitrary Crown, have arbitrage exit through legislative coalition-building. Crown executive: d approaches 0.90 (strong target) — trapped by statute law, cannot unilaterally revise, bears the restraint absolutely. Excluded minorities and non-propertied subjects: d approaches 0.95 (full targets) — identity-locked (subject nationality), trapped exit, zero revisionary power over their own constraint experience. Juridical interpreters: d approaches 0.5 (symmetric) — apply the law but claim no authority over it. The directionality is highly asymmetric: powerless subjects experience nearly full extraction (their restraint protections are Parliament's to revise), while parliament experiences nearly full subsidy (authority and legitimacy). This asymmetry is the core structure the constraint encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would apply if the founding problem (restraint on medieval Crown prerogative) is dead but the constraint persists through institutional inertia. Under the parliamentary sovereignty reading, the founding problem status is contested: Parliament attests Crown prerogative still threatens and restraint remains necessary; judicial and reform traditions attest the problem evolved but the constraint persists; excluded minorities attest the real problem is now majoritarian tyranny, not Crown prerogative. The constraint does not appear to be pure mandatrophy (dead problem, theater only) — it serves the live function of legitimizing parliamentary authority. However, if living_constitutionalism or feudal_obsolescence readings gain institutional power, the parliamentary sovereignty reading could degrade into mandatrophy: a symbolic maintenance of charter language while actual restraint migrates to other sources (constitutional entrenchment, judicial common law, popular sovereignty).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_revisionary_legitimacy,
    'Does Parliament''s power to revise or repeal Magna Carta restraints constitute sovereign supremacy, or does it violate the idea that some rights are constitutionally entrenched and beyond legislative revision?',
    'Comparative constitutional analysis: jurisdictions with entrenched constitutional rights show different patterns of rights protection; a dispute over whether UK parliamentary sovereignty is a strength or a structural vulnerability to majority tyranny would resolve this.',
    'If restraint entrenchment is required, parliamentary sovereignty reading collapses toward legislative positivism and the constraint becomes snare (majority tyranny over minorities). If parliamentary supremacy is legitimate, the constraint holds as tangled_rope (restraint exists, but revisable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_revisionary_legitimacy, conceptual, 'Whether legislative supremacy is compatible with the idea of inalienable charter protections.').

omega_variable(
    victim_status_of_excluded_majorities,
    'Are excluded minorities and non-propertied subjects victims of this constraint, or are they unprotected parties whose only path to protection is legislative reform?',
    'Historical analysis of charter reforms: legislation driven by excluded groups mobilizing for protection (e.g. franchise expansion, due process extension) shows whether parliamentary absorption mechanism enables or prevents protection expansion.',
    'If excluded groups can use legislative process to claim charter protections, the constraint is tangled_rope with moderate extraction. If the legislative process is structurally closed to them, the constraint becomes snare with high extraction for those classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_excluded_majorities, empirical, 'Whether parliamentary mediation of charter restraints includes or excludes powerless populations.').

omega_variable(
    common_law_vs_statutory_entrenchment,
    'Are the charter''s restraints actually enforceable through common law judicial independence, or does Parliament''s legislative supremacy mean judges cannot override statute to protect charter principles?',
    'Case law analysis: do courts enforce charter-based restraints against Parliament itself, or only apply Parliament''s statutes? Historical instances where courts claimed charter-based supremacy vs. instances of judicial deference to legislative revision.',
    'If judges can enforce charter restraints independently, the constraint is hybrid (not purely parliamentary). If judges defer to Parliament always, the constraint is purely legislative and vulnerable to majority revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(common_law_vs_statutory_entrenchment, empirical, 'Whether judicial enforcement of charter principles operates independently or defers to parliamentary sovereignty.').

omega_variable(
    charter_kernel_vs_sibling_readings,
    'Is Magna Carta understood primarily as a historical feudal document (feudal_obsolescence reading), a living constitutional principle (living_constitutionalism reading), or a statutory framework Parliament controls (parliamentary_sovereignty reading)? Which reading legitimately inherits the kernel?',
    'Jurisprudential authority: which reading appears in judicial opinions, parliamentary acts, and constitutional treatises as the authoritative interpretation? Which reading has accumulated institutional power to define the constraint?',
    'If living_constitutionalism reading gains authority, restraints become harder to revise (higher entrenchment, lower extractiveness). If feudal_obsolescence reading gains authority, restraints lose legitimacy (higher extraction of authority itself). Parliamentary_sovereignty reading assumes parliamentary authority is the legitimate inheritance mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_kernel_vs_sibling_readings, conceptual, 'Which sibling reading''s framing of charter legitimacy becomes institutionally dominant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(magn_tr_t0, projected).
narrative_ontology:measurement(magn_tr_t7, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement_basis(magn_tr_t7, observed).
narrative_ontology:measurement(magn_tr_t14, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement_basis(magn_tr_t14, observed).
narrative_ontology:measurement(magn_tr_t21, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement_basis(magn_tr_t21, observed).
narrative_ontology:measurement(magn_tr_t28, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 28, 0.27).
narrative_ontology:measurement_basis(magn_tr_t28, observed).
narrative_ontology:measurement(magn_tr_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(magn_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(magn_be_t0, projected).
narrative_ontology:measurement(magn_be_t7, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement_basis(magn_be_t7, observed).
narrative_ontology:measurement(magn_be_t14, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement_basis(magn_be_t14, observed).
narrative_ontology:measurement(magn_be_t21, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 21, 0.5).
narrative_ontology:measurement_basis(magn_be_t21, observed).
narrative_ontology:measurement(magn_be_t28, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 28, 0.51).
narrative_ontology:measurement_basis(magn_be_t28, observed).
narrative_ontology:measurement(magn_be_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(magn_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(magn_su_t0, projected).
narrative_ontology:measurement(magn_su_t7, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 7, 0.37).
narrative_ontology:measurement_basis(magn_su_t7, observed).
narrative_ontology:measurement(magn_su_t14, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 14, 0.39).
narrative_ontology:measurement_basis(magn_su_t14, observed).
narrative_ontology:measurement(magn_su_t21, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 21, 0.4).
narrative_ontology:measurement_basis(magn_su_t21, observed).
narrative_ontology:measurement(magn_su_t28, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 28, 0.41).
narrative_ontology:measurement_basis(magn_su_t28, observed).
narrative_ontology:measurement(magn_su_t35, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(magn_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the Magna Carta kernel. The feudal_obsolescence_reading treats the charter as historically inert; the living_constitutionalism_reading treats it as entrenched and binding; the parliamentary_sovereignty_reading (this story) treats it as absorbed into statute and revisable by Parliament. Each reading has a different ε (legislative control over restraints increases extractiveness potential), different victim sets (Parliament's majorities vs. entrenchment guarantees vs. historical irrelevance), and different sibling relationships. All three are linked via network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
