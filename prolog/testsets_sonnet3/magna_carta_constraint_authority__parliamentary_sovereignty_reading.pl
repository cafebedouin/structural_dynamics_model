% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Magna Carta Restraint Absorbed into Parliamentary Statute (Parliamentary Sovereignty Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-sovereignty reading of the
 *   Magna Carta constraint-authority kernel: the charter's substantive
 *   protections survive only insofar as they have been absorbed into ordinary
 *   statute (habeas corpus acts, due process legislation, later human-rights
 *   framework acts), and Parliament — not any fixed text or judicial body —
 *   holds ultimate revisionary authority over all of it. This is a distinct
 *   constraint from the living-constitutionalism reading (which treats the
 *   charter as generating binding precedent no ruler can undo) and the
 *   feudal-obsolescence reading (which treats the charter as historically
 *   spent and irrelevant to modern sovereignty). Under this reading,
 *   extraction is moderate: the restraint genuinely functions to check Crown
 *   prerogative and coordinate the boundary of lawful state action, but it is
 *   structurally revisable by whoever controls a parliamentary majority,
 *   which creates an asymmetric extraction against minorities and future
 *   generations who cannot entrench their protections against majoritarian
 *   revision.
 *
 * KEY AGENTS:
 *   - parliament: administers and can revise the restraint (institutional/arbitrage)
 *   - parliamentary_majority_coalitions: primary beneficiary of revisionary control
 *   - unrepresented_minorities: bear the cost of unentrenched protection (powerless/trapped)
 *   - future_generations_bound_by_repealable_rights: inherit contingent, not fixed, liberties
 *   - the_crown: constrained party and incidental beneficiary of continued legitimacy
 *   - courts_and_judiciary: analytical/interpretive observer with no override power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.47).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.4).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraint Absorbed into Parliamentary Statute (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '5cfe1a6c-1da2-477f-a0cd-118c636abf14').
narrative_ontology:cs_kernel_codification('5cfe1a6c-1da2-477f-a0cd-118c636abf14', distributed).
narrative_ontology:cs_authority_grounding('5cfe1a6c-1da2-477f-a0cd-118c636abf14', practice).
narrative_ontology:cs_interpretation_layer_present('5cfe1a6c-1da2-477f-a0cd-118c636abf14').
narrative_ontology:cs_reading_relation('5cfe1a6c-1da2-477f-a0cd-118c636abf14', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cfe1a6c-1da2-477f-a0cd-118c636abf14', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_axiom('5cfe1a6c-1da2-477f-a0cd-118c636abf14', foundational, no_parliament_can_bind_its_successor).
narrative_ontology:cs_axiom_status(no_parliament_can_bind_its_successor, holdable).
narrative_ontology:cs_axiom_grounding('5cfe1a6c-1da2-477f-a0cd-118c636abf14', no_parliament_can_bind_its_successor, conventional).
narrative_ontology:cs_axiom('5cfe1a6c-1da2-477f-a0cd-118c636abf14', secondary, legislative_majority_is_sole_legitimate_rights_definer).
narrative_ontology:cs_axiom_status(legislative_majority_is_sole_legitimate_rights_definer, holdable).
narrative_ontology:cs_axiom_grounding('5cfe1a6c-1da2-477f-a0cd-118c636abf14', legislative_majority_is_sole_legitimate_rights_definer, conventional).
narrative_ontology:cs_reference_frame('5cfe1a6c-1da2-477f-a0cd-118c636abf14', post_glorious_revolution_parliamentary_settlement).
narrative_ontology:cs_drift_state('5cfe1a6c-1da2-477f-a0cd-118c636abf14', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5cfe1a6c-1da2-477f-a0cd-118c636abf14', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority_coalitions).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, electorally_enfranchised_citizens).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, electorally_enfranchised_citizens).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, no_entrenched_higher_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the sole authority to enact, amend, or repeal any statute descended from Magna Carta's charter clauses, including habeas corpus protections and due-process guarantees. No court or crown can bind a future Parliament; each successive Parliament can undo what its predecessor entrenched. It administers the restraint and can dissolve it by ordinary majority vote.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Whichever coalition commands a working majority effectively controls the content and durability of constitutional restraint; it can invoke Magna Carta's legacy rhetoric while legislating around or through its substance. Benefits from a system where popular electoral will, not fixed charter text, sets the limits of state power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Gain a restraint system responsive to democratic majorities rather than frozen baronial or judicial precedent — they can vote to change the rules that bind the state. But they also bear the cost when a shifting majority narrows protections that a fixed charter would have held constant; their security depends on remaining part of a winning coalition.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, electorally_enfranchised_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, electorally_enfranchised_citizens, payer).

% Groups without durable electoral leverage — historically religious dissenters, colonized populations, and today various minority constituencies — find that no charter-derived protection is secure against a determined majority in Parliament. Their due-process and liberty guarantees exist only until a statute revises them; they cannot appeal to any higher entrenched text.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities, payer,
    powerless, biographical, trapped, national).

% Inherit whatever version of Magna Carta's restraints the current Parliament has chosen to keep, amend, or discard, with no mechanism to bind future legislatures to today's protections. What looks like an ancient, stable liberty is in fact contingent on continuous legislative reaffirmation they had no part in securing.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights, payer,
    powerless, civilizational, trapped, national).

% The monarch's prerogative is formally constrained by statute descended from the charter, but that constraint is entirely Parliament's creature — the Crown pays the cost of restrained prerogative while also benefiting from continued constitutional legitimacy and non-abolition as an institution, since Parliament has chosen accommodation over republicanism.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown, beneficiary).

% Interpret and apply statutes descended from Magna Carta but cannot strike down an Act of Parliament as unconstitutional under this reading; they can only construe legislative intent, leaving ultimate revisionary authority with the legislature. Their role is adjudicative, not constraint-generating.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, courts_and_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority_coalitions).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, continuously updatable mechanism — statute law passed by Parliament — through which the ancient restraint on arbitrary state power is maintained, interpreted, and adapted to changing circumstances without requiring constitutional convention or judicial supremacy.
% TRANSFER_FUNCTION: Moves the authority to define the boundary of lawful state coercion from the Crown (originally) and from any fixed baronial charter (subsequently) into the hands of whichever parliamentary majority currently holds power, at the expense of durable guarantees for groups outside that majority.
% ABSENT_VOICES: Unrepresented minorities and future generations have no seat in the parliamentary process that revises their protections; they would argue for an entrenched, judicially enforceable floor of rights that no ordinary majority could touch, but no such floor exists under this reading.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over charter-descended restraints disappeared overnight — replaced by, say, a judicially enforceable entrenched constitution — the entire architecture of what counts as a 'right' in the UK constitutional tradition would shift: courts could strike down statutes, minorities would gain a forum insulated from majoritarian revision, and the Crown's prerogative would be measured against fixed text rather than shifting legislative will.
% FOUNDING_PROBLEM: Magna Carta was built to solve the problem of unconstrained royal prerogative — arbitrary imprisonment, arbitrary taxation, and denial of judgment by peers — by forcing the Crown to accept fixed limits on its power.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative law scholars outside the parliamentary tradition (e.g., scholars of entrenched-constitution systems) attest that the original founding problem — protection against arbitrary state power — is only partially solved under parliamentary sovereignty, since the mechanism protecting citizens is the same body capable of removing that protection; parliamentary insiders and legal positivists corroborate that the doctrine of sovereignty itself is a settled, functioning solution, but this corroboration comes from within the benefiting institutional tradition rather than from an independent check.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.47) is set at a moderate level because the coordination function is genuine and historically significant — real limits on arbitrary state power do operate through this mechanism — but the same mechanism that creates the restraint also permits its erosion or repeal without recourse, which is a structural extraction against parties who cannot participate effectively in revision. Suppression (0.4) reflects that the mechanism operates mostly through ordinary legislative and electoral processes rather than raw coercion, though the historical suppression of dissenting factions (religious minorities, colonial subjects) during statute formation was higher and is reflected in the declining-suppression trajectory across the measurement series. Theater ratio rises modestly over time (0.1 to 0.3) as invocation of Magna Carta's symbolic authority in political rhetoric increasingly outpaces its operative content in current statute.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and current majority coalitions sit near the beneficiary end: they hold and exercise the revisionary power. Enfranchised citizens sit close to symmetric — real benefit from adaptability, real cost from insecurity. Unrepresented minorities and future generations sit near the full-target end: they bear the structural cost of a restraint that offers no protection against a hostile majority, with no institutional standing to prevent revision. The Crown carries a dual position — constrained (payer of prerogative limitation) but also beneficiary (survival of the institution under an accommodating rather than abolitionist settlement).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal prerogative) has been substantially but not completely resolved: modern parliamentary systems no longer face literal baronial rebellion, but the underlying problem of concentrated, unaccountable power revising the rules that bind it persists in a different institutional form (a majoritarian legislature rather than a monarch). Classifying this as tangled_rope rather than mountain or rope prevents two mislabeling errors: treating parliamentary sovereignty as pure extraction (ignoring its real coordination function protecting most citizens most of the time) or treating it as costless coordination (ignoring the structural vulnerability of unrepresented groups). The founding_problem_status is authored as contested precisely because parliamentary insiders see the mechanism as a living solution while independent minority-rights advocates see it as an unresolved vulnerability wearing historical legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_entrenchment_framing,
    'Is the parliamentary-sovereignty reading itself a stable structural fact, or does it depend on the continued absence of a codified, judicially enforceable constitution — meaning the reading could be foreclosed by future constitutional entrenchment (e.g., an entrenched Human Rights Act or written constitution)?',
    'Track whether any UK constitutional reform entrenches rights against ordinary legislative repeal (e.g., requiring supermajority or referendum for repeal) — such a change would structurally convert this reading toward the living-constitutionalism reading.',
    'If entrenchment occurs, the parliamentary-sovereignty reading''s core premise (unlimited revisionary authority) would be falsified for the entrenched provisions, and this constraint would need to be re-authored or narrowed to non-entrenched statute only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_entrenchment_framing, conceptual, 'Whether parliamentary sovereignty''s structural stability depends on the continued absence of constitutional entrenchment.').

omega_variable(
    minority_protection_gap_measurement,
    'How much of the actual historical protection experienced by minorities under charter-descended statute is attributable to genuine constitutional restraint versus contingent political majorities that happened not to target them?',
    'Comparative historical analysis of episodes where parliamentary majorities did legislate against minority protections (e.g., historical religious test acts, immigration restrictions) versus episodes of legislative protection, to determine the base rate of majoritarian override.',
    'A high base rate of override would support classifying the victim-side extraction as severe and structural rather than incidental; a low base rate would support treating the current moderate extractiveness score as durable rather than provisional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_gap_measurement, empirical, 'How reliable majoritarian restraint has actually been for minority protection historically.').

omega_variable(
    cross_reading_framing_choice,
    'Was the choice to author this as the parliamentary-sovereignty reading (rather than living-constitutionalism) itself guided by a defensible structural signal, or could the same historical record equally support the living-constitutionalism framing?',
    'Compare judicial doctrine across common-law jurisdictions: UK courts explicitly affirm parliamentary sovereignty as the operative doctrine (contrast with jurisdictions like the US or post-1982 Canada, where courts can strike down legislation against entrenched charters) — this is the signal used to select this reading.',
    'If UK doctrine shifted toward judicial supremacy over statute (as some argue is happening incrementally via human rights jurisprudence and EU-derived law before Brexit), the classification could shift substantially toward the living-constitutionalism reading, changing extractiveness and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_framing_choice, conceptual, 'Whether the parliamentary-sovereignty framing choice is robust against an emerging judicial-supremacy trend.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.15).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1832, 0.2).
narrative_ontology:measurement(magn_tr_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1911, 0.24).
narrative_ontology:measurement(magn_tr_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1972, 0.27).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.3).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1832, 0.35).
narrative_ontology:measurement(magn_be_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1911, 0.4).
narrative_ontology:measurement(magn_be_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1972, 0.44).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1215, 0.55).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.5).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1832, 0.45).
narrative_ontology:measurement(magn_su_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1911, 0.42).
narrative_ontology:measurement(magn_su_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1972, 0.4).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the binding authority of Magna Carta' per the epsilon-invariance principle. The three readings (parliamentary_sovereignty_reading, living_constitutionalism_reading, feudal_obsolescence_reading) share a kernel (magna_carta_constraint_authority) but instantiate structurally distinct constraints with different epsilon values, victim sets, and classifications: parliamentary sovereignty (this file) reads as tangled_rope with moderate extraction and a minority/future-generations victim set; living constitutionalism would read as more rope-like with near-zero extraction under its own premises (the restraint is treated as durably binding, so no revisionary extraction exists); feudal obsolescence would read as near-mountain-null or simply denies the constraint has modern force at all. All three files must link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__parliamentary_sovereignty_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
