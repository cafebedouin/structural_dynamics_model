% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta as Living Constitutional Restraint (Due Process Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the living-constitutionalism reading of the Magna
 *   Carta kernel: the claim that the charter's due-process and
 *   lawful-restraint principles bind all subsequent rulers through an
 *   unbroken chain of juridical precedent and evolutionary interpretation,
 *   rather than being a spent feudal artifact (the
 *   feudal_obsolescence_reading) or a delegated authority now fully absorbed
 *   and revisable by Parliament (the parliamentary_sovereignty_reading).
 *   Under this reading the coordination function is real and low-extraction:
 *   a durable, inherited check on arbitrary executive power that subjects and
 *   future claimants benefit from, at the structural cost of constraining
 *   royal prerogative and executive discretion. Extractiveness rises modestly
 *   over the interval as the doctrine accumulates procedural machinery
 *   (judicial review apparatus, precedent citation practices) that partly
 *   serves the interpreting judiciary's own institutional authority alongside
 *   the protective function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta as Living Constitutional Restraint (Due Process Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '51ef25be-8fed-4363-a844-fd154b784865').
narrative_ontology:cs_kernel_codification('51ef25be-8fed-4363-a844-fd154b784865', fixed_text).
narrative_ontology:cs_authority_grounding('51ef25be-8fed-4363-a844-fd154b784865', lineage).
narrative_ontology:cs_interpretation_layer_present('51ef25be-8fed-4363-a844-fd154b784865').
narrative_ontology:cs_reading_relation('51ef25be-8fed-4363-a844-fd154b784865', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('51ef25be-8fed-4363-a844-fd154b784865', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('51ef25be-8fed-4363-a844-fd154b784865', foundational, charter_generates_independent_binding_authority).
narrative_ontology:cs_axiom_status(charter_generates_independent_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('51ef25be-8fed-4363-a844-fd154b784865', charter_generates_independent_binding_authority, conventional).
narrative_ontology:cs_axiom('51ef25be-8fed-4363-a844-fd154b784865', foundational, due_process_principle_generalizes_across_eras_by_interpretation).
narrative_ontology:cs_axiom_status(due_process_principle_generalizes_across_eras_by_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('51ef25be-8fed-4363-a844-fd154b784865', due_process_principle_generalizes_across_eras_by_interpretation, instrumental).
narrative_ontology:cs_reference_frame('51ef25be-8fed-4363-a844-fd154b784865', id_1215_baronial_settlement_as_living_charter).
narrative_ontology:cs_drift_state('51ef25be-8fed-4363-a844-fd154b784865', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51ef25be-8fed-4363-a844-fd154b784865', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, future_constitutional_claimants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_office).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under whatever restraint the sovereign or executive accepts as binding. Under this reading, they inherit a due-process shield traceable through juridical precedent back to the 1215 charter and its reissues: no imprisonment or dispossession except by lawful judgment. They cannot personally invoke the charter directly in most cases, but courts and later statutes carry its logic forward on their behalf.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens, beneficiary,
    powerless, civilizational, trapped, national).

% Interprets and extends the charter's due-process language through precedent, treating it as a living authority that grows by evolutionary interpretation rather than a frozen 13th-century text. Judges decide which claims of executive overreach the charter's descendant doctrines will check, and thereby administer the constraint's practical scope.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Historically the Crown, now the residual executive prerogative power. Bound by the inherited principle that lawful judgment must precede deprivation of liberty or property; cannot simply issue arbitrary detention or seizure without running into due-process doctrine descended from the charter. Cannot exit the constraint without repudiating centuries of accepted constitutional legitimacy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_office, payer,
    institutional, civilizational, constrained, national).

% Modern government ministers and agencies whose discretionary powers are checked by courts invoking the due-process lineage. They experience the constraint as a recurring limit on emergency powers, detention policy, and executive action, adjudicated against them in judicial review.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_holders, payer,
    powerful, generational, constrained, national).

% Litigants and advocacy groups not yet born or not yet party to any dispute, who will invoke due-process precedent tracing to the charter in future cases. They benefit from the doctrine's continued vitality without having contributed to its construction.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, future_constitutional_claimants, beneficiary,
    powerless, civilizational, trapped, national).

% Study whether the living-constitutionalism reading accurately traces continuous doctrinal descent from 1215 or retrofits modern due process onto a baronial land-dispute settlement. Their scholarship feeds directly into the interpretive authority judges claim.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expectations across generations that no ruler or executive may act against a subject's life, liberty, or property except through lawful judgment and established process — a durable check against arbitrary power that all parties can plan around.
% TRANSFER_FUNCTION: Moves discretionary latitude away from the sovereign/executive and toward courts and rights-holders: the executive's capacity to act unilaterally is constrained, and the resulting protection accrues to subjects and future claimants.
% ABSENT_VOICES: The 1215 barons who actually negotiated the charter for their own feudal privileges are not present in this reading's account — their narrow, self-interested grievance has been reinterpreted centuries later as a universal due-process principle without their participation or consent to that reframing.
% DISAPPEARANCE_RATIONALE: Proponents of this reading argue that if the inherited due-process principle vanished, executive detention and seizure powers would expand immediately, since courts would lose the doctrinal lineage they cite in judicial review. Skeptics (feudal-obsolescence and parliamentary-sovereignty readers) argue the substantive protections are actually carried by modern statute and constitutional convention, so the disappearance of Magna Carta specifically would change citation practice but not outcomes.
% FOUNDING_PROBLEM: In 1215, barons sought to constrain King John's arbitrary taxation, seizure of property, and extra-judicial punishment of feudal tenants — a narrow problem of curbing one monarch's specific abuses of feudal lordship.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and legal scholars within the common-law tradition attest that the due-process problem remains live and that the charter's descendant doctrine actively checks executive power today. Legal historians outside the judiciary and outside constitutional advocacy note that the founding problem (13th-century baronial feudal grievance) was resolved or became moot centuries ago, and that the 'living' doctrine is a later constitutional tradition using the charter's authority as a legitimating ancestor rather than a continuously operating solution to its original problem.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at present) because the coordination function — binding restraint on arbitrary deprivation of liberty/property — is genuine and the beneficiary class (subjects, future claimants) is broad and diffuse rather than narrow and concentrated; the small rising trend reflects the judiciary's growing interpretive authority as an institutional actor with its own stake in maintaining the doctrine's vitality. Suppression is authored moderate-to-declining (0.5 to 0.35) reflecting that early enforcement of the charter against reluctant monarchs required real coercive confrontation (barons' war, repeated reissues under threat), while modern enforcement runs through accepted judicial review machinery with far less overt coercion. Theater ratio rises slowly as ceremonial invocation of the charter (anniversary commemorations, rhetorical citation) grows relative to its operative doctrinal content, but stays low overall because the due-process doctrine continues to do real work in live litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects and future claimants are declared beneficiaries because the due-process shield subsidizes their position against arbitrary state action — they did not construct the doctrine and bear none of its maintenance cost, so directionality sits near the beneficiary end. Royal prerogative and executive discretion holders are declared victims/payers because the constraint's entire operative content is a restraint on THEIR latitude — every successful due-process claim is a discretionary power they do not get to exercise; their exit options are constrained rather than trapped because they retain the formal capacity to contest specific applications in court, but cannot exit the doctrine itself without a constitutional rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is genealogical: was the 13th-century founding problem (curbing one king's feudal abuses) ever actually the same problem as the modern one (checking executive detention and emergency powers)? This reading answers yes via evolutionary interpretation — the underlying principle (no deprivation without lawful judgment) generalizes cleanly. The sibling feudal_obsolescence_reading answers no — the modern doctrine is a different arrangement wearing the charter's name for legitimacy. The living-constitutionalism reading resists being classified as pure snare/theater by pointing to continuously active litigation where the doctrine changes real outcomes (habeas corpus challenges, detention reviews) — this is not merely ceremonial invocation of a dead text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_vs_retroactive_construction,
    'Is the modern due-process doctrine genuinely continuous with the 1215 charter''s provisions, or is it a 17th-19th century retroactive construction that borrowed the charter''s prestige to legitimate principles developed independently?',
    'Detailed doctrinal history tracing citation chains: do courts from 1215 through the present cite the charter''s actual clauses in a continuous interpretive line, or does citation reappear discontinuously at moments (1628 Petition of Right, 17th century parliamentary struggles, 20th century civil liberties cases) when it is politically useful to invoke ancient authority for contemporary purposes?',
    'If continuous, the living-constitutionalism reading''s core premise holds and the rope classification with genuine coordination function is well-grounded. If discontinuous/reconstructed, the coordination story is largely a legitimating narrative laid over doctrine that would exist anyway, pushing the constraint toward tangled_rope or even piton (inherited symbolic authority with the real work done elsewhere).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_retroactive_construction, empirical, 'Whether the charter''s due-process lineage is continuous doctrine or retroactive legitimation.').

omega_variable(
    kernel_framing_committer_choice,
    'Given the three-reading kernel contest (feudal_obsolescence, living_constitutionalism, parliamentary_sovereignty), what signals guided selecting the living-constitutionalism framing for this story rather than treating the charter as fully absorbed into statute (parliamentary_sovereignty) or as historically inert (feudal_obsolescence)?',
    'Cross-reference judicial opinions that explicitly invoke Magna Carta as an independent source of authority (rather than merely as a historical predecessor to statute) — where courts treat the charter itself, not just its statutory descendants, as doing interpretive work, the living-constitutionalism framing has more purchase.',
    'If courts consistently frame due-process protections as flowing from statute and constitutional convention rather than the charter itself, the parliamentary_sovereignty_reading better describes actual practice and this reading''s claimed_type (rope with independent coordination function) would overstate the charter''s operative role relative to Parliament''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_committer_choice, conceptual, 'Documents the framing choice among three kernel readings and what evidence would shift it.').

omega_variable(
    beneficiary_diffuseness_ambiguity,
    'Are ''subjects_and_citizens'' and ''future_constitutional_claimants'' genuine beneficiaries of a coordination mechanism, or is the diffuse-beneficiary framing itself doing legitimating work for a judiciary that gains institutional authority from being the doctrine''s interpreter?',
    'Compare judicial self-citation patterns and institutional budget/authority growth in judicial review capacity against measurable improvements in due-process outcomes for ordinary claimants over the same period.',
    'If judicial institutional authority has grown disproportionately to measurable subject-level due-process improvement, the true beneficiary is closer to the judiciary itself, which would push the classification toward tangled_rope (coordination cover for institutional self-interest) rather than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_diffuseness_ambiguity, empirical, 'Whether beneficiary diffuseness masks judiciary-concentrated institutional gain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.12).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(magn_tr_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1400, 0.12).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.15).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(magn_be_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.5).
narrative_ontology:measurement(magn_su_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1400, 0.42).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.4).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement(magn_su_t1970, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1970, 0.36).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_constraint_authority kernel, decomposed per the ε-invariance principle because the natural-language label 'Magna Carta's authority' conflates structurally distinct claims with different ε values: feudal_obsolescence_reading (ε near zero — no live coordination function, historically inert), living_constitutionalism_reading (this story — ε 0.28, genuine ongoing coordination via judicial interpretation), and parliamentary_sovereignty_reading (ε to be authored separately — restraint authority fully subordinated to and revisable by Parliament). Each carries its own beneficiary/victim structure and classification; they are linked here rather than merged into one averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
