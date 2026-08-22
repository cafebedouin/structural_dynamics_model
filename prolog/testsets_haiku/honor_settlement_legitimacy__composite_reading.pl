% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor Settlement Legitimacy (Composite Decline Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   Honor settlement — formal dueling as a legitimate mechanism for resolving
 *   insults to social standing — declined from 1600 to 1900 across European
 *   nobility. The decline is NOT singular in cause. This reading
 *   (composite_reading) asserts that multiple mechanisms converged to
 *   suppress the practice: (1) state criminalization and prosecution
 *   (institutional/legal suppression), (2) bourgeois cultural dominance
 *   delegitimizing honor as primitive (cultural/cognitive shift), (3)
 *   economic irrationality as commercial law became the prestige system
 *   (material/institutional realignment), and (4) identity lock binding
 *   noblemen to a practice the law forbade and culture ridiculed
 *   (psychological/identity trap). The constraint structure transforms over
 *   the interval: early on, dueling persists as a real practice with real
 *   participants and real resistance to suppression; late in the interval, it
 *   survives primarily as ritualized theater among a shrinking residue,
 *   maintained by identity commitment rather than functional legitimacy. The
 *   reading instantiates a composite causal story where contraction (cultural
 *   unthinkability) dominates but is REINFORCED by material and legal changes
 *   that would independently suppress practice even absent the cognitive
 *   shift.
 *
 * KEY AGENTS:
 *   - state_legal_apparatus: Criminalizes dueling and prosecutes violators; monopolizes legitimate violence and dispute resolution authority.
 *   - bourgeois_commercial_culture: Redefines honor as barbaric and commercial law as rational; displaces honor-based legitimacy.
 *   - nobility_residual_honor_culture: Maintains honor as social identity; bears extraction (criminalization, cultural delegitimization, identity trap).
 *   - legal_reform_intellectuals: Construct the intellectual case that dueling is irrational; shape the bourgeois cultural frame.
 *   - would_be_duelers: Caught between honor norms and legal prohibition; experience identity lock and material cost inflation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.79).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor Settlement Legitimacy (Composite Decline Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, 'f08fc2d2-b85a-4304-9a6a-c0226c06c9cc').
narrative_ontology:cs_kernel_codification('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', distributed).
narrative_ontology:cs_authority_grounding('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', extraction).
narrative_ontology:cs_interpretation_layer_present('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc').
narrative_ontology:cs_reading_relation('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', foundational, multiple_mechanisms_overdetermined).
narrative_ontology:cs_axiom_status(multiple_mechanisms_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', multiple_mechanisms_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', foundational, contraction_dominates_but_reinforced).
narrative_ontology:cs_axiom_status(contraction_dominates_but_reinforced, holdable).
narrative_ontology:cs_axiom_grounding('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', contraction_dominates_but_reinforced, empirically_contingent).
narrative_ontology:cs_reference_frame('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', honor_settlement_legitimate_noble_autonomous).
narrative_ontology:cs_drift_state('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', post_enlightenment_bourgeois_dominance, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('f08fc2d2-b85a-4304-9a6a-c0226c06c9cc', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_commercial_culture).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, nobility_residual_honor_culture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, legal_reform_intellectuals).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, would_be_duelers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state monopolizes legitimate violence and dispute resolution. It actively prosecutes dueling as murder despite nobility's claims that honor resolution lies outside state jurisdiction. The state apparatus benefits by eliminating rival dispute-resolution mechanisms and consolidating legal authority. Its enforcement infrastructure (police, courts, prosecution) must remain active to suppress persistent dueling.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Commercial, industrial society requires predictable legal frameworks and written contract enforcement, not honor-based dispute resolution. As bourgeois culture becomes the dominant cultural frame, honor-based settlement becomes categorically unthinkable — not merely costly but irrational. Bourgeois institutions (courts, insurance, banking) depend on dueling's illegitimacy and irrelevance.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_commercial_culture, beneficiary,
    organized, generational, mobile, national).

% The nobility traditionally resolved honor disputes through formal dueling; this was their claim to autonomous justice. As the state criminalizes dueling and bourgeois culture delegitimizes it as barbaric, they face a choice: submit to state courts (accepting subordination) or continue in secret (accepting criminal prosecution). Their identity as honorable men becomes contingent on practicing what the law forbids and culture ridicules. Exit from identity (renouncing honor) is unthinkable within their own framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, nobility_residual_honor_culture, payer,
    powerful, generational, identity_locked, national).

% Enlightenment and 19th-century reformers (Voltaire, Montesquieu, Bentham) actively construct the intellectual case that dueling is irrational and barbaric. Their work delegitimizes honor settlement in the public sphere and shapes bourgeois law. They benefit by influence; they observe the constraint from outside the honor economy itself.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, legal_reform_intellectuals, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, legal_reform_intellectuals, observer).

% Individuals (officers, nobility) caught between honor norms and legal prohibition. Challenging an insult by dueling risks prosecution; accepting the insult risks reputation loss and social exclusion. Both paths exact a cost that did not exist when dueling was legitimate. Their exit from honor culture itself is barred by their social position.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, would_be_duelers, payer,
    moderate, biographical, identity_locked, national).

% Families of dueling victims (killed for reasons of honor that outsiders see as trivial) have no voice in the honor culture's own framing. They are the collateral damage of the system. Once state prosecution becomes viable, they benefit from legal recourse, but they were excluded from the honor debate entirely while the system persisted.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, victim_families_residual, excluded,
    powerless, biographical, trapped, national).

% Church authority formally opposed dueling for centuries as murder and violation of divine law; however, its enforcing power declined as state monopoly on violence solidified. The church observes the constraint from an institutional perspective aligned with the state's legal prohibition, though for distinct (theological) reasons.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, church_moral_authority, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, state_legal_apparatus).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor settlement provided autonomous dispute resolution outside state courts for the nobility — a coordination function that solved how to remedy insults without state mediation. The constraint structures who can call the other honorable and who must submit to legal authority.
% TRANSFER_FUNCTION: Moves the legitimate authority to define and resolve honor disputes from the nobility (private, autonomous) to the state (public, monopolized). Moves the cultural prestige of honor resolution from the nobility to the bourgeoisie. Extracts the option of honor-based justice from those whose identity depended on it.
% ABSENT_VOICES: Dueling victims and their families are structurally absent from the honor culture itself — they have no seat at the table of honor. Once state prosecution provides recourse, they become visible but only as the cost that justifies suppression, not as stakeholders with say in the system itself.
% DISAPPEARANCE_RATIONALE: If honor settlement and its legal prohibition both vanished, the legal system would reorganize around written contract and state courts (already the dominant form). The nobility would have a path back to honor settlement, but commercial society's institutional weight would make dueling economically irrational even if legal. The constraint's disappearance would take centuries to reverse.
% FOUNDING_PROBLEM: Honor insults required swift, autonomous resolution to preserve social standing; the legal system was too slow and did not recognize honor as a valid legal category. Nobility needed a recognized mechanism for honor recovery outside state courts.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (slow state courts, honor not recognized as legal) was real in 1600–1700. By 1850, it was obsolete: state courts had formalized and accelerated, honor had been reclassified as barbaric rather than legitimate, and commercial law had become the prestige system. Legal historians and sociologists document this shift; competition authorities and bourgeois reformers explicitly worked to accelerate it.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction begins low (0.15 at t=0) because dueling is still legitimate in the social circles where it occurs — the constraint has not yet imposed a cost on those it governs; it is still a coordination mechanism. By t=50, extraction rises (0.32) as state prosecution begins and cultural delegitimization accelerates — the practice now imposes costs (legal risk, social ridicule) on those who continue. By t=100, extraction reaches 0.51 as the bourgeois frame becomes dominant and dueling transitions from legitimate to deviant. The trajectory plateaus at t=200–250 (0.65–0.68) because by then dueling has been eliminated from the mainstream; residual practice is marginal and mostly theater. Suppression requirement tracks extraction with a lag: early suppression is light (0.25 at t=0) because social coordination suffices; later suppression intensifies (0.79 at t=250) as the state and culture work in concert to eliminate remnants. Theater rises sharply from t=100 onward (0.22 to 0.42) as dueling persists among the residue less as functional practice and more as identity ritual. The coercion grid shows that individual-level and organizational-level accessibility collapse sharply (individual rises from 0.35 to 0.82; organizational from 0.38 to 0.81) — alternatives to honor settlement close off completely for those caught in the identity frame. Structural-level accessibility collapse is less severe (0.28 to 0.75) because the system as a whole has alternatives (state courts, written contracts); what closes off is the individual's ability to pursue honor settlement without criminal prosecution. Resistance is high at t=0 (0.68) among the nobility but falls to 0.28 by t=250 as resistance becomes futile and costly.
 *
 * PERSPECTIVAL GAP:
 *   The payer (nobility) and agenda-setter (state) seats compute radically differently. From the state's perspective, this is institutional consolidation of legitimate force — a Rope where coordination is genuine (unified legal authority, predictable courts). From the nobility's perspective, it is a Snare disguised as coordination — the state co-opts the language of legal rationality to extract the option of honor-based justice. The engine computes both per-seat; the authored claimed_type (tangled_rope) reflects the structure: there IS a coordination function (unified legal authority), but it is ENTANGLED with extraction (loss of noble autonomy). The measurement series show how the composition shifts: early in the interval the coordination is more real (dueling still functions for some; state courts are genuinely new); late in the interval the extraction dominates (dueling is nearly extinct; the state monopoly is uncontested).
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and bourgeois culture benefit from the prohibition: they collect the eliminated alternative (exclusive jurisdiction, cultural prestige). From their seats, dueling's disappearance is not extraction but consolidation of legitimate authority — d approaches 0.0 (beneficiary end). From the nobility's seat, the same process is extraction: they lose an option that defined their autonomy and identity, and are forced to accept state jurisdiction or face prosecution — d approaches 1.0 (target end). The divergence is structural: the beneficiaries experience coordination (legitimate authority consolidated); the targets experience suppression (alternatives eliminated). Identity lock for the nobility is crucial: they cannot simply exit honor culture and adopt bourgeois norms without renouncing their own social identity. This makes their d value stable and high throughout the interval, whereas for the beneficiaries d is stable and low (they never bear costs from the prohibition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is DEAD by t=200. The founding problem was that honor insults required swift autonomous resolution outside state courts. By 1850–1900, this problem is solved by bourgeois law itself: state courts are fast, written contracts are legitimate, honor is reclassified as not-a-legal-category. The constraint persists AFTER the founding problem disappears, purely by inertia and institutional embedding. This is the mandatrophy signature. However, the constraint is NOT yet a piton at t=250: it still extracts (0.68), still requires suppression (0.79), still shapes behavior (honor is still unthinkable in bourgeois circles as a legal option). A piton would show extraction declining and theater dominating. This constraint shows extraction plateaued and theater elevated but not dominant. The reading is that mandatrophy is LIVE — the founding problem is dead, the constraint persists, and the system works to maintain it — but the constraint has not yet degraded into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_material_primacy,
    'Did dueling decline primarily because honor became cognitively unthinkable (contraction reading), or would it have declined from legal prohibition and material institutional change even if honor remained culturally respectable?',
    'Counterfactual analysis from legal-history cases where dueling was decriminalized (e.g., some German states in the 19th century) to observe whether the practice revived if cultural stigma was locally weaker.',
    'If decriminalized practice revived in culturally-residual regions, material suppression is secondary to contraction. If decriminalization produced no revival even in honor-sympathetic regions, material suppression (legal + economic) is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_material_primacy, empirical, 'Whether contraction (cognitive unthinkability) is the dominant suppression mechanism or one of several equally-weighted mechanisms.').

omega_variable(
    identity_lock_irreversibility,
    'For noblemen caught in the identity lock (honor-bound to dueling, but dueling is criminal), is exit from honor identity possible, or is the lock truly irreversible without generational turnover?',
    'Historical record of individual noblemen who renounced honor and adopted bourgeois legal frameworks; analysis of generational cohorts'' acceptance of state-court justice.',
    'If exit is possible and individual noblemen chose it, the lock is permeable and extraction is lower than measured. If exit is structurally unavailable (identity loss = social death), the lock is binding and extraction is correctly measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_irreversibility, empirical, 'Whether identity lock for the nobility is a structural constraint or a choice within an identity frame.').

omega_variable(
    sibling_reading_framing_dependence,
    'Is the ''multiple mechanisms'' framing of composite_reading dependent on the choice to evaluate dueling''s legitimacy on the axis of state vs. noble authority (the committer frame), or would decomposing dueling into functional components (honor recovery vs. dispute resolution vs. social-status assertion) change which reading dominates?',
    'Reframing analysis: define dueling''s functional components separately and trace which mechanisms suppress each component independently. If different components have different dominant mechanisms, the composite reading is frame-dependent.',
    'If composite reading is frame-dependent, it is a robust description of one committer axis but not a universal account. The alternative decomposition (sibling readings) would become equally legitimate. If mechanisms remain overdetermined across reframings, the composite reading is more robustly causal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_dependence, conceptual, 'Whether the composite reading''s causal account is frame-independent or committer-dependent.').

omega_variable(
    bourgeois_culture_agency_vs_inevitability,
    'Did bourgeois culture deliberately suppress honor settlement as a strategic threat to its legal system, or did honor''s decline follow inevitably from economic transformation (industrialization, written contracts becoming necessities) that made honor-based settlement economically irrational?',
    'Archival evidence of intellectual and policy deliberation: did reformers and lawyers explicitly target honor settlement as a rival system to eliminate, or was the delegitimization a post-hoc rationalization of economic shifts?',
    'If deliberate suppression, the beneficiary (bourgeois culture) actively extracted by working to eliminate the alternative — the constraint is extractive at the system level. If post-hoc rationalization, the beneficiary passively won because their system was more efficient — the constraint is a coordination shift, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bourgeois_culture_agency_vs_inevitability, empirical, 'Whether bourgeois suppression of honor settlement was strategic or incidental to economic transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__composite_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__composite_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(hono_tr_t150, honor_settlement_legitimacy__composite_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement(hono_tr_t200, honor_settlement_legitimacy__composite_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement(hono_tr_t250, honor_settlement_legitimacy__composite_reading, theater_ratio, 250, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__composite_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__composite_reading, base_extractiveness, 100, 0.51).
narrative_ontology:measurement(hono_be_t150, honor_settlement_legitimacy__composite_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(hono_be_t200, honor_settlement_legitimacy__composite_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement(hono_be_t250, honor_settlement_legitimacy__composite_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__composite_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__composite_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(hono_su_t150, honor_settlement_legitimacy__composite_reading, suppression_requirement, 150, 0.71).
narrative_ontology:measurement(hono_su_t200, honor_settlement_legitimacy__composite_reading, suppression_requirement, 200, 0.77).
narrative_ontology:measurement(hono_su_t250, honor_settlement_legitimacy__composite_reading, suppression_requirement, 250, 0.79).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=250
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 0, 0.32).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 250, 0.79).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 250, 0.82).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 250, 0.81).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 0, 0.28).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 250, 0.75).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__composite_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__composite_reading, resistance(class), 250, 0.25).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__composite_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__composite_reading, resistance(individual), 250, 0.28).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__composite_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__composite_reading, resistance(organizational), 250, 0.32).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__composite_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__composite_reading, resistance(structural), 250, 0.18).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 0, 0.18).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 250, 0.65).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 0, 0.2).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 250, 0.72).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 0, 0.22).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 250, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 0, 0.15).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 250, 0.61).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__composite_reading, suppression(class), 0, 0.2).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__composite_reading, suppression(class), 250, 0.75).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__composite_reading, suppression(individual), 0, 0.18).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__composite_reading, suppression(individual), 250, 0.81).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__composite_reading, suppression(organizational), 0, 0.22).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__composite_reading, suppression(organizational), 250, 0.78).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__composite_reading, suppression(structural), 0, 0.32).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__composite_reading, suppression(structural), 250, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The kernel 'honor_settlement_legitimacy' decomposes into three structurally distinct constraint stories: composite_reading (multiple overlapping suppression mechanisms, contraction dominates), contraction_reading (cultural unthinkability as the primary cause), and drop_reading (dueling persists as fringe practice among residual honor communities). Each reading instantiates a different causal story about why honor settlement legitimacy collapsed. The ε values differ: composite_reading assigns high extractiveness (0.68) because multiple mechanisms work in concert to suppress; contraction_reading would assign higher extractiveness if contraction is sufficient alone; drop_reading would assign lower effective extractiveness because persistence among residual communities suggests incomplete suppression. All three are readings of the same kernel (contested legitimacy of honor as a legal mechanism), but they structure the causal account differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
