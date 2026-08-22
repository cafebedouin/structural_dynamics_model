% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Fard 'Ayn Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story instantiates the revolutionary vanguard reading of the jihad
 *   kernel: jihad as an immediate individual obligation (fard 'ayn) against
 *   rulers and regimes declared apostate, and against occupiers, activated by
 *   takfir rulings issued by non-state commanders and legitimated through
 *   emergency jurisprudence (fiqh al-darura) that suspends the classical
 *   procedural safeguards — imam authorization, formal declaration,
 *   proportionality, non-combatant immunity. This reading is structurally
 *   distinct from the defensive_spiritual_reading (jihad al-nafs plus
 *   constrained defensive response) and the expansionist_legalist_reading
 *   (state-authorized offensive campaigns under classical jurisprudential
 *   conditions): where those readings retain either an internal-spiritual
 *   center of gravity or a state/imam gatekeeping function, this reading's
 *   defining move is precisely the elimination of the gatekeeping function
 *   itself, replacing it with individual or small-group discretion exercised
 *   under a self-declared emergency. That elimination is what pulls apostate
 *   Muslims and occupying-state civilians into the victim set and what makes
 *   collective-guilt reasoning available as a targeting logic. The ε authored
 *   here (0.81) is high because this specific reading's operation —
 *   decentralized takfir, suspended safeguards, identity-locked recruitment —
 *   produces extraction (risk transfer onto fighters, civilians, and accused
 *   apostates; benefit capture by commanders and ideologues) that the sibling
 *   readings do not structurally generate to the same degree.
 *
 * KEY AGENTS:
 *   - takfiri_commanders
 *   - vanguard_ideologues
 *   - accused_apostate_muslims
 *   - civilian_populations_in_contested_zones
 *   - occupying_state_personnel
 *   - mainstream_clerical_establishment
 *   - recruited_fighters
 *   - rival_insurgent_factions
 *   - comparative_jurisprudence_scholars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.81).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Fard 'Ayn Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '0999b44c-0c29-416a-a9a1-66d6e57b45ff').
narrative_ontology:cs_kernel_codification('0999b44c-0c29-416a-a9a1-66d6e57b45ff', fixed_text).
narrative_ontology:cs_authority_grounding('0999b44c-0c29-416a-a9a1-66d6e57b45ff', practice).
narrative_ontology:cs_interpretation_layer_present('0999b44c-0c29-416a-a9a1-66d6e57b45ff').
narrative_ontology:cs_reading_relation('0999b44c-0c29-416a-a9a1-66d6e57b45ff', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0999b44c-0c29-416a-a9a1-66d6e57b45ff', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('0999b44c-0c29-416a-a9a1-66d6e57b45ff', foundational, emergency_suspends_procedural_authorization).
narrative_ontology:cs_axiom_status(emergency_suspends_procedural_authorization, holdable).
narrative_ontology:cs_axiom_grounding('0999b44c-0c29-416a-a9a1-66d6e57b45ff', emergency_suspends_procedural_authorization, instrumental).
narrative_ontology:cs_axiom('0999b44c-0c29-416a-a9a1-66d6e57b45ff', foundational, individual_discernment_of_apostasy_binds_without_central_ratification).
narrative_ontology:cs_axiom_status(individual_discernment_of_apostasy_binds_without_central_ratification, holdable).
narrative_ontology:cs_axiom_grounding('0999b44c-0c29-416a-a9a1-66d6e57b45ff', individual_discernment_of_apostasy_binds_without_central_ratification, conventional).
narrative_ontology:cs_reference_frame('0999b44c-0c29-416a-a9a1-66d6e57b45ff', classical_imam_authorized_jihad_doctrine).
narrative_ontology:cs_drift_state('0999b44c-0c29-416a-a9a1-66d6e57b45ff', post_20th_century_state_collapse_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0999b44c-0c29-416a-a9a1-66d6e57b45ff', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_insurgent_factions).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, accused_apostate_muslims).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_contested_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_state_personnel).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_clerical_establishment).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, recruited_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, recruited_fighters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare which rulers, institutions, and individuals are apostate (murtad), thereby activating the individual-obligation doctrine and recruiting fighters under it. They administer emergency jurisprudence (fiqh al-darura) that suspends classical procedural safeguards (imam authorization, proportionality review, non-combatant immunity) on the grounds that the emergency itself justifies bypassing them. They move between fronts and factions as alliances shift, retaining leverage regardless of any single campaign's outcome.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders, agenda_setter,
    organized, biographical, arbitrage, regional).

% Produce and circulate the doctrinal literature that grounds takfir and fard 'ayn mobilization — often from outside the contested territory itself. They gain influence, funding, and recruitment pipelines from the doctrine's spread without bearing direct combat risk; their exit options (relocation, media platforms, transnational networks) are far wider than those of the fighters they recruit.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues, beneficiary,
    organized, generational, mobile, global).

% Rulers, officials, security personnel, or ordinary Muslims judged insufficiently orthodox are declared murtad by a rival faction's takfir ruling, which reclassifies killing them as a religious duty rather than a crime. They cannot appeal to a shared jurisprudential authority because the doctrine itself denies the legitimacy of the state or clerical body that would hear such an appeal; the declaration itself is the trap.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, accused_apostate_muslims, payer,
    powerless, immediate, trapped, local).

% Live under, near, or between forces operating under the fard 'ayn mobilization. Collective-guilt reasoning (tax-paying to the apostate state, working for its institutions, even passive residence in contested territory) can reclassify them as legitimate targets, collapsing the combatant/non-combatant distinction the classical jurisprudence they were told applied to them. Displacement is the only functional exit, and it is frequently blocked by the fighting itself.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_contested_zones, payer,
    powerless, immediate, trapped, local).

% Soldiers, administrators, and contractors of an occupying or foreign-backed government are named targets under the doctrine's occupier clause. They can withdraw or rotate out at policy discretion, giving them more exit than local civilians, but their presence itself is the doctrine's stated justification, and continued deployment sustains the mobilization it is used to legitimize.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_state_personnel, payer,
    institutional, biographical, constrained, national).

% State-affiliated scholars and institutions that hold classical jurisprudential authority over jihad rulings (imam authorization, formal declaration, proportionality) are structurally bypassed by the doctrine's core move: that emergency conditions permit any qualified individual or small group to independently discharge the obligation. Their rulings against the vanguard reading are treated by the doctrine's adherents as evidence of the clerics' own collaboration with apostasy, which forecloses their ability to be heard as neutral arbiters.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_clerical_establishment, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_clerical_establishment, excluded).

% Often young men persuaded that individual religious duty overrides family, state, and communal obligations. They gain purpose, belonging, and religious certainty from the framework, but bear nearly all physical risk and are structurally difficult to disengage from once identity and social bonds are reorganized around the obligation — desertion is coded not as a tactical choice but as apostasy itself, closing off the exit the doctrine's own logic would otherwise permit.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, recruited_fighters, payer,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, recruited_fighters, beneficiary).

% Competing armed groups invoke the same fard 'ayn and takfir logic against each other as well as against the nominal common enemy, using the doctrine's decentralization to justify independent action, funding capture, and territorial control without needing to coordinate with or defer to any other faction's authority.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_insurgent_factions, beneficiary,
    organized, generational, arbitrage, regional).

% Trace how classical safeguards (imam authorization, proportionality, non-combatant immunity) are reinterpreted or discarded under emergency doctrine, and document how the individual-obligation framing was assembled from selective readings of specific historical fatawa and crisis-era jurisprudence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, comparative_jurisprudence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rapid-mobilization framework for armed resistance when no functioning, religiously legitimate central authority is available to authorize collective defense — solving a genuine coordination failure in contexts of state collapse or occupation where classical channels for authorizing jihad have been captured or destroyed.
% TRANSFER_FUNCTION: Moves recruitment, funding, and moral authority away from state and mainstream clerical institutions toward decentralized commanders and ideologues, while moving physical risk, social death, and often life itself from those commanders onto recruited fighters, accused apostates, and civilians caught in contested zones.
% ABSENT_VOICES: Accused apostates have no forum: the doctrine denies legitimacy to precisely the institutions (state courts, mainstream clerical councils) that would ordinarily hear a defense against a takfir charge. Civilian populations in contested zones are spoken about in collective-guilt terms but never party to the ruling that reclassifies them as targets. Families of recruited fighters, whose social and economic stability is destroyed by identity-locked recruitment, are absent from the theological argument entirely.
% DISAPPEARANCE_RATIONALE: If the individual-obligation, bypass-the-state doctrine vanished, mobilization for armed action would again require some form of centralized religious-political authorization, sharply reducing the number of independent factions able to declare jihad, collapsing the takfir pipeline that currently reclassifies civilians and rival Muslims as legitimate targets, and re-routing disputes back toward jurisprudential and political channels — a substantial rearrangement of who can wage war and against whom.
% FOUNDING_PROBLEM: In moments of state collapse, foreign occupation, or a ruling class judged to have abandoned Islamic governance, classical jurisprudence's requirement of imam authorization leaves communities without a legitimate mechanism to organize armed resistance — the doctrine was built to fill that authorization vacuum.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard ideologues and commanders attest the authorization vacuum is still live wherever they operate. Mainstream clerical bodies, several post-conflict truth commissions, and independent scholars of Islamic law attest that in most contemporary invocations no genuine authorization vacuum exists — functioning, if imperfect, religious and political institutions are available — and that the doctrine persists chiefly as a recruitment and legitimation tool for factions competing for territory and funding rather than as a response to an actual absence of authority.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.48 to 0.81) reflecting the doctrine's documented drift from an initially narrower emergency-authorization argument toward increasingly expansive takfir practice and collective-guilt targeting as competing factions adopted and radicalized the framework. Suppression is very high (0.88) because persistence depends on actively closing off appeal channels (denying legitimacy to state courts and mainstream clerical bodies) and on identity-locking recruits so exit reads as apostasy rather than choice. Theater ratio is comparatively low (0.28) — the coordination function (rapid mobilization absent legitimate central authority) is not merely performed; it is genuinely exercised in cases of real state collapse, which is exactly why this reading cannot be classified as pure snare: a real coordination problem exists alongside the extraction. All three temporal metrics share one time grid (0, 8, 16, 24, 32, 40) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (takfiri_commanders) the arrangement is a legitimate discharge of individual religious duty in the absence of authorized central authority. From the payer seats (accused_apostate_muslims, civilian_populations_in_contested_zones, recruited_fighters) the same structure operates as an unappealable death sentence or an identity trap with no honest exit. The engine's per-seat computation should register this divergence directly from the declared power/exit asymmetries — trapped and identity_locked payers sit far toward the target end of directionality; organized/arbitrage-exit commanders and ideologues sit far toward the beneficiary end.
 *
 * DIRECTIONALITY LOGIC:
 *   Takfiri_commanders and vanguard_ideologues are declared beneficiaries with wide exit (arbitrage, mobile) — they collect authority, funding, and narrative capital without bearing proportional physical risk, so directionality derives toward the beneficiary end. Accused_apostate_muslims and civilian_populations_in_contested_zones are declared victims with trapped exit and immediate time horizon — the doctrine's collective-guilt and takfir logic is what converts their prior civilian status into targetable status, so directionality derives strongly toward the target end. Recruited_fighters carry a secondary beneficiary role (purpose, belonging) but their exit_options is identity_locked rather than merely constrained — this is why an omega below flags the suppression-mechanism ambiguity for this seat specifically. Mainstream_clerical_establishment is a payer/excluded dual seat: its institutional authority is the thing the doctrine is structurally built to bypass, which is a cost distinct from physical extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authorization vacuum during state collapse or occupation) can be genuinely live in some invocations and structurally dead in most contemporary ones, which is why founding_problem_status is authored as contested rather than resolved either way — collapsing this into a flat 'always cover story' or 'always genuine' claim would misclassify. Reading the doctrine as tangled_rope rather than pure snare depends on treating the rare genuine-vacuum case as real coordination while the vastly more common territorial-competition invocation is extraction riding on that coordination's residual legitimacy — exactly the hybrid structure tangled_rope is built to name.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vacuum_vs_manufactured_emergency,
    'In any given invocation, is the fard ''ayn mobilization responding to a genuine authorization vacuum (real state collapse, real absence of any legitimate religious-political authority) or is the ''emergency'' itself manufactured or exaggerated by the commander declaring it, as a legitimation device for what is otherwise factional or criminal violence?',
    'Case-by-case historical and political analysis of whether alternative legitimate authorities existed and were functioning at the time of the takfir declaration; testimony from displaced populations and comparative jurisprudence scholars as to whether classical channels remained accessible.',
    'If the vacuum is genuine, the coordination function is real and the tangled_rope classification is well-grounded; if manufactured, the coordination story is closer to pure cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vacuum_vs_manufactured_emergency, empirical, 'Whether the doctrine''s triggering condition (authorization vacuum) is factually present or rhetorically asserted.').

omega_variable(
    takfir_declaration_legitimacy_standard,
    'Within the revolutionary vanguard framework itself, is there any commonly recognized evidentiary standard for a valid takfir declaration, or does the framework''s decentralization mean any commander''s declaration is self-validating?',
    'Comparative analysis of takfir declarations across multiple factions to see whether any shared evidentiary or procedural standard is actually applied, versus purely commander-discretionary assertion.',
    'A shared standard would suggest some residual coordination discipline within the doctrine; pure self-validation would confirm the accessibility_collapse score and support a harder extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takfir_declaration_legitimacy_standard, empirical, 'Whether takfir declarations are constrained by any inter-subjective standard or are purely discretionary.').

omega_variable(
    collective_guilt_scope_ambiguity,
    'How far does the collective-guilt logic extend in practice — does it require active material support for the occupier/apostate regime, or does mere residence, tax-paying, or employment suffice to reclassify a civilian as a legitimate target?',
    'Documentation of actual targeting decisions and post-hoc justifications across multiple campaigns invoking this doctrine.',
    'A narrow reading (active material support required) would moderate the extractiveness score toward something closer to the expansionist_legalist_reading''s proportionality constraints; a broad reading (mere residence suffices) sustains the high extractiveness and victim-set breadth authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_guilt_scope_ambiguity, conceptual, 'The operative breadth of collective-guilt civilian targeting under this reading.').

omega_variable(
    recruited_fighter_suppression_mechanism,
    'Is the exit-blocking experienced by recruited_fighters structural (social/physical enforcement by the faction) or internalized (identity fusion with the cause such that desertion feels like self-annihilation even absent external enforcement), or both in some proportion?',
    'Post-defection interview data and deradicalization program case records: if disengaged fighters report persistent felt obligation or shame after removal from the faction''s physical control, that indicates a substantial internalized component.',
    'If internalized, the effective suppression on this seat is higher than the structural coercion measure alone suggests — recruits carry the constraint''s suppressive force with them even after physical exit, which matters for deradicalization design and for how completely ''trapped'' should be distinguished from ''identity_locked'' in future stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recruited_fighter_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for recruited fighters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jiha_tr_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(jiha_tr_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(jiha_tr_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(jiha_be_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(jiha_be_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(jiha_be_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jiha_su_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(jiha_su_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(jiha_su_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: jihad_quranic_corpus kernel, three readings. defensive_spiritual_reading retains non-combatant immunity and an internal-spiritual center of gravity (near-mountain/rope territory for its own claim). expansionist_legalist_reading retains imam-authorization gatekeeping and classical proportionality while permitting offensive campaigns (tangled_rope territory with a narrower victim set). revolutionary_vanguard_reading (this story) eliminates both the spiritual center and the authorization gate, producing the widest victim set and highest authored extractiveness of the three. Each story authors its own stable ε from its own reading's operation; none averages across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
