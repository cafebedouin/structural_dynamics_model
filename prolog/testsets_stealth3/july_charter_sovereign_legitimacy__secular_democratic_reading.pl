% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Sovereign Legitimacy - Secular Democratic Reading
 *   domain: constitutional/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   A post-uprising charter fixes the state's constitutional form: secular
 *   democratic institutions, universal civic citizenship, and armed forces
 *   placed under elected civilian command. The arrangement is enforced rather
 *   than self-executing - party-registration tribunals, command-authorization
 *   rules, and amendment supermajorities keep it in place. Its coordination
 *   core is real: it converted a revolutionary legitimacy vacuum into
 *   scheduled elections and a working civilian chain of command. Its
 *   asymmetries are equally real: the same instruments suspend the principal
 *   Islamist party's registration and candidacy rights, strip the officer
 *   corps of autonomous budget and deployment authority, and leave the ranks
 *   executing orders they had no hand in writing. The claim and the metrics
 *   are independent authored facts: this reading claims tangled_rope because
 *   it sees genuine coordination carrying real asymmetry; the engine computes
 *   per-seat verdicts from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take. KEY
 *   AGENTS (by structural relationship): - interim_government_council:
 *   Agenda-setter (institutional/constrained) - administered the transition,
 *   convened the constituent process, signed the charter into force -
 *   secular_democratic_parties: Primary beneficiary (organized/mobile) -
 *   gains a cleared electoral field and governing access - jamaat_e_islami:
 *   Primary target (organized/identity_locked) - bears registration
 *   suspension and candidacy disqualification - military_officer_corps:
 *   Secondary target (institutional/constrained) - stripped of autonomous
 *   budget, promotion, and deployment authority -
 *   religious_minority_communities: Diffuse beneficiary
 *   (moderate/constrained) - holds new equal-citizenship guarantees -
 *   student_movement_veterans: Beneficiary (organized/mobile) - converts
 *   uprising legitimacy into office - rural_religious_establishment: Excluded
 *   voice (moderate/trapped) - normative authority ruled out of the
 *   conversation - conscript_soldiers_and_junior_ranks: Unrepresented bearer
 *   (powerless/trapped) - executes subordination without a channel -
 *   constitutional_court: Enforcement interpreter (institutional/constrained)
 *   - adjudicates the charter's meaning - international_democracy_partners:
 *   Analytical observer (institutional/analytical) - conditions aid on
 *   civilian-supremacy milestones
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.54).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Sovereign Legitimacy - Secular Democratic Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'eeecbb5d-7216-42d3-81d7-710863a81672').
narrative_ontology:cs_kernel_codification('eeecbb5d-7216-42d3-81d7-710863a81672', fixed_text).
narrative_ontology:cs_authority_grounding('eeecbb5d-7216-42d3-81d7-710863a81672', lineage).
narrative_ontology:cs_interpretation_layer_present('eeecbb5d-7216-42d3-81d7-710863a81672').
narrative_ontology:cs_reading_relation('eeecbb5d-7216-42d3-81d7-710863a81672', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('eeecbb5d-7216-42d3-81d7-710863a81672', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('eeecbb5d-7216-42d3-81d7-710863a81672', foundational, secular_state_neutrality_toward_religion).
narrative_ontology:cs_axiom_status(secular_state_neutrality_toward_religion, holdable).
narrative_ontology:cs_axiom_grounding('eeecbb5d-7216-42d3-81d7-710863a81672', secular_state_neutrality_toward_religion, deontological).
narrative_ontology:cs_axiom('eeecbb5d-7216-42d3-81d7-710863a81672', foundational, civilian_supremacy_over_armed_forces).
narrative_ontology:cs_axiom_status(civilian_supremacy_over_armed_forces, holdable).
narrative_ontology:cs_axiom_grounding('eeecbb5d-7216-42d3-81d7-710863a81672', civilian_supremacy_over_armed_forces, instrumental).
narrative_ontology:cs_reference_frame('eeecbb5d-7216-42d3-81d7-710863a81672', revolutionary_popular_mandate_secular_civilian_order).
narrative_ontology:cs_drift_state('eeecbb5d-7216-42d3-81d7-710863a81672', post_ratification_entrenchment_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eeecbb5d-7216-42d3-81d7-710863a81672', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, student_movement_veterans).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minority_communities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_officer_corps).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, conscript_soldiers_and_junior_ranks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, interim_government_council).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_supremacy_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_secularism_principle).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, popular_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the state between the uprising and ratification: ran the constituent consultation, scheduled elections, and signed the charter into force. Its members drafted the civilian-command provisions they now operate under and stood for office under rules they wrote. After ratification their discretion narrowed to administering what they created.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, interim_government_council, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, interim_government_council, beneficiary).

% Compete for office under a charter that suspends their strongest ideological rival from full participation and guarantees the civic-legal framework their platforms assume. They gain governing access, committee chairs, and first claim on the revolution's legitimacy narrative. Losing an election costs them office, not the field itself.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, beneficiary,
    organized, generational, mobile, national).

% Interprets the charter's meaning in registration disputes, command-authority cases, and amendment challenges. Judges are appointed through the civilian process the charter created. Every seat's dispute ultimately arrives at its bench, and its rulings bind parties that did not consent to them.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% An Islamist political organization whose registration and candidacy rights are challenged, suspended, or conditioned under the charter's secular-mandate provisions. Cadres face disqualification and local welfare networks operate under scrutiny. The organization's entire existence is Islamic political action, so abandoning the field means dissolving the movement rather than rebranding it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, biographical, identity_locked, national).

% Lost autonomous budget lines, final authority over internal promotions, and the doctrine that legitimated internal-security intervention. Commands now route through a civilian defense ministry and deployments require civilian authorization. Officers continue careers inside the institution or resign outright; there is no external market for what they are.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_officer_corps, payer,
    institutional, generational, constrained, national).

% Hindu, Buddhist, Christian, and Ahmadi communities hold explicit equal-citizenship guarantees and protection from majoritarian religious law for the first time in the state's constitutional history. Their day-to-day security still depends on police and courts they do not staff, and emigration remains the quiet fallback.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minority_communities, beneficiary,
    moderate, generational, constrained, national).

% The uprising's cohort supplied the charter's legitimacy story and much of its drafting staff. They move into elected office, ministry posts, and commission seats ahead of older politicians. Their advantage is biographical and ages with them.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, student_movement_veterans, beneficiary,
    organized, biographical, mobile, national).

% Madrasa networks and village clergy outside any party structure. Their normative authority over marriage, inheritance, and schooling was ruled out of the constitutional conversation; they held no seat in drafting and no vote on ratification. They accommodate locally, resist through sermons, and wait.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, rural_religious_establishment, excluded,
    moderate, generational, trapped, regional).

% Donor states and multilateral bodies funding election infrastructure and conditioning assistance on civilian-supremacy milestones. They publish assessments, send observer missions, and hold no domestic seat. Their leverage is financial and reputational.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democracy_partners, observer,
    institutional, generational, analytical, global).

% Junior personnel execute the subordination they had no hand in drafting. Pay, pensions, and promotion now depend on civilian ministries, and dissent inside the ranks has no channel. Service contracts bind them for fixed terms; desertion is the only exit and it forfeits everything.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, conscript_soldiers_and_junior_ranks, payer,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-revolutionary collective-action problem: converting mass-mobilization legitimacy into durable institutions without either a new autocracy or a military veto. It establishes neutral rules for transferring power, a single civilian chain of command over the armed forces, and a common civic framework that spans religious communities.
% TRANSFER_FUNCTION: Moves command authority over the armed forces and agenda-setting power over the state's religious character away from the military high command and Islamist political organizations, and toward elected civilian institutions and the secular-democratic coalitions that dominate them.
% ABSENT_VOICES: The rural religious establishment and non-party religious conservatives had no drafting seat and no ratification vote; junior soldiers subject to subordination were unrepresented; the Islamist organization held consultative seats but its positions were outvoted and its subsequent legal constraint was decided largely by its opponents.
% DISAPPEARANCE_RATIONALE: If the charter and its enforcement vanished overnight, command arrangements over the military, party-registration rules, the election calendar, and the minority-protection guarantees would all collapse back into open contestation; the officer corps and the constrained party would immediately begin reclaiming the ground the charter allocated away.
% FOUNDING_PROBLEM: Prevent the post-uprising legitimacy vacuum from resolving into either a military takeover or sectarian fragmentation, while organizing elections and a constituent process under credible rules.
% FOUNDING_PROBLEM_CORROBORATION: International observer missions, regional democracy assessments, and independent constitutional scholars attest the consolidation risks remain live, and the officer corps' own public statements about instability corroborate from outside the beneficiary set. Note the dissenting attestation: the constrained party's leadership asserts the founding problem is solved and the charter now functions as a partisan tool - that disagreement is itself evidence the status is contested in public discourse, while the liveness claim rests on sources outside the benefiting parties.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.54 because the charter's coordination core - civilian command, a neutral civic framework, election machinery - is real, but the same instruments clear one ideological competitor from the field and demote a second institution, and the founding coalition's use of both grows more partisan with each electoral cycle. Suppression (0.62) reflects machinery that must stay switched on: registration tribunals, command-authorization rules, amendment supermajorities. Suppression is authored as a raw structural property and is NOT scaled by power or scope - only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater (0.40) is rising because anniversary rituals and civilian-supremacy review ceremonies increasingly substitute for substantive security-sector reform. Accessibility collapse is low (0.35): amendment, judicial reinterpretation, and rival readings of the same text remain live alternatives. Resistance (0.58) is sustained - street mobilization by the constrained party, institutional foot-dragging by the officer corps, and a persistent guided-nationalist bloc in the legislature. All three temporal series run on one shared grid (t=0,3,6,9,12,15); the t=0 points are observed, later points projected. The rising trajectories model entrenchment drift: enforcement hardening around a founding coalition rather than relaxing as norms consolidate.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the charter reads as enabling structure: it solved the transition, opened the electoral field, and guaranteed equal citizenship. From the payer seats the identical text reads as enforced demotion: the Islamist organization experiences suspension and surveillance; the officer corps experiences stripped authority; junior ranks experience orders they never consented to. The engine computes these divergent per-seat classifications from power, exit, and role data - the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular-democratic parties, movement veterans, and minority communities sit near the beneficiary pole (low d): the arrangement subsidizes their competition, security, and careers. Jamaat-e-Islami sits near the full-target pole - identity_lock removes arbitrage-grade exit, so the structural derivation amplifies its d toward 1.0. The officer corps is also target-side (high d) but slightly damped: the charter preserves the institution's budget scale and legal personality even as it strips autonomy. Junior ranks inherit the corps' target position without its compensations. The council and the court sit mid-structure: they administer extraction they also undergo, being bound by their own ratification and rulings. International partners are analytical - no directionality worth computing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing the post-uprising vacuum from resolving into coup or sectarian fragmentation - is still live, so mandatrophy has not resolved. The danger signature is visible in the trajectories: theater_ratio climbing toward the 0.5 substitution threshold while extraction creeps upward suggests the coordination story is slowly becoming cover for the founding coalition's entrenchment. If consolidation completes and the exclusions persist past necessity, the mandate will have outlived its function and the structure drifts toward inertial maintenance; if the coalition begins losing elections and tightens the exclusions instead, the drift runs toward pure extraction. Declaring both beneficiaries and victims keeps the verdict from collapsing into either whitewash (pure coordination) or blanket condemnation (pure extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading does the charter text actually bear - does sovereign legitimacy ground in democratic procedure (this reading), in religious identity (guided_nationalism_reading), or in military custodianship (military_custodian_reading)?',
    'Constitutional-court adjudication of the legitimacy articles, an amendment supermajority, or the next constituent process settling the text''s operative meaning.',
    'Wholesale reclassification: under guided_nationalism the victim set inverts toward secular and minority actors; under military_custodian the payer set shifts to civilian politicians and this story''s beneficiaries become targets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'One charter text, three incompatible institutionalizations; this story authors only the secular-democratic instantiation.').

omega_variable(
    foreclosure_stability,
    'Are the forecloses edges to both siblings stable, or can hybrid frameworks absorb the apparently contradictory premises (historical precedents pair secular-democratic constitutional forms with military tutelage)?',
    'Track whether any amendment or court doctrine operationalizes two sibling premises simultaneously; if a durable hybrid emerges, downgrade the edges to influences.',
    'If hybrids prove stable, the computed foreclosure between readings weakens and the kernel''s contest becomes a bargaining space rather than a partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_stability, conceptual, 'Stability of the logical incompatibility between this reading''s axioms and each sibling''s core premise.').

omega_variable(
    safeguard_vs_entrenchment,
    'Is the constraint on the Islamist party a durable democratic safeguard, or partisan extraction that tracks the founding coalition''s electoral fortunes?',
    'Cross-government test: observe whether registration and candidacy restrictions relax when an opposing coalition wins office; persistence across alternating governments indicates structural safeguard.',
    'If the restrictions track the founders'' fortunes, the extraction component is entrenchment and the drift runs toward snare; if they persist across governments, they sit closer to legitimate constitutional defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguard_vs_entrenchment, empirical, 'Whether the exclusionary edge is safeguard or rent.').

omega_variable(
    subordination_formal_vs_effective,
    'Is civilian supremacy over the military effective in practice, or does formal subordination coexist with de facto autonomous budget, promotion, and deployment behavior?',
    'Trace budget approvals, senior promotions, and cross-border deployments to documented civilian directives over successive fiscal years.',
    'If subordination is formal-only, the civilian-supremacy provision computes as heavily theatrical and the structure drifts toward inertial maintenance; if effective, the provision carries genuine coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_formal_vs_effective, empirical, 'Depth of the civilian-control transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t3, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(july_tr_t3, projected).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(july_tr_t6, projected).
narrative_ontology:measurement(july_tr_t9, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement_basis(july_tr_t9, projected).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(july_tr_t12, projected).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(july_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t3, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 3, 0.43).
narrative_ontology:measurement_basis(july_be_t3, projected).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement_basis(july_be_t6, projected).
narrative_ontology:measurement(july_be_t9, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 9, 0.49).
narrative_ontology:measurement_basis(july_be_t9, projected).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(july_be_t12, projected).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(july_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t3, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement_basis(july_su_t3, projected).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement_basis(july_su_t6, projected).
narrative_ontology:measurement(july_su_t9, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 9, 0.56).
narrative_ontology:measurement_basis(july_su_t9, projected).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(july_su_t12, projected).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(july_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the July Charter' covers three structurally distinct claims about sovereign legitimacy, decomposed per the epsilon-invariance principle into three linked stories sharing the kernel july_charter_sovereign_legitimacy. This member (secular_democratic_reading) authors epsilon for the charter-as-secular-democratic-mandate: moderate extraction, victim set anchored in the constrained Islamist party and the demoted officer corps. The guided_nationalism sibling authors a different epsilon with an inverted victim set (secular and minority actors constrained); the military_custodian sibling authors a third with the payer set shifted onto civilian politicians. The upstream/downstream pressure runs through the shared text: whichever reading captures the constitutional court's interpretation changes the operating environment of the other two. Each story links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
