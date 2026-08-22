% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Post-Manifesto Monogamy Requirement (Substitutionist Reading)
 *   domain: religious/political-theology
 *
 * SUMMARY:
 *   Under the substitutionist reading, the 1890 Manifesto is new binding
 *   revelation that supersedes the prior command to practice plural marriage:
 *   monogamy becomes doctrinally required, contracting plural marriage
 *   afterward constitutes apostasy, and the institution's legitimacy rests on
 *   the shift being revelation rather than concession. The standing
 *   arrangement under contest — the referent for epsilon — is that
 *   post-Manifesto enforcement regime: temple-recommend gatekeeping,
 *   disciplinary councils, excommunication of continuers, and the doctrinal
 *   teaching that binds members to the new command. The claim/metric gap is
 *   deliberate: the reading CLAIMS a divinely authorized covenant boundary,
 *   while the authored metrics describe a substantially extractive, actively
 *   enforced arrangement whose gains concentrate institutionally — the engine
 *   measures that divergence; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - - institutional_church_hierarchy: Primary agenda-setter and beneficiary (institutional/identity_locked) — administers enforcement, collects legitimacy and survival
 *   - - monogamous_latter_day_saints: Beneficiary body (organized/constrained) — receives legal security and statehood benefits, carries diffuse framing costs
 *   - - pre_manifesto_plural_households: Primary target (powerless/trapped) — existing plural marriages stripped of sanction, families不可dissolved without ruin
 *   - - post_manifesto_polygamists: Target (powerless/constrained) — excommunicated for honoring the prior command
 *   - - united_states_federal_government: Excluded coercive counterparty (institutional/mobile) — causally central, structurally unnamed
 *   - - historians_of_american_religion: Analytical observer — attests the documentary genealogy from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.6).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.75).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Post-Manifesto Monogamy Requirement (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political-theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '7f1ece99-ca6d-4f47-bbd7-ba7459795e88').
narrative_ontology:cs_kernel_codification('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', fixed_text).
narrative_ontology:cs_authority_grounding('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', lineage).
narrative_ontology:cs_interpretation_layer_present('7f1ece99-ca6d-4f47-bbd7-ba7459795e88').
narrative_ontology:cs_reading_relation('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', foundational, manifesto_new_revelation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(manifesto_new_revelation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', manifesto_new_revelation_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', secondary, post_manifesto_plural_marriage_is_apostasy).
narrative_ontology:cs_axiom_status(post_manifesto_plural_marriage_is_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', post_manifesto_plural_marriage_is_apostasy, conventional).
narrative_ontology:cs_reference_frame('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', progressive_revelation_supersession).
narrative_ontology:cs_drift_state('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', contemporary_documentary_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f1ece99-ca6d-4f47-bbd7-ba7459795e88', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_latter_day_saints).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, pre_manifesto_plural_households).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, post_manifesto_polygamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of Twelve administer the monogamy requirement: temple recommend interviews, disciplinary councils, and revocation of membership for those contracting plural marriages after the Manifesto. The institution's claim to a continuous line of prophetic authority is fused with the Manifesto's status as revelation; admitting the shift was mere capitulation would dissolve the authority claim itself, so exit from the arrangement is unavailable to the officeholders without dissolving the office.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_church_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% The membership body at large received legal security, territorial statehood citizenship, and social integration into American civic life once the domestic-law conflict ended. They carry diffuse costs: loyalty expectations, financial contributions sustaining the institution, and participation in a collective memory that frames the shift as revelation. Leaving carries real social and familial cost but is possible.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_latter_day_saints, beneficiary,
    organized, biographical, constrained, global).

% Families formed under the prior prophetic command before 1890. The new doctrine stripped their marriages of institutional sanction while the families themselves — children, household economies, sealing bonds — could not be dissolved without destroying the lives built under the earlier command. Many continued de facto; all lost status, and the husbands among them had already borne imprisonment risk under federal raids.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, pre_manifesto_plural_households, payer,
    powerless, biographical, trapped, regional).

% Those who contracted new plural marriages after the Manifesto, treating the prior command as still binding. They face disciplinary councils, excommunication, and loss of temple and community standing, particularly after the 1904 hardening. Their exits lead outward into marginal fundamentalist settlements or inward into abandoning marriages they believe were divinely commanded.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, post_manifesto_polygamists, payer,
    powerless, biographical, constrained, regional).

% The statutory and prosecutorial counterparty whose anti-polygamy legislation, property seizures, and disenfranchisement campaigns created the crisis the arrangement resolves. In the substitutionist account it appears nowhere as cause: the framing that legitimizes the shift as revelation cannot name the coercive agent without converting revelation into capitulation. It obtained its objective and departed the scene.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, united_states_federal_government, excluded,
    institutional, generational, mobile, national).

% Scholars working from congressional records, presidential papers, and church archives who reconstruct the causal genealogy of the 1890 shift. They collect no rent from the arrangement and bear none of its discipline; their seat is the primary extra-beneficiary witness to what the founding problem actually was.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, historians_of_american_religion, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, institutional_church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Realigned the covenant community's domestic law with United States federal law, resolving an otherwise fatal conflict between a binding prior command and national sovereignty; enabled Utah statehood, restored corporate and property standing, and gave the membership a single authoritative resolution of a command-conflict that had split loyalties between prophet and nation.
% TRANSFER_FUNCTION: Moves legitimacy, legal security, and statehood benefits to the institution and the membership body at large; moves marital sanction, sealing validity, and membership itself away from plural households and from anyone contracting plural marriage after the Manifesto; moves doctrinal authority over marriage from the recorded prior command to the present prophetic office.
% ABSENT_VOICES: The federal government whose coercion produced the crisis is structurally absent from the official account — the substitutionist framing cannot admit it as cause without dissolving the revelatory claim. The fundamentalists appear only as objects of discipline, never as interlocutors with a theological case. The testimony of plural wives about family dissolution is subordinated to the institutional narrative of smooth revelatory transition.
% DISAPPEARANCE_RATIONALE: If the monogamy requirement and its enforcement apparatus vanished overnight, the church's legal settlement, Utah's statehood compact, and the entire boundary between the mainline institution and fundamentalist Mormonism would reorganize; the living-prophet doctrine would face immediate crisis over which commands bind and which do not.
% FOUNDING_PROBLEM: The institution faced destruction — property confiscation, imprisonment of leaders, disenfranchisement of the membership — so long as it sustained the prior commanded practice. The arrangement was built to end the commanded practice without forfeiting the prophetic authority that had issued the command.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion, working from congressional records and the documentary trail outside the benefiting parties, attest that the proximate founding problem was federal coercion and that it was resolved by roughly 1907; the institution itself attests instead that the problem was revelatory and remains live. No beneficiary-party source corroborates the dead-status finding — the corroboration comes entirely from the analytical and excluded seats, which is itself signal about the framing.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.6) because even from the substitutionist seat the arrangement strips sanction from existing plural families and membership from continuers while concentrating legitimacy, property standing, and statehood benefits in the institutional seat; the reading's own lights authorize the enforcement but do not erase the transfer. Suppression is high (0.75) and unscaled: excommunication machinery, recommend withdrawal, and the memory-discipline that keeps the coercive genealogy unspeakable in official space. Theater is moderate (0.4): the revelatory framing performs seamless continuity while the operative mechanism is boundary enforcement — and the performance intensified once post-Manifesto sealings had to be denied retroactively. Accessibility collapse is moderate (0.5): fundamentalist settlements persist as live, costly exits, so alternatives contract but do not vanish. Resistance is substantial (0.6): schisms, continued secret plural marriages through the 1900s, and the oppositional testimony of the Smoot hearings era. The measurement series run on one shared time grid (all three metrics at every point 0–40) so no metric row borrows an end-state value. The trajectory is a ratchet, not a cycle: enforcement was lenient at T=0, hardened sharply around T=14 (the 1904 Second Manifesto) under Senate scrutiny of the Smoot seat, peaked near T=27 as the fundamentalist exit completed, then plateaued — the plateau marks the point where suppression shifted from policing insiders to maintaining the boundary against an externalized remnant.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently: from the hierarchy's position the arrangement is the covenant community's survival, purchased and sanctified; from the plural households' and continuers' positions the same structure strips sanction from marriages commanded by the same prophetic office that now dissolves them. The excluded federal seat sharpens the gap: the arrangement reads as revelation only from inside a framing that cannot look at the counterparty that forced it. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy is the structural beneficiary and agenda-setter (collects legitimacy, controls the rules — d near the beneficiary end, further stabilized by identity lock: the office cannot exit without dissolving its own authority claim). Monogamous members sit nearer symmetric-low: genuine coordination benefit (legal security, statehood) with diffuse indirect costs. Pre-Manifesto plural households are trapped targets (high d — no exit that preserves their families) and post-Manifesto polygamists are constrained targets (high d — exit exists but costs them the marriages they believe were commanded). The federal government is authored as an excluded seat, not a beneficiary or victim: per the R3 ruling, an authored absence is commentary-grade and must not drive classification overrides, so no directionality override is authored for it and it rides the canonical fallback for its power atom. No other overrides are needed — the derivation from declared beneficiaries, victims, and exit options produces the correct directionalities for every seated party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — survive the federal assault without forfeiting prophetic authority — was resolved by roughly 1907: the practice ended, the property stood, statehood arrived. The arrangement nonetheless persists as permanent covenant rather than concluded crisis response, and the substitutionist framing is precisely the mechanism of that conversion: by recasting a survival measure as new revelation, the mandate is made unfalsifiable and permanent. Authoring founding_problem_status as dead alongside disappearance_verdict world_rearranges surfaces the mismatch the consumer cross-checks against the theater path — a zombie flag, not a settled verdict. Classifying as tangled_rope rather than snare preserves both halves of the truth: the coordination achievement was real (the community did survive, and most members are net beneficiaries), and the extraction is real (identifiable payers, concentrated gains, active enforcement). Reading the arrangement as pure revelation erases the payers; reading it as pure coercion erases the survival the coordination bought. The hybrid category is the honest seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_causal_genealogy,
    'Does the documentary record support predominantly revelatory or predominantly coercive causation for the 1890 shift?',
    'Archival work on the Woodruff papers, contemporaneous correspondence with federal officials, and congressional records; weigh internal revelatory accounts against the documented sequence of seizures, indictments, and legislative deadlines.',
    'A coercive-dominant record undermines the warrant of the substitutionist axiom and shifts analytic weight toward the coercion_visibility account of the same arrangement; a genuinely revelatory-dominant record stabilizes this reading''s legitimacy claim and lowers measured theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_causal_genealogy, empirical, 'Whether the Manifesto''s causal origin is revelation or coercion as the record shows it.').

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading of kernel divine_marriage_command — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Conceptual analysis of the three readings'' victim sets and epsilon referents: continuationist restores plural-marriage validity (victims become those prevented from practicing); coercion_visibility relocates the extraction source to the state-church interaction; substitutionist locates it in ecclesiastical enforcement of the new command.',
    'If the disagreement is located in the Manifesto''s ontological status rather than in any observable outcome, the three stories must remain separately classified files linked by network edges; merging them would produce an epsilon that varies with the reading adopted, violating epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the status of the 1890 act.').

omega_variable(
    enforcement_ratchet_driver,
    'Was the post-1904 enforcement hardening driven externally (Senate scrutiny of the Smoot seat, renewed federal pressure) or internally (institutional consolidation of the new doctrinal boundary)?',
    'Correlate the timing and intensity of disciplinary actions with external political events versus internal administrative milestones; examine whether hardening preceded or followed each round of federal scrutiny.',
    'An external driver supports tangled_rope stability (enforcement tracks a real settlement being defended); an internal driver after the external threat lapsed suggests snare-ward drift — enforcement persisting for its own sake.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_driver, empirical, 'Whether the suppression ratchet was externally compelled or internally owned.').

omega_variable(
    fundamentalist_exit_permeability,
    'Does the fundamentalist exit represent a genuine alternative that bounds the arrangement''s suppression, or a recaptured population that extends it?',
    'Track the disciplinary reach, retention pressure, and social/economic dependency of fundamentalist communities on the mainline institution over the interval; measure return migration and cross-community control.',
    'Genuine exit lowers effective suppression below the authored scalar and supports the moderate accessibility_collapse value; effective recapture raises suppression above it and pushes the arrangement toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_exit_permeability, empirical, 'Whether the fundamentalist settlements are exits or extensions of the enforcement perimeter.').

omega_variable(
    eternal_sealing_compensation,
    'Does the doctrine that plural sealings remain eternally valid while forbidden temporally function as compensating benefit for believing plural families, or as deferred-cost cover that lets the institution dissolve temporal marriages while owing nothing?',
    'Examine how the doctrine operated for affected families: whether it carried material or status compensation, or functioned solely to reconcile members to losses the institution declined to remedy.',
    'A genuine compensation reading lowers net extraction experienced by believing payers; a cover reading raises it, since the promise collects compliance now at zero present cost to the beneficiary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eternal_sealing_compensation, conceptual, 'Whether eternal-validation doctrine compensates the payers or defers their costs indefinitely.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t8, divine_marriage_command__substitutionist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(divi_tr_t14, divine_marriage_command__substitutionist_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(divi_tr_t27, divine_marriage_command__substitutionist_reading, theater_ratio, 27, 0.44).
narrative_ontology:measurement(divi_tr_t34, divine_marriage_command__substitutionist_reading, theater_ratio, 34, 0.42).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t8, divine_marriage_command__substitutionist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(divi_be_t14, divine_marriage_command__substitutionist_reading, base_extractiveness, 14, 0.6).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(divi_be_t27, divine_marriage_command__substitutionist_reading, base_extractiveness, 27, 0.62).
narrative_ontology:measurement(divi_be_t34, divine_marriage_command__substitutionist_reading, base_extractiveness, 34, 0.61).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(divi_su_t8, divine_marriage_command__substitutionist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(divi_su_t14, divine_marriage_command__substitutionist_reading, suppression_requirement, 14, 0.7).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(divi_su_t27, divine_marriage_command__substitutionist_reading, suppression_requirement, 27, 0.78).
narrative_ontology:measurement(divi_su_t34, divine_marriage_command__substitutionist_reading, suppression_requirement, 34, 0.77).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto' covers three structurally distinct claims about one 1890 act, decomposed per the epsilon-invariance principle into three readings of the kernel divine_marriage_command. This file instantiates the substitutionist reading (new superseding revelation; victims are post-Manifesto polygamists and unsanctioned plural households; epsilon authored from the reading's own covenantal lights). The continuationist sibling (prudential suspension) assigns a different victim set — those prevented from practicing a still-valid command — and the coercion_visibility sibling (acknowledged capitulation) relocates the extraction source to the state-church interaction. The substitutionist reading is the institutional descendant: its legitimacy requires suppressing the coercion account (influences edge) and logically excludes the suspension account (forecloses edge), since a command cannot be both rescinded by new revelation and merely paused.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
