% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Technological Mountain Causing Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The technological determinism reading of the press-Reformation causation
 *   kernel claims the printing press was an exogenous mountain — a
 *   technological constraint that made the Reformation inevitable by
 *   destroying the material possibility of effective censorship and making
 *   vernacular scripture a structural necessity. The press is not a tool
 *   reformers chose; it is a landscape they inherited. The Church's
 *   resistance was not merely overcome — it was structurally futile because
 *   the constraint (print technology) operates at a level where human
 *   enforcement cannot reach. This reading claims Mountain type: the press's
 *   causal power is a fixed feature of the historical physics of information,
 *   not a contingent social arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.75).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.2).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.75).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Technological Mountain Causing Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb').
narrative_ontology:cs_kernel_codification('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', distributed).
narrative_ontology:cs_authority_grounding('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', distributed).
narrative_ontology:cs_reading_relation('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', foundational, printing_press_as_exogenous_mountain).
narrative_ontology:cs_axiom_status(printing_press_as_exogenous_mountain, holdable).
narrative_ontology:cs_axiom_grounding('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', printing_press_as_exogenous_mountain, empirically_contingent).
narrative_ontology:cs_reference_frame('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', technological_determinist_frame).
narrative_ontology:cs_drift_state('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', contemporary_historiography, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1bed7c6a-18b5-4a91-a3ee-b686bdbd03eb', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_translators).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_censors).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, latin_ecclesiastical_authority).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_determines_social_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated the pre-print censorship apparatus (Index Librorum Prohibitorum, pre-publication licensing, inquisitorial networks). The press renders their gatekeeping structurally ineffective — vernacular texts proliferate beyond licensing reach, cross-border print networks bypass territorial bans. They respond with expanded Indexes and harsher penalties but cannot restore the pre-print information monopoly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_censors, payer,
    powerful, biographical, constrained, continental).

% Luther, Calvin, Zwingli and their networks exploit the press's exogenous capacity: pamphlets, vernacular Bibles, and polemics spread faster than any ecclesiastical response. They do not invent the press; they inherit a dissemination infrastructure that makes their theology unstoppable. Their 'exit' from Catholic orthodoxy is enabled by the press — without it, they remain local heretics easily suppressed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, reformers, beneficiary,
    organized, biographical, mobile, continental).

% Tyndale, Luther (as translator), Olivétan, and others produce vernacular scriptures that the press multiplies identically across regions. Manuscript translation was slow, error-prone, and suppressible; print makes vernacular scripture a mass commodity. They benefit from a technological capacity they did not create and cannot control.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_translators, beneficiary,
    moderate, biographical, mobile, continental).

% Operate the presses, choose which texts to print, and profit from both reformist and Catholic demand. They are not neutral conduits — they select, edit, and distribute. But their agenda-setting power derives from the press's mountain-like nature: once the technology exists, someone must print, and printers capture the rent. They can move between cities, play authorities against each other, and arbitrage regulatory gaps.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers, agenda_setter,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, printers, beneficiary).

% The papacy, episcopal hierarchy, and university theology faculties grounded in Latin textual authority. Their structural power depends on Latin as a controlled, clerical language. The press + vernacular translation destroys this monopoly. They are trapped in an institutional identity fused to Latin authority — they cannot 'exit' to vernacular legitimacy without dissolving their own rationale. The Council of Trent (1545-63) is a reactive institutional hardening, not an adaptation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, latin_ecclesiastical_authority, payer,
    institutional, generational, trapped, continental).

% Analyze the press-Reformation causal link from Eisenstein's 'unacknowledged revolution' to contemporary debates. They see the full structure: the press as mountain, the reformers as downstream beneficiaries, the Church as trapped payer. Their seat computes the constraint's type from outside the historical moment.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historians_of_reformation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved the coordination problem of mass dissemination of ideas across linguistic and political boundaries, enabling a distributed reform movement without central organization.
% TRANSFER_FUNCTION: Moves information control from centralized ecclesiastical censors to decentralized printers and vernacular translators; transfers interpretive authority from Latin-trained clergy to lay readers.
% ABSENT_VOICES: Peasant populations whose religious experience was mediated through both Latin liturgy and vernacular print; women excluded from both clerical and printing guild structures; Jewish and Muslim communities affected by Reformation's religious polarization but not consulted in the press's deployment.
% DISAPPEARANCE_RATIONALE: If the printing press had not existed or had been suppressed, the Reformation as a mass movement would not have occurred; censorship would have remained effective; vernacular scripture would have remained marginal; the Catholic Church would have retained unified doctrinal control.
% FOUNDING_PROBLEM: The problem of disseminating religious ideas beyond the control of centralized ecclesiastical censorship in a fragmented political landscape.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) 'The Printing Press as an Agent of Change' corroborates the dissemination function; Febvre & Martin (1958) 'The Coming of the Book' documents the press's role from outside the determinist thesis; modern historians (e.g., Pettegree, Johns) contest the determinist framing while acknowledging the press's material impact.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the press transfers massive informational control from Church to reformers/printers — a zero-sum shift in dissemination capacity. Suppression is low (0.20) because the press itself does not suppress; it dismantles suppression. The Church's counter-suppression (Index, Inquisition) is a response to the press, not the press's own operation. Theater ratio is low (0.15) — the press's function is real dissemination, not performance. Accessibility collapse is very high (0.85) — manuscript-based censorship alternatives collapse completely once print networks establish. Resistance is low (0.15) — per this reading, Church resistance was structurally doomed, not a genuine contest. The measurement series shows extractiveness rising as print networks mature (1450-1550), then plateauing.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (censors, Latin authority) experience the press as a snare — an extractive constraint they cannot escape. The beneficiary seats (reformers, translators) experience it as a rope — a coordination gift that solves their dissemination problem. The agenda_setter seat (printers) experiences it as a mountain they administer for profit. The observer seat sees the press as a mountain that restructures the entire field. The engine computes this divergence from the structural data; the claimed_type (mountain) is the reading's own claim about the press's nature, not a reconciliation of seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The press as mountain has no directionality itself — it is the fixed landscape. Reformers and translators are beneficiaries (d ~ 0.1-0.2): they receive exogenous capacity they did not build. Printers are agenda_setters with arbitrage exit (d ~ 0.3): they operate the mountain and capture rent, but can move. Catholic censors are payers with constrained exit (d ~ 0.8): they lose their gatekeeping function and cannot restore it. Latin ecclesiastical authority is trapped (d ~ 0.95): their institutional identity is fused to the Latin monopoly the press destroys. Historians are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The press as mountain has no mandate — it is not an institution with a purpose that atrophied. The mandatrophy question applies to the Church's censorship apparatus, which persists (Index, Inquisition) after its function (effective control) is destroyed by the mountain. That apparatus is a piton: theatrical maintenance of a lost function. This reading distinguishes the mountain (press) from the piton (censorship bureaucracy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (technological_determinism) of the contested kernel press_reformation_causation. What structural elements distinguish it from the strategic_deployment and mutual_shaping readings?',
    'Comparative constraint story generation for each reading; engine computes per-reading classifications from authored structural data. The kernel''s committer-axis divergence is measured by cross-reading metric and axiom comparison.',
    'If strategic_deployment computes as rope/tangled_rope and mutual_shaping as tangled_rope, while this reading computes as mountain, the kernel''s classificatory spread is the measurement of historiographical contestation. If all three compute similarly, the kernel label masks a false disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel reading identity and structural differentiation from sibling readings.').

omega_variable(
    suppression_mechanism_church_censors,
    'Is the Catholic Church''s loss of censorship effectiveness structural (print technology physically bypasses controls) or internalized (censors believed control was lost and reduced effort)?',
    'Counterfactual: if the Church had maintained pre-print enforcement intensity (expanded Inquisition, total border control of books), would censorship have remained effective? Historical evidence from Spanish/Portuguese strict control vs. German/Italian permeability tests this.',
    'If structural, the press is a genuine mountain with low suppression (it dismantles suppression). If partially internalized, the Church''s surrender amplifies the press''s effective extraction — the mountain''s power is co-produced by the payer''s response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_church_censors, empirical, 'Structural vs. internalized mechanism of the Church''s censorship collapse.').

omega_variable(
    technological_determinism_vs_agency,
    'Does the press''s mountain-like causal power require reformer agency to activate, or does it operate autonomously once instantiated?',
    'Compare regions with presses but no reformers (e.g., early Italian/Spanish printing) vs. regions with reformers but delayed press access. If Reformation only occurs where both coincide, agency is necessary condition; if press alone suffices, mountain autonomy holds.',
    'If agency necessary, the press is a necessary-but-not-sufficient mountain (still mountain, but with coordination gate). If autonomous, pure mountain. This affects whether reformers are beneficiaries of a mountain or coordinators of a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Whether the printing press as mountain requires human agency to exert its determinism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_td_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(press_reformation_td_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.08).
narrative_ontology:measurement(press_reformation_td_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(press_reformation_td_tr_t1517, press_reformation_causation__technological_determinism, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(press_reformation_td_tr_t1530, press_reformation_causation__technological_determinism, theater_ratio, 1530, 0.14).
narrative_ontology:measurement(press_reformation_td_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(press_reformation_td_tr_t1600, press_reformation_causation__technological_determinism, theater_ratio, 1600, 0.15).

% Extraction over time
narrative_ontology:measurement(press_reformation_td_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(press_reformation_td_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.25).
narrative_ontology:measurement(press_reformation_td_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(press_reformation_td_be_t1517, press_reformation_causation__technological_determinism, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(press_reformation_td_be_t1530, press_reformation_causation__technological_determinism, base_extractiveness, 1530, 0.72).
narrative_ontology:measurement(press_reformation_td_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.75).
narrative_ontology:measurement(press_reformation_td_be_t1600, press_reformation_causation__technological_determinism, base_extractiveness, 1600, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_td_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(press_reformation_td_su_t1470, press_reformation_causation__technological_determinism, suppression_requirement, 1470, 0.1).
narrative_ontology:measurement(press_reformation_td_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement(press_reformation_td_su_t1517, press_reformation_causation__technological_determinism, suppression_requirement, 1517, 0.18).
narrative_ontology:measurement(press_reformation_td_su_t1530, press_reformation_causation__technological_determinism, suppression_requirement, 1530, 0.2).
narrative_ontology:measurement(press_reformation_td_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.2).
narrative_ontology:measurement(press_reformation_td_su_t1600, press_reformation_causation__technological_determinism, suppression_requirement, 1600, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.02).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, vernacular_literacy_spread).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, catholic_counter_reformation_institutionalization).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, early_modern_public_sphere_formation).

% DUAL FORMULATION NOTE:
% This constraint (technological_determinism) is one of three readings of the press_reformation_causation kernel. The strategic_deployment reading would claim lower extractiveness (reformers deploy neutral tool) and higher suppression (Church resists effectively for decades). The mutual_shaping reading would claim bidirectional extraction (press shapes reformers, reformers shape press markets). All three share the same historical referent but author different ε and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__technological_determinism, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
