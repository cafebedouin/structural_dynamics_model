% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Study-as-Archiving Reading of the Temple Sacrifice Obligation
 *   domain: religious/halakhic authority
 *
 * SUMMARY:
 *   This story instantiates the study_as_archiving reading of the contested
 *   temple_sacrifice_obligation kernel. On this reading, the obligation to
 *   offer sacrifices remains fully binding, but since the Temple's
 *   destruction it cannot be performed; intensive study of the sacrificial
 *   order (Seder Kodashim) is held to preserve the knowledge necessary for
 *   eventual restoration, but study is explicitly NOT treated as fulfilling
 *   the obligation itself. The entire post-Temple period is therefore, on
 *   this reading's own terms, a period of ongoing non-compliance with a
 *   divine command — softened but not resolved by the archiving function.
 *   This is a distinct constraint from study_as_occupation (which holds that
 *   study DOES close the compliance gap) and from messianic_suspension (which
 *   holds there is no ongoing gap to close because the obligation is dormant,
 *   not violated). Each reading has a different victim set and a different ε:
 *   this reading's ε sits at a moderate 0.47 because study provides real,
 *   partial relief (a genuine coordination/preservation function) while an
 *   authentic accounting gap remains open and unaddressed for two millennia.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: institutional agenda-setters who maintain the binding status of an unperformable law
 *   - rabbinic_academies: organized beneficiaries whose prestige and curricular centrality derive from the archiving function
 *   - ordinary_observant_laity: powerless payers who carry the liturgical and psychological weight of unresolved non-compliance without access to the scholarly substitution
 *   - the_unfulfilled_divine_command: the non-agent structural victim in the reading's own terms
 *   - comparative_religion_scholars: analytical observers of how legal systems sustain binding status across performability ruptures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.47).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.47).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Study-as-Archiving Reading of the Temple Sacrifice Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic authority").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '838b7101-92e4-4c6a-a19d-264fcbf75a03').
narrative_ontology:cs_kernel_codification('838b7101-92e4-4c6a-a19d-264fcbf75a03', fixed_text).
narrative_ontology:cs_authority_grounding('838b7101-92e4-4c6a-a19d-264fcbf75a03', lineage).
narrative_ontology:cs_interpretation_layer_present('838b7101-92e4-4c6a-a19d-264fcbf75a03').
narrative_ontology:cs_reading_relation('838b7101-92e4-4c6a-a19d-264fcbf75a03', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('838b7101-92e4-4c6a-a19d-264fcbf75a03', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('838b7101-92e4-4c6a-a19d-264fcbf75a03', foundational, study_preserves_but_does_not_fulfill).
narrative_ontology:cs_axiom_status(study_preserves_but_does_not_fulfill, holdable).
narrative_ontology:cs_axiom_grounding('838b7101-92e4-4c6a-a19d-264fcbf75a03', study_preserves_but_does_not_fulfill, conventional).
narrative_ontology:cs_axiom('838b7101-92e4-4c6a-a19d-264fcbf75a03', foundational, obligation_remains_actively_binding_despite_impossibility).
narrative_ontology:cs_axiom_status(obligation_remains_actively_binding_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('838b7101-92e4-4c6a-a19d-264fcbf75a03', obligation_remains_actively_binding_despite_impossibility, deontological).
narrative_ontology:cs_reference_frame('838b7101-92e4-4c6a-a19d-264fcbf75a03', temple_era_direct_performance).
narrative_ontology:cs_drift_state('838b7101-92e4-4c6a-a19d-264fcbf75a03', post_destruction_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('838b7101-92e4-4c6a-a19d-264fcbf75a03', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_academies).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, communal_continuity_of_tradition).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, the_unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, ordinary_observant_laity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, perpetual_bindingness_of_torah_law).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, study_equals_preservation_not_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule that the obligation to bring sacrifices remains fully binding in principle even though it cannot be performed without a Temple, and that detailed study of the sacrificial order (Seder Kodashim, laws of the Temple service) is the correct mode of engagement in the interim. They set curricula, adjudicate disputes about the status of the law, and their authority is reinforced by being the sole legitimate interpreters of what counts as adequate engagement with an unperformable command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Institutions of Torah study gain a permanent, prestige-bearing curriculum item — the sacrificial order is one of the six orders of the Mishnah and its study is treated as equivalent in merit to performing the sacrifices themselves. This generates ongoing scholarly output, teaching positions, and institutional legitimacy that would not exist if the obligation were simply declared void or suspended.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_academies, beneficiary,
    organized, civilizational, mobile, global).

% Most laypeople lack the textual training to engage in the technical study of Kodashim in a way that halakhic authorities recognize as substitutive. They are told the obligation persists and that study substitutes for performance, but the substitution is calibrated to a scholarly elite's capacities, leaving most people formally bound by a command they cannot fulfill or meaningfully approximate. They carry the psychological and liturgical weight (recited prayers for restoration, fast days over the Temple's destruction) without access to the study-based resolution offered to the learned.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, ordinary_observant_laity, payer,
    powerless, biographical, constrained, national).

% The commandment to offer sacrifices, as a matter of the reading's own internal logic, is not being performed and has not been performed for roughly two millennia. Listed here for structural completeness: it is the entity that 'bears the cost' of non-fulfillment in the reading's own terms, distinct from any human actor, and is not itself an agent capable of benefiting or exiting.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, the_unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, the_unfulfilled_divine_command).

% Hold the sibling view that study does not merely archive but actively occupies and legitimately substitutes for the obligation, closing the compliance gap rather than merely preserving knowledge of it. This reading is not adopted within the archiving framework and its proponents' view that the gap is already closed does not enter this reading's own accounting of ongoing non-compliance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, occupation_reading_proponents, excluded,
    organized, civilizational, constrained, global).

% Hold that the obligation is suspended entirely pending restoration, so there is no ongoing violation to archive against — only a dormant state. This reading is not adopted here; the archiving framework's insistence that non-fulfillment continues to accrue (even while unperformable) is precisely what the suspension reading denies.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_suspension_proponents, excluded,
    organized, civilizational, constrained, global).

% Study how legal systems maintain binding status for commandments that are structurally impossible to perform, and how study-substitution doctrines function to preserve institutional authority and continuity of practice across ruptures like the Temple's destruction.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of the sacrificial order across a two-thousand-year discontinuity, so that if a Temple were rebuilt, the practice could in principle be resumed correctly rather than reconstructed from fragments. This is a genuine transmission/archiving function.
% TRANSFER_FUNCTION: Moves prestige, curricular centrality, and institutional legitimacy to scholars and academies capable of the technical study, while the formal weight of an unfulfilled divine obligation — its liturgical mourning, its guilt-adjacent unresolved status — is distributed onto ordinary observant people who cannot access the study-based partial resolution.
% ABSENT_VOICES: Proponents of the study-as-occupation reading (who would say the compliance gap is already closed by study itself) and proponents of messianic suspension (who would say there is no ongoing gap to archive against) are structurally excluded from this reading's own accounting — the archiving reading's coherence depends on neither of those resolutions being adopted.
% DISAPPEARANCE_RATIONALE: If the archiving doctrine were dropped, halakhic authorities disagree on the consequence: some hold the obligation would revert to unmediated non-compliance with no interim mode of engagement at all (a rearrangement of liturgical and educational practice), others (adjacent to the occupation reading) hold that abandoning archiving in favor of occupation would functionally close the compliance gap rather than leave a vacuum. The verdict depends on which sibling reading absorbs the space vacated.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments became physically unperformable, threatening to either delegitimize the law as a whole or require declaring an entire category of Torah obligation void. The founding problem was how to keep the law's bindingness intact and its knowledge transmissible without a Temple.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (e.g. statements attributing merit to Torah study of sacrificial law as if the sacrifice were offered) attest the problem was live and required a resolution at the time of composition. Modern historians of Jewish law and some non-Orthodox denominational authorities, external to the beneficiary academies, attest that the founding problem (an unperformable commandment threatening legal coherence) has been effectively resolved by two millennia of stable practice under the archiving doctrine, and that continued insistence on the obligation's live bindingness now serves institutional and identity-maintenance functions more than it manages any residual crisis of legal coherence.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.47, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.47 (not high) because the archiving function is not purely extractive theater — it performs genuine knowledge-preservation work that would matter if restoration ever occurred, and it does provide partial psychological/spiritual accommodation to a genuinely difficult legal situation. But it is not zero because, unlike the messianic_suspension reading, this reading insists the obligation remains actively binding and thus actively unfulfilled, generating a persistent liturgical debt that the study function does not discharge — that undischarged debt is the extraction, concentrated on those (the laity) who cannot access the study-based mode of engagement. Theater ratio rises modestly over the interval (0.20 to 0.42) reflecting an increasing share of communal energy going to symbolic/commemorative practice (fast days, liturgical mourning) relative to the study's original preservationist function as living memory of Temple practice recedes further into the past. Suppression is moderate-high (0.58) because the authority to declare study 'sufficient engagement but not fulfillment' is centrally held and not open to lay reinterpretation — an individual cannot unilaterally decide the obligation is fulfilled, suspended, or void; only recognized halakhic authority adjudicates the category.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities and rabbinic academies sit near the beneficiary end: they administer the archiving doctrine, derive institutional and scholarly legitimacy from it, and are not themselves burdened by inability to perform sacrifices (their status does not depend on the Temple's restoration). Ordinary observant laity sit near the target end: they are told the obligation binds them, but the archiving resolution is calibrated to scholarly capacity they typically lack, so they carry the unresolved liturgical weight most directly and have essentially no exit (leaving observance is the only real exit, at high identity cost, hence 'constrained' rather than 'trapped'). The unfulfilled divine command, authored as a non-agent, is placed at the extreme target end structurally but does not participate in the directionality computation as a real actor per the schema's agent-hood gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The archiving reading resists a simple mandatrophy verdict because the founding problem (an unperformable commandment threatening the coherence of the whole legal system) is genuinely contested as to whether it is still live: from inside the tradition it remains live because restoration is a standing eschatological hope, not merely a wish, and the law's bindingness is prior to and independent of feasibility. From outside, two millennia of stable, non-catastrophic practice under the archiving doctrine looks like evidence the original crisis has been successfully and permanently managed by the very mechanism that would, on a strict mandatrophy read, be declared to have outlived its function. This story deliberately declares founding_problem_status as contested rather than dead, because collapsing that contest into either a clean live/dead verdict would itself be adopting the occupation or a crypto-suspension reading rather than reporting the archiving reading honestly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_occupation_boundary,
    'Is there a principled, non-question-begging line between study that merely ''preserves knowledge for restoration'' (archiving) and study that ''legitimately occupies the obligation'' (occupation), or is the distinction itself a contested theological choice with no independent fact of the matter?',
    'Compare classical rabbinic sources (e.g., statements in tractates Menachot and Taanit about Torah study ''counting as if'' sacrifices were offered) for internal consistency: do they consistently treat study as substitutive-in-merit-only versus substitutive-in-fulfillment? Textual and historical analysis of how successive authorities have drawn or blurred this line would surface whether it is a stable structural distinction or a shifting rhetorical one.',
    'If no principled line exists, the archiving and occupation readings are not two genuine positions but one position described two ways for different rhetorical purposes — which would mean this story''s ε and the occupation story''s ε should converge rather than diverge, undermining the decomposition into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_occupation_boundary, conceptual, 'Whether archiving and occupation are structurally distinct readings or the same position under different framing.').

omega_variable(
    founding_problem_liveness,
    'Is the founding problem (coherence of a legal system containing an unperformable commandment) still live, or has it been permanently and successfully resolved by the archiving doctrine''s stable two-millennium operation, making continued insistence on active bindingness a legacy commitment rather than a response to ongoing crisis?',
    'Examine whether halakhic authorities today treat the sacrificial laws'' bindingness as generating any practical legal consequences beyond study obligations and liturgical practice (e.g., does it affect any other area of law, inheritance, communal obligation) — a purely notional bindingness with zero downstream legal effect would support the ''permanently resolved, now legacy'' reading.',
    'If resolved, the archiving doctrine functions closer to a piton (inertial, mostly performative maintenance of a settled matter) than an active tangled_rope; if genuinely live (restoration remains a structurally anticipated event with real, if deferred, legal consequence), the tangled_rope classification with ongoing extraction is the more accurate read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness, conceptual, 'Whether the archiving doctrine still manages a live crisis or has become inertial legacy maintenance.').

omega_variable(
    lay_access_asymmetry,
    'Is the asymmetry between scholarly and lay capacity to engage in the study-based resolution a designed feature that concentrates institutional prestige, or an incidental consequence of the technical difficulty of the material with no extractive intent?',
    'Compare accessibility of Seder Kodashim study relative to other, more widely-taught areas of Jewish law and practice; examine whether outreach or simplified-access programs for lay engagement with Kodashim exist and are promoted at parity with elite yeshiva curricula.',
    'If accessibility could readily be increased but is not, the concentration of the archiving resolution''s benefit among scholars looks more like extraction riding on a genuine coordination function; if the technical difficulty is irreducible, the asymmetry is closer to an unavoidable cost of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_access_asymmetry, empirical, 'Whether unequal lay access to the study-resolution is designed or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.2).
narrative_ontology:measurement(temp_tr_t325, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 325, 0.25).
narrative_ontology:measurement(temp_tr_t650, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 650, 0.3).
narrative_ontology:measurement(temp_tr_t975, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 975, 0.33).
narrative_ontology:measurement(temp_tr_t1300, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1300, 0.37).
narrative_ontology:measurement(temp_tr_t1625, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1625, 0.4).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1950, 0.42).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temp_be_t325, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(temp_be_t650, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 650, 0.4).
narrative_ontology:measurement(temp_be_t975, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 975, 0.42).
narrative_ontology:measurement(temp_be_t1300, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1300, 0.44).
narrative_ontology:measurement(temp_be_t1625, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1625, 0.46).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1950, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(temp_su_t325, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 325, 0.48).
narrative_ontology:measurement(temp_su_t650, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 650, 0.5).
narrative_ontology:measurement(temp_su_t975, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 975, 0.53).
narrative_ontology:measurement(temp_su_t1300, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1300, 0.55).
narrative_ontology:measurement(temp_su_t1625, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1625, 0.57).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial single label 'the obligation to offer sacrifices after the Temple's destruction' per the ε-invariance principle. study_as_occupation treats study as closing the compliance gap (lower ε than this story). messianic_suspension treats the obligation as dormant rather than violated (near-zero ε). This story (study_as_archiving) sits between them: real partial relief via preservation, but an acknowledged, undischarged compliance gap, yielding moderate ε (0.47). All three are linked bidirectionally in their respective network.affects_constraints arrays; each authors independent metrics, beneficiaries, and victims rather than a shared or averaged value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
