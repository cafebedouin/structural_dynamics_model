% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Expansionist-Legalist Jihad Doctrine (Siyar Framework)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story instantiates the expansionist-legalist reading of the jihad
 *   kernel: the classical siyar doctrine developed primarily by Hanafi,
 *   Shafi'i, and other jurists holding that jihad includes a standing
 *   obligation to extend Islamic political order into territory not governed
 *   by Islamic law, subject to procedural conditions (formal invitation to
 *   Islam or submission, exclusive declaration authority vested in the
 *   imam/caliph, proportionality, and rules for captive and land treatment).
 *   This is a distinct constraint from the defensive-spiritual reading (which
 *   denies any offensive obligation and centers internal struggle) and the
 *   revolutionary-vanguard reading (which relocates authority to individuals
 *   via takfir rather than the state). Each reading has a different ε because
 *   each names a different beneficiary/victim structure and a different locus
 *   of authority: this reading's ε reflects a rule-bound but
 *   state-monopolized extraction machine, not a defensive posture and not an
 *   anti-authority insurgent posture.
 *
 * KEY AGENTS:
 *   - caliphal_state: agenda-setter and primary institutional beneficiary — holds exclusive declaration authority and collects tribute/land
 *   - classical_jurist_class: co-agenda-setter — defines the procedural conditions that legitimate campaigns, gains patronage and interpretive authority
 *   - military_commander_class: beneficiary — executes campaigns, collects spoils
 *   - conquered_non_muslim_populations and dhimmi_subjects: primary victims — bear conquest, tribute, and permanent subordinate legal status
 *   - comparative_religious_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.58).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.62).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Expansionist-Legalist Jihad Doctrine (Siyar Framework)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'ca65ad8b-4efb-4238-a42b-66d00b662804').
narrative_ontology:cs_kernel_codification('ca65ad8b-4efb-4238-a42b-66d00b662804', fixed_text).
narrative_ontology:cs_authority_grounding('ca65ad8b-4efb-4238-a42b-66d00b662804', lineage).
narrative_ontology:cs_interpretation_layer_present('ca65ad8b-4efb-4238-a42b-66d00b662804').
narrative_ontology:cs_reading_relation('ca65ad8b-4efb-4238-a42b-66d00b662804', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca65ad8b-4efb-4238-a42b-66d00b662804', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('ca65ad8b-4efb-4238-a42b-66d00b662804', foundational, offensive_jihad_conditionally_licit_under_state_authority).
narrative_ontology:cs_axiom_status(offensive_jihad_conditionally_licit_under_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('ca65ad8b-4efb-4238-a42b-66d00b662804', offensive_jihad_conditionally_licit_under_state_authority, conventional).
narrative_ontology:cs_axiom('ca65ad8b-4efb-4238-a42b-66d00b662804', foundational, declaration_authority_vested_exclusively_in_recognized_imam).
narrative_ontology:cs_axiom_status(declaration_authority_vested_exclusively_in_recognized_imam, holdable).
narrative_ontology:cs_axiom_grounding('ca65ad8b-4efb-4238-a42b-66d00b662804', declaration_authority_vested_exclusively_in_recognized_imam, conventional).
narrative_ontology:cs_reference_frame('ca65ad8b-4efb-4238-a42b-66d00b662804', classical_siyar_dar_al_islam_expansion_framework).
narrative_ontology:cs_drift_state('ca65ad8b-4efb-4238-a42b-66d00b662804', post_westphalian_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca65ad8b-4efb-4238-a42b-66d00b662804', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, military_commander_class).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, converted_administrative_elites).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_subjects).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, border_populations_under_ongoing_raids).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_conscripted_soldiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_conscripted_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the exclusive jurisprudential authority (imam/caliph monopoly) to declare offensive jihad, organize campaigns, negotiate dhimmi terms, and distribute conquered land and tribute. The legal conditions (invitation first, proportionality) function as procedural legitimation for expansion the state initiates and controls.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Develops and administers the siyar rules — invitation requirements, proportionality doctrine, treatment-of-captives law — that make offensive campaigns religiously licit. Their interpretive authority over what counts as valid jihad is itself a source of social status and patronage from the state they legitimate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class, beneficiary).

% Leads campaigns authorized under the doctrine, receiving a share of ghanima (spoils) and land grants upon conquest. The jurisprudential conditions provide the legal cover under which their expansion campaigns proceed; adherence to procedure is variably enforced in practice.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, military_commander_class, beneficiary,
    powerful, biographical, mobile, continental).

% Local elites in conquered territories who convert or collaborate gain administrative posts, tax exemptions, and social mobility unavailable to those who remain dhimmi. Their position depends on the conquest-and-conversion pipeline the doctrine authorizes continuing.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, converted_administrative_elites, beneficiary,
    organized, generational, constrained, regional).

% Populations outside dar al-islam who receive the invitation to convert or submit; refusal after the formal invitation renders them licit targets of campaign under the doctrine's own procedural logic. Their only sanctioned outcomes are conversion, submission as dhimmi, or war — the invitation requirement formalizes rather than removes the underlying threat.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations, payer,
    powerless, generational, trapped, regional).

% Non-Muslims who submit and pay jizya in exchange for protection and non-interference in religious practice, but bear permanent second-class legal status, restrictions on testimony, dress, and worship, and periodic renegotiation of terms at the discretion of the ruling authority. Exit means conversion (loss of prior identity) or flight.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_subjects, payer,
    powerless, generational, trapped, regional).

% Communities on the frontier subject to recurring seasonal campaigns (ghazw) that predate and postdate any formal declaration, experiencing the doctrine's proportionality and invitation conditions as intermittently observed rather than reliably protective.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, border_populations_under_ongoing_raids, payer,
    powerless, biographical, trapped, regional).

% Foot soldiers who bear the physical risk of campaigns and receive a much smaller share of any spoils than commanders; participation is framed as religious obligation, making refusal carry both material and spiritual cost.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_conscripted_soldiers, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_conscripted_soldiers, beneficiary).

% Contemporary jurists arguing the offensive-jihad rulings were contingent on a specific historical balance of power (dar al-harb/dar al-islam) that no longer obtains, and that the doctrine should be read as historically bounded rather than perpetually operative. Their reinterpretation competes with, but does not displace, the classical legalist reading in traditionalist institutions.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, modern_reformist_scholars, excluded,
    moderate, generational, constrained, global).

% Study the doctrine's textual sources, historical application, and jurisprudential evolution across schools (Hanafi, Shafi'i, Maliki, Hanbali) without adjudicating its theological validity, documenting both the procedural constraints jurists imposed and their inconsistent historical enforcement.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_religious_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-bound legal framework distinguishing licit from illicit warfare within a specific tradition: requiring formal invitation before attack, vesting declaration authority in a single recognized authority rather than any armed group, and imposing proportionality and captive-treatment rules — a real constraint on unregulated raiding compared to the alternative of unbounded conquest.
% TRANSFER_FUNCTION: Moves territory, tribute (jizya, kharaj), captives, and administrative control from populations outside the Islamic polity to the caliphal state, its military commanders, and the jurist class that legitimates the campaigns; moves permanent legal subordination onto populations who submit rather than convert.
% ABSENT_VOICES: Conquered and dhimmi populations, and the border communities subject to recurring raids, have no seat in the juristic process that defines when invitation was adequately extended or when proportionality was honored — those determinations are made entirely by the authority conducting the campaign. Modern reformist scholars arguing for historical contingency are present in discourse but hold no institutional authority to bind traditionalist practice.
% DISAPPEARANCE_RATIONALE: If this doctrine's authority vanished, caliphal and successor states would lose their primary religious-legal justification for offensive campaigns and tribute extraction from conquered non-Muslims; the jurist class administering siyar law would lose a domain of interpretive authority and patronage; conquered and dhimmi populations' legal status would have to be renegotiated from different first principles entirely.
% FOUNDING_PROBLEM: Early Muslim community needed a legal framework distinguishing legitimate warfare in the service of establishing and expanding Islamic political order from unregulated tribal raiding, and needed to resolve internal disputes over who held authority to initiate war and how conquered populations would be governed.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists and caliphal chroniclers (interested parties) attest the doctrine remained necessary and operative throughout the classical and medieval period. Independent historians of Islamic law and comparative religion, along with reformist scholars operating outside state patronage, attest that the dar al-harb/dar al-islam balance-of-power condition the offensive doctrine presupposed no longer obtains in a world of sovereign nation-states and international law, making the doctrine's continued invocation by some modern actors a repurposing rather than a live application of its founding logic.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58) rather than extreme because the doctrine's procedural conditions (invitation, proportionality, captive treatment rules) are genuine constraints relative to unregulated conquest — they are not costless theater, and historically constrained some commanders' conduct. Suppression is authored moderate (0.62) reflecting that submission, conversion, or war are the doctrine's own sanctioned outcomes for those outside dar al-islam — refusal after invitation is treated as licit grounds for war, which is a structural form of coercion built into the legal framework itself, not merely its abuse. Theater ratio rises across the interval (0.2 to 0.4) reflecting later periods where the invitation and proportionality requirements became increasingly formal/pro forma relative to state realpolitik, particularly in later imperial contexts where campaigns preceded any genuine communicative attempt.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal-state and jurist seats, the doctrine is legitimate rule-governed coordination establishing licit political order and restraining unregulated violence. From the conquered-population and dhimmi seats, the same procedural apparatus (invitation, proportionality) operates as legal cover for conquest whose outcome — submission, conversion, or war — was never genuinely open to them. The engine should compute divergent per-seat classifications from these structural facts without either seat's perspective being treated as more authoritative than the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal state and jurist class sit near the full-beneficiary end: they set the rules, control interpretation, and capture the gains (territory, tribute, patronage, interpretive authority). Conquered populations and dhimmi subjects sit near the full-target end: trapped exit options, generational time horizon of subordination, and no voice in determining when procedural conditions were satisfied. Rank-and-file soldiers are a genuine hybrid — bearing risk under religious obligation while receiving a much smaller share of gains than commanders, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing licit war-for-political-order from unregulated raiding, resolving internal authority disputes) was live in the early conquest period when unified command over disparate tribal forces was a genuine coordination need. Its status is contested rather than flatly dead because state actors and traditionalist jurists still invoke it as operative doctrine, while comparative legal historians and reformist scholars argue the dar al-harb/dar al-islam condition presupposed by the offensive obligation dissolved with the end of that specific interstate configuration. The tangled_rope classification is deliberately not tuned toward snare: the coordination function (rule-bound versus unbound warfare, a real distinction with real historical effect on captive treatment and proportionality) is genuine even though the beneficiary/victim asymmetry is severe — collapsing this into pure snare would erase the doctrine's actual constraining function on state violence, which the framework's tangled_rope category exists to preserve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_contingency_of_offensive_obligation,
    'Is the offensive-jihad obligation in this reading a perpetually binding legal norm, or was it contingent on a specific historical balance of power (dar al-harb/dar al-islam) that has since dissolved?',
    'Comparative analysis of classical jurisprudential texts against their historical context of production, cross-referenced with modern reformist jurisprudence arguing for context-bound interpretation versus traditionalist jurisprudence maintaining perpetual applicability.',
    'If the obligation is genuinely contingent and the presupposed conditions no longer obtain, invocation of this doctrine by contemporary actors is a repurposing of a historically bounded ruling rather than continuation of a live legal obligation — which would sharply reduce the doctrine''s legitimate present-tense scope while leaving its historical operation as described.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_contingency_of_offensive_obligation, conceptual, 'Whether the offensive obligation is perpetually binding or historically bounded.').

omega_variable(
    procedural_conditions_genuine_constraint_or_legitimation_theater,
    'Did the invitation, imam-authority, and proportionality requirements function as genuine constraints on state violence, or primarily as post-hoc legitimation for campaigns that would have occurred regardless?',
    'Historical case analysis of documented campaigns checking whether invitation was extended with genuine time for response and whether campaigns were altered or aborted when conditions were not met, versus cases where the requirements were satisfied only formally after the fact.',
    'If largely genuine constraint, the coordination function underlying the tangled_rope classification is stronger than the theater_ratio trajectory suggests. If largely post-hoc legitimation, the doctrine tilts further toward snare and the rising theater_ratio understates the drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_conditions_genuine_constraint_or_legitimation_theater, empirical, 'Whether procedural conditions constrained conduct or merely legitimated predetermined campaigns.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does this reading''s boundary sit relative to the defensive_spiritual_reading — specifically, do classical jurists who accept some offensive obligation but restrict its scope to restoring lapsed treaty obligations belong in this reading or the defensive one?',
    'Textual mapping of specific fiqh rulings (e.g., Shafi''i vs. Hanafi positions on initiating war absent prior aggression) against the two readings'' core premises to identify where individual jurists or schools sit.',
    'A cleaner boundary would sharpen which historical jurists and texts belong to this constraint''s stakeholder set versus the sibling defensive reading''s set; an unresolvable boundary suggests some jurisprudential positions are genuinely intermediate and may warrant a fourth reading rather than forced assignment to one of the three declared readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Ambiguity in where certain classical positions fall between the expansionist and defensive readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the jihad_quranic_corpus kernel. defensive_spiritual_reading denies any standing offensive obligation and centers jihad al-nafs plus proportionate defensive response — its beneficiary/victim structure and ε are expected to be substantially lower-extraction than this reading. revolutionary_vanguard_reading relocates declaration authority away from the state entirely, to individuals via takfir and emergency jurisprudence against rulers deemed apostate — its stakeholder set (targeting incumbent Muslim rulers rather than external non-Muslim populations) and its suppression/theater profile differ structurally from this reading's state-monopolized, externally-directed conquest model. This reading (expansionist_legalist) sits between the two: it shares the defensive reading's procedural rule-boundedness but shares the revolutionary reading's willingness to license offensive violence, differing from it chiefly in WHO holds declaration authority (state/imam here, individual/vanguard there).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
