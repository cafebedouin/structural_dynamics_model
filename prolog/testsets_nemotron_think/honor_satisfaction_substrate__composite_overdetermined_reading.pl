% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The honor satisfaction substrate — the system of codes, seconds,
 *   challenges, and duels that regulated aristocratic and military dispute
 *   resolution in early modern Europe — declined between roughly 1650-1900.
 *   This reading (composite_overdetermined) holds that the decline was
 *   overdetermined: exogenous legal/institutional suppression
 *   (criminalization, military prohibition, state monopoly on violence) AND
 *   endogenous honor code transformation (the shift from 'cultures of honor'
 *   to 'cultures of dignity', the bourgeois redefinition of honor as internal
 *   rather than external) operated simultaneously with non-independent causal
 *   pathways. Legal suppression did not merely crush a static institution; it
 *   interacted with a substrate already undergoing semantic and normative
 *   mutation. Honor courts (Ehrengerichte) both enforced the code and rewrote
 *   it. The constraint functioned as a tangled_rope: genuine coordination
 *   (dispute resolution without state courts) hybridized with asymmetric
 *   extraction (aristocratic status maintenance, officer corps cohesion at
 *   the expense of enlisted men, gendered honor economies). Its disappearance
 *   exhibits both rope-breaking (coordination collapse under legal pressure)
 *   AND mountain erosion (the honor substrate's perceived naturalness
 *   dissolving).
 *
 * KEY AGENTS:
 *   - aristocracy: Primary agenda_setter and beneficiary (institutional/identity_locked) — set codes, benefited from status monopoly
 *   - military_officer_corps: Beneficiary and secondary agenda_setter (organized/identity_locked) — honor central to professional identity and command authority
 *   - honor_custodians: Agenda_setter (organized/constrained) — seconds, code authors, court of honor members who administered the system
 *   - duel_participants: Payer (moderate/identity_locked) — bore mortality risk and legal jeopardy; exit blocked by identity fusion
 *   - lower_status_individuals: Payer and excluded (powerless/trapped) — subject to honor violence without access to satisfaction; no voice in codes
 *   - women_in_honor_economy: Payer and excluded (powerless/identity_locked) — honor currency traded through them (sexual reputation, marriage market); no direct participation
 *   - state_legal_authorities: Observer becoming agenda_setter (institutional/analytical → institutional/arbitrage) — initially tolerated, then suppressed, then replaced with state courts
 *   - bourgeoisie: Excluded becoming observer (organized/constrained → organized/analytical) — outside honor culture but competed for status; later authored dignity culture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24').
narrative_ontology:cs_kernel_codification('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', formalized).
narrative_ontology:cs_authority_grounding('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', practice).
narrative_ontology:cs_interpretation_layer_present('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24').
narrative_ontology:cs_reading_relation('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', foundational, decline_was_overdetermined_and_entangled).
narrative_ontology:cs_axiom_status(decline_was_overdetermined_and_entangled, holdable).
narrative_ontology:cs_axiom_grounding('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', decline_was_overdetermined_and_entangled, empirically_contingent).
narrative_ontology:cs_axiom('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', secondary, honor_courts_mediated_legal_and_cultural_pathways).
narrative_ontology:cs_axiom_status(honor_courts_mediated_legal_and_cultural_pathways, holdable).
narrative_ontology:cs_axiom_grounding('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', honor_courts_mediated_legal_and_cultural_pathways, empirically_contingent).
narrative_ontology:cs_reference_frame('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', honor_satisfaction_as_aristocratic_coordination_extraction_system).
narrative_ontology:cs_drift_state('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', post_dueling_prohibition_1880s, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1d3365a7-56e5-4c8b-8e7d-a9069cd5ca24', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, aristocracy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_custodians).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, duel_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, lower_status_individuals).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, women_in_honor_economy).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_distinction_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, violence_as_status_arbitration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and controlled the honor codes (Code Duello, national variants). Benefited from the substrate's concentration of symbolic capital and dispute-resolution monopoly. Exit from the honor system meant exit from the class identity itself — aristocrats who refused duels faced social death. The substrate was experienced as a natural law of their world.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocracy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, aristocracy, beneficiary).

% Honor was the operational substrate of officer cohesion and command authority. Duels regulated status disputes that could not enter formal military justice without destroying unit cohesion. Officers were both primary participants and primary enforcers of the code. Exit (resignation) was possible but meant abandoning the only professional identity available to younger sons of nobility and bourgeois aspirants.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, military_officer_corps, agenda_setter).

% Seconds, authors of honor treatises, members of courts of honor (Ehrengerichte). They administered the procedural machinery: negotiating terms, witnessing duels, ruling on point of honor. They derived professional and status income from this role. Their exit was constrained — they were specialists whose expertise had no outside market, but they could (and did) transition to legal or military bureaucracies.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_custodians, agenda_setter,
    organized, biographical, constrained, regional).

% The men who actually fought: officers, aristocrats, students, professionals. They bore the mortality risk (1 in 6 duels fatal in some periods), legal jeopardy (criminal prosecution), and social cost of refusal. Their exit was blocked not by physical barriers but by identity fusion: 'a man who refuses a challenge is no man.' The substrate made their self-concept dependent on participation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, duel_participants, payer,
    moderate, immediate, identity_locked, local).

% Servants, enlisted men, peasants, urban poor — subject to honor violence (insults, assaults, arbitrary challenges) without access to satisfaction. They could not issue challenges, could not choose seconds, had no standing in courts of honor. Their 'exit' was geographic mobility or military service, both constrained. They bore the substrate's externalities without its protections.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, lower_status_individuals, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, lower_status_individuals, excluded).

% Women's sexual reputation was the primary currency of the honor economy — duels fought 'over' women, marriages arranged for honor alliances, widowhood managed for honor preservation. They had no formal role in the code (could not challenge, could not be seconds) but their entire social existence was structured by it. Exit meant religious life or scandalous notoriety — both identity-locked.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, women_in_honor_economy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, women_in_honor_economy, excluded).

% Initially tolerated dueling as aristocratic prerogative and officer corps necessity. From mid-18th century, progressively criminalized (edicts, military codes, civilian penal codes). By 1900, active suppression: police surveillance of known dueling grounds, prosecution of seconds, military cashiering. They experienced the constraint first as a rival jurisdiction, then as a target of state monopoly consolidation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_authorities, agenda_setter).

% Excluded from aristocratic honor culture but competed for status through wealth, profession, and public service. Authored the competing 'dignity culture' (internal moral worth vs. external reputation). Their exclusion was structural — the code required noble birth or officer commission — but they gained analytical leverage through press, literature, and legislative influence. Their cultural competition was the endogenous transformation pressure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeoisie, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeoisie, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, aristocracy).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a private, status-sensitive dispute resolution mechanism for elites (aristocracy, officer corps) that avoided state courts — which were slow, public, and insensitive to honor gradations. Also coordinated violence: channeled lethal force into ritualized encounters with witnesses, seconds, and rules, preventing feuds and ambushes.
% TRANSFER_FUNCTION: Moved life, liberty, and status from duel_participants (who risked death, prosecution, and ruin) and lower_status_individuals/women (who bore collateral violence and reputational appropriation) to aristocracy and officer_corps (who monopolized symbolic capital, command authority, and marriage-market value). The honor_custodians captured professional rents (fees, prestige) for administering the transfer.
% ABSENT_VOICES: Enlisted men, domestic servants, peasant populations, and colonized subjects (in imperial contexts) were structurally excluded — they suffered honor violence without access to satisfaction. Their voices appear only in court records as victims, never as code authors. Early feminists (e.g., George Sand, Sophie Mereau) critiqued the honor economy's treatment of women but were excluded from the honor discourse itself.
% DISAPPEARANCE_RATIONALE: When the substrate vanished (legal prohibition + cultural obsolescence), the world rearranged: state courts absorbed dispute resolution; officer corps cohesion shifted to professional ethics and bureaucratic hierarchy; aristocratic status converted to economic/cultural capital; gendered honor economies partially migrated to new sexual morality regimes. The rearrangement was not seamless — the transition period (1870-1914) saw honor crimes, honor suicides, and residual duel cultures in military/aristocratic enclaves.
% FOUNDING_PROBLEM: Early modern European elites lacked a trusted, status-appropriate mechanism for resolving status disputes. State courts were seen as alien (Roman law, bureaucratic, public) and inadequate for the fine gradations of aristocratic honor. The substrate was built to solve: (1) private dispute resolution for elites, (2) violence channeling to prevent feuds, (3) status boundary maintenance against rising bourgeoisie.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by: (1) state court reformers (e.g., Feuerbach, Savigny) who documented the growing legitimacy and reach of state justice; (2) military reformers (Scharnhorst, Gneisenau) who replaced honor-based cohesion with professional/bureaucratic models; (3) bourgeois liberals (constant press campaign) who documented the code's incompatibility with legal equality. The aristocracy and officer corps themselves attested the problem was live until the 1880s — their memoirs and codes treat the substrate as necessary. No non-beneficiary source corroborates the problem's persistence past 1870.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: The substrate extracted life, liberty, and status from participants (especially non-elite) while concentrating symbolic capital in aristocratic/officer hands. The rate is not higher because the coordination function (private dispute resolution) was genuine and valued by participants. Suppression 0.72: Legal prohibition required active enforcement (police, military courts, civilian criminalization) that intensified over the interval. Theater 0.58: By the late period, ritualized duels (first blood, deliberate misses) and performative seconds' negotiations indicate growing performative-to-functional ratio. Accessibility_collapse 0.75: For identity_locked participants (officers, aristocrats), alternatives were nearly unthinkable — the substrate structured self-concept. Resistance 0.45: Resistance existed (reformist codes, bourgeois criticism, female salon influence) but was fragmented and largely internal to the beneficiary class until late.
 *
 * PERSPECTIVAL GAP:
 *   The aristocracy/officer seats experience the constraint as a mountain (natural, inevitable, identity-constituting) — high accessibility_collapse, near-zero resistance from their perspective. The duel_participants and lower_status seats experience it as a snare (coercive, extractive, no exit). The honor_custodians experience it as a rope (coordination they maintain). The state_legal_authorities experience it as a snare to be suppressed. The engine computes these divergences from the structural data: same constraint, different positional atoms (power, exit_options, time_horizon) yield different effective extraction and thus different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocracy and officer_corps are structural beneficiaries (d ~0.15-0.25): they collect status rents, control code interpretation, and have arbitrage-grade exit (could retire from service, though identity_locked makes it costly). Honor_custodians are near-symmetric (d ~0.45): they administer and gain prestige but bear enforcement burden. Duel_participants are full targets (d ~0.85): identity_locked, mortality risk, legal jeopardy, no viable exit without status loss. Lower_status_individuals and women are trapped targets (d ~0.95): powerless, identity_locked through gender/class position, bear collateral violence. State_authorities start as observers (d ~0.5) but become agenda_setters of suppression (d shifts as they internalize the suppression mission). Bourgeoisie are excluded (d ~0.5) but their cultural competition creates the endogenous transformation pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (private aristocratic dispute resolution without state courts) was live until state courts gained legitimacy and reach (roughly 1750-1850). After that, the coordination function atrophied but the extraction function (status maintenance, officer cohesion) persisted — a classic mandatrophy trap. The composite reading avoids mislabeling this as pure snare because the coordination function was genuine and valued early on; it avoids mislabeling as pure rope because extraction was asymmetric and enforcement-active throughout. The overdetermined decline means the mandatrophy resolution was blocked: legal suppression prevented the coordination function from naturally atrophying (it was crushed), while cultural transformation prevented the extraction function from stabilizing (its legitimacy substrate dissolved). The constraint disappeared rather than resolving into a stable type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_mechanism,
    'Through what specific mechanisms did legal suppression and honor code transformation become causally entangled rather than merely concurrent?',
    'Comparative micro-history of dueling prosecutions alongside honor code treatises across jurisdictions; trace whether legal cases cite shifting honor norms or whether honor treatises cite legal pressure as justification for reform.',
    'If entanglement is mediated through state honor courts (e.g., Prussian Ehrengerichte) that both enforced and reshaped the code, the constraint is a single tangled_rope with dual extraction axes. If they operated on separate tracks that merely coincided, the composite reading overstates unity and the kernel decomposes into two constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_entanglement_mechanism, empirical, 'Whether the two causal pathways share institutional mediation or are analytically separable').

omega_variable(
    kernel_reading_commitment,
    'This constraint is the composite_overdetermined_reading of kernel honor_satisfaction_substrate; sibling readings are practice_decline_reading and cultural_contraction_reading. What structural elements distinguish this reading?',
    'The engine computes per-seat classifications from the structural data; this omega records the committer-frame commitment that the decline was overdetermined with non-independent pathways, not additive or monocausal.',
    'If resolved toward monocausal exogenous suppression, the constraint reclassifies toward snare (coordination cover for extraction). If resolved toward monocausal cultural contraction, it reclassifies toward mountain_erosion (substrate transformation without coordination collapse). The composite reading holds both as entangled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Commitment to overdetermined entangled decline as the structural signature of this reading').

omega_variable(
    honor_substrate_naturalness,
    'Did the honor substrate appear to participants as a natural/mountain-like constraint (inevitable, pre-social) or as a constructed coordination system?',
    'Analyze contemporary treatises, correspondence, and legal commentary for language of ''natural law of honor'' vs. ''instituted code''; track whether participants experienced exit as unthinkable (identity_locked) or merely costly (constrained).',
    'If experienced as mountain-like (high accessibility_collapse, low resistance), the substrate''s erosion is a false_summit_mountain dynamic. If experienced as constructed coordination (moderate accessibility_collapse, measurable resistance), the tangled_rope classification holds throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_naturalness, empirical, 'Whether the honor substrate had false-summit mountain characteristics for its participants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_cor_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hss_cor_tr_t50, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(hss_cor_tr_t100, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(hss_cor_tr_t150, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement(hss_cor_tr_t200, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(hss_cor_tr_t250, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 250, 0.58).

% Extraction over time
narrative_ontology:measurement(hss_cor_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hss_cor_be_t50, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(hss_cor_be_t100, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(hss_cor_be_t150, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement(hss_cor_be_t200, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement(hss_cor_be_t250, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hss_cor_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hss_cor_su_t50, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(hss_cor_su_t100, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(hss_cor_su_t150, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 150, 0.65).
narrative_ontology:measurement(hss_cor_su_t200, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(hss_cor_su_t250, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 250, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, state_monopoly_violence_consolidation).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_dignity_culture_formation).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, military_justice_reform_19th_century).

% DUAL FORMULATION NOTE:
% Part of honor_satisfaction_substrate kernel family. This reading (composite_overdetermined) links to practice_decline_reading (exogenous-only) and cultural_contraction_reading (endogenous-only). The three readings share the kernel but author different ε values and beneficiary/victim structures: practice_decline assigns lower extractiveness (coordination persists), cultural_contraction assigns higher accessibility_collapse (substrate transformation is total), composite assigns intermediate values with high theater and suppression reflecting entanglement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, institutional, 0.2).
constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, organized, 0.4).
constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, moderate, 0.85).
constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
