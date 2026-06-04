% ============================================================================
% CONSTRAINT STORY: senate_authority__oligarchic_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senate_authority__oligarchic_capture_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: senate_authority__oligarchic_capture_reading
 *   human_readable: Senate Authority as Oligarchic Capture (Institutionalized Nobility Reading)
 *   domain: legal/doctrinal/roman_constitutional
 *
 * SUMMARY:
 *   The Roman Senate under the oligarchic_capture_reading is a
 *   self-perpetuating institutional mechanism through which a hereditary
 *   nobility (the nobiles, descendants of successful magistrates)
 *   systematized the extraction of military booty, provincial tribute, and
 *   state resources into senatorial estates. This reading interprets the
 *   Senate not as an advisory council or a deliberative body of necessity,
 *   but as a formalized oligarchic monopoly: ex-magistrates recycled
 *   themselves through offices and consolidated power through procedural
 *   consensus that no popular magistrate could defy without career
 *   destruction. The Senate's 'auctoritas' — its 'advice,' 'weight,' and
 *   'authority' in the formal constitution — was de facto command because
 *   oligarchic solidarity and control of enforcement mechanisms (military,
 *   priesthood, legal prosecution) made dissent impossible. The constraint
 *   exhibits high extractiveness (0.68) and high suppression (0.72), with
 *   extractiveness rising over the interval as the oligarchy systematized
 *   mechanisms for converting military conquest into private estate wealth
 *   and blocking popular land redistribution. Theater ratio (0.55) reflects
 *   that formal republican machinery (assemblies, magistracies, written laws)
 *   continued to function while actual decision-making authority migrated to
 *   Senate consensus behind closed doors. This reading forecloses the
 *   advisory_only_reading's claim that Senate authority was genuinely
 *   non-binding, and influences the deliberative_supremacy_reading by showing
 *   that the 'necessity' for continuous deliberative authority arose from
 *   oligarchic concentration, not from inherent complexity.
 *
 * KEY AGENTS:
 *   - Nobiles (senatorial order): Institutional/arbitrage — Primary beneficiaries; self-perpetuating through magistratic succession and kinship networks; extract war booty, provincial tribute, and state revenue to estates
 *   - Plebs (land-hungry populace): Powerless/trapped — Primary victims; blocked from land redistribution and decision-making power; experience extraction through taxation and conscription
 *   - Provincial subjects: Moderate/constrained — Secondary victims; experience extraction through tax collection and military occupation; constrained by imperial military apparatus
 *   - Popular magistrates: Moderate/mobile — Secondary actors; experience mixed coordination (need Senate consensus for legitimacy and funding) and extraction (Senate consensus forecloses popular initiatives); retain some structural agency
 *   - Republican machinery (formal assemblies, laws, procedures): Institutional/arbitrage — Performative apparatus; sustains legitimacy while actual power migrates to Senate; benefits the oligarchy through legal cover
 *   - Analytical observer: Analytical/analytical — Civilizational view risking naturalization of oligarchic capture as 'inherent to republics'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senate_authority__oligarchic_capture_reading, 0.68).
domain_priors:suppression_score(senate_authority__oligarchic_capture_reading, 0.72).
domain_priors:theater_ratio(senate_authority__oligarchic_capture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senate_authority__oligarchic_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(senate_authority__oligarchic_capture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(senate_authority__oligarchic_capture_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senate_authority__oligarchic_capture_reading, snare).
narrative_ontology:human_readable(senate_authority__oligarchic_capture_reading, "Senate Authority as Oligarchic Capture (Institutionalized Nobility Reading)").
narrative_ontology:topic_domain(senate_authority__oligarchic_capture_reading, "legal/doctrinal/roman_constitutional").

domain_priors:requires_active_enforcement(senate_authority__oligarchic_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(senate_authority__oligarchic_capture_reading, '86a8c170-d9c7-48ad-8871-642b201e3046').
narrative_ontology:cs_kernel_codification('86a8c170-d9c7-48ad-8871-642b201e3046', formalized).
narrative_ontology:cs_authority_grounding('86a8c170-d9c7-48ad-8871-642b201e3046', extraction).
narrative_ontology:cs_interpretation_layer_present('86a8c170-d9c7-48ad-8871-642b201e3046').
narrative_ontology:cs_reading_relation('86a8c170-d9c7-48ad-8871-642b201e3046', senate_authority__advisory_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('86a8c170-d9c7-48ad-8871-642b201e3046', senate_authority__deliberative_supremacy_reading, influences).
narrative_ontology:cs_axiom('86a8c170-d9c7-48ad-8871-642b201e3046', foundational, senate_authority_derives_from_oligarchic_solidarity).
narrative_ontology:cs_axiom_status(senate_authority_derives_from_oligarchic_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('86a8c170-d9c7-48ad-8871-642b201e3046', senate_authority_derives_from_oligarchic_solidarity, empirically_contingent).
narrative_ontology:cs_axiom('86a8c170-d9c7-48ad-8871-642b201e3046', foundational, popular_initiative_structurally_foreclosed).
narrative_ontology:cs_axiom_status(popular_initiative_structurally_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('86a8c170-d9c7-48ad-8871-642b201e3046', popular_initiative_structurally_foreclosed, empirically_contingent).
narrative_ontology:cs_reference_frame('86a8c170-d9c7-48ad-8871-642b201e3046', oligarchic_monopoly_through_senatorial_succession).
narrative_ontology:cs_drift_state('86a8c170-d9c7-48ad-8871-642b201e3046', late_republic_end_of_traditional_oligarchy, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('86a8c170-d9c7-48ad-8871-642b201e3046', '').
narrative_ontology:cs_kernel_id(senate_authority__oligarchic_capture_reading, senate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senate_authority__oligarchic_capture_reading, nobiles).
narrative_ontology:constraint_beneficiary(senate_authority__oligarchic_capture_reading, senatorial_estates).
narrative_ontology:constraint_victim(senate_authority__oligarchic_capture_reading, plebs).
narrative_ontology:constraint_victim(senate_authority__oligarchic_capture_reading, provincial_subjects).
narrative_ontology:constraint_victim(senate_authority__oligarchic_capture_reading, popular_magistrates).
narrative_ontology:constraint_victim(senate_authority__oligarchic_capture_reading, popular_assemblies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBS (SNARE) — Trapped by procedural weight and oligarchic control of military and treasury. The Senate's 'advice' forecloses popular initiative through consensus-signaling that no magistrate dare violate. Exit is structural impossibility: plebs cannot leave the Republic, cannot veto Senate decisions, cannot access the formal deliberative bodies. Maximum experienced extraction — the oligarchy channels war booty, provincial tribute, and state revenues to senatorial estates while blocking land redistribution.
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL SUBJECTS (SNARE) — Constrained by military occupation and tax extraction mechanisms. The Senate designs imperial policy in the interest of senatorial latifundia expansion and tributary wealth concentration. Provinces cannot exit the empire; exit costs (rebellion, conquest, enslavement) are absolute. High extraction, structured suppression through military presence and administrative dependencies.
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: POPULAR MAGISTRATE (TANGLED ROPE) — Mobile within the system (can refuse office, can exercise veto during tenure, can appeal to assembly) but experiences significant extraction from Senate procedures. The magistrate needs Senate consensus for funding and legitimacy; lacks it, and the magistrate becomes a rogue operator. Mixed experience: genuine coordination function (Senate deliberates common interest, coordinates complex administration) and asymmetric extraction (Senate pressure enforces oligarchic consensus and forecloses popular-backed initiatives). Moderate extraction because the magistrate retains structural agency within the constraint.
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SENATORIAL ORDER (ROPE) — Experiences the Senate as coordination mechanism for their collective interest. The constraint coordinates estate protection, war finance, provincial resource extraction, and the closed succession of magistracies within the nobility. The constraint is functional from this perspective — coordination with minimal coercion because consensus is guaranteed by kinship and class solidarity. Senatorial class sees the system as legitimate deliberation, not extraction, because extraction flows toward them.
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REPUBLICAN MACHINERY (PITON) — From a civilizational view, the formal legal structure of the Republic (assembly votes, magistrates, written constitutions) is substantially performative. The actual decision-making authority has migrated to the Senate chamber, where unwritten consensus among the nobiles forecloses assembly choices before they are formally proposed. The machinery persists through institutional inertia and the class's interest in maintaining legal legitimacy — 'the Republic' wears the oligarchy's name. Theater ratio high (0.55-0.70) because vast legal apparatus exists to ratify decisions already made. Piton classification derives from the gap between formal structure (assembly-sovereign republic) and actual power (Senate-sovereign oligarchy).
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-spanning analytical context, some form of oligarchic concentration is claimed as inherent to republican government: 'stable republics require a propertied deliberative class; military and financial complexity demand expertise; expertise concentrates power; concentration is the price of stability.' This perspective naturalizes the Senate's oligarchic capture as a law of political economy. However, the structural data reveals this as a false summit: the classification is driven by beneficiary dominance (nobiles extract enormous gain), suppression (systematic foreclosure of plebs), and enforcement (military and legal mechanisms sustain the arrangement). The 'law of nature' framing masks a contingent institutional arrangement.
constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senate_authority__oligarchic_capture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senate_authority__oligarchic_capture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senate_authority__oligarchic_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senate_authority__oligarchic_capture_reading, TR),
    TR >= 0.70.

:- end_tests(senate_authority__oligarchic_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate, rising over the interval from 0.42 to 0.68. The oligarchy begins with moderate extraction (early Republic period, t=0) when senatorial class consolidates magistratic succession and develops client-patron networks; extraction intensifies as the Senate systematizes conversion of military conquest into private wealth (mid-Republic period, t=150) and peaks as provincial empire accumulates under senatorial control (late Republic period, t=300). The measurement tracks the historical trajectory: as the Senate's institutional mechanisms solidify, the extractiveness of the constraint increases. Suppression (0.72): High and rising. Suppression mechanisms include: (a) procedural barriers (complex election calendars, property requirements for office, religious veto powers held by oligarchic priesthood); (b) military control (Senate dominates provincial armies, can use force against reformers); (c) legal enforcement (oligarchic control of magistracies allows prosecution of defiant magistrates); (d) oligarchic solidarity (kinship networks and shared economic interest make consensus-breaking costly). Rising suppression trajectory reflects increasing mechanization of these barriers. Theater ratio (0.55): Moderate, stable. The formal republican machinery continues to function — assemblies meet, magistrates hold office, laws are written — but decision-making authority is concentrated in Senate consensus. The constraint requires both coordination (Senate must deliberate common interest) and performance (formal machinery must ratify pre-made decisions), hence moderate theater ratio rather than high.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap between beneficiaries and victims most sharply. The senatorial order (Rope perspective) sees legitimate deliberation and coordination for collective nobility interest; the plebs (Snare perspective) see an oligarchic monopoly foreclosing their initiative and channeling state resources to estates. The popular magistrate (Tangled Rope perspective) occupies a middle position: they need Senate consensus for legitimacy but can exercise veto power and assembly appeal. The analytical observer (Mountain perspective) risks naturalizing the oligarchic capture as 'inherent to republics' — a constitutional law rather than a contingent arrangement. The piton perspective (Republican machinery) shows that the formal system persists through inertia while actual power has migrated. The entire perspectival set is unified by the oligarchic_capture_reading's core claim: the Senate is a mechanism for institutionalizing and legitimizing oligarchic extraction under the name of the Republic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from structural position within the constraint. Beneficiaries (nobiles, senatorial estates) occupy high institutional power with arbitrage options (can form factions, can maneuver within the Senate): d ≈ 0.10 → f(d) ≈ -0.05 → negative effective extraction (they are net beneficiaries; extraction flows toward them). Victims (plebs, provincial subjects) occupy low power with trapped/constrained exit: d ≈ 0.90 → f(d) ≈ 1.38 → high effective extraction (they experience maximum chi). Popular magistrates occupy moderate power with mobile/constrained exit (can refuse office, can veto during tenure, but face Senate consensus pressure): d ≈ 0.65 → f(d) ≈ 1.00 → moderate effective extraction. The scope modifier σ(S) is continental (1.1), which amplifies extractiveness slightly — larger scope makes verification of oligarchic unity and monitoring of distributed extraction more difficult, allowing higher real extraction to hide in institutional complexity. The senatorial order itself (Perspective 4) sees the system as Rope because from their vantage, the constraint primarily solves the coordination problem of managing the empire and the succession of offices among themselves; the extraction is invisible to them because it flows toward them.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy resolution because extractiveness (0.68) exceeds the 0.70 threshold in the final measurement, though it falls slightly below at t=300 (0.68 vs 0.70 threshold). The core structural tension resolved is between claiming the Senate was purely advisory (which would classify as Rope or lower) versus showing it operated as an extraction mechanism (Snare). The extraction is real and structural — senatorial class genuinely concentrates wealth and power through institutionalized monopoly. The coordination function also exists — the Senate does coordinate defense, foreign policy, and state administration. But the asymmetry is the defining feature: plebs and provinces experience suppression and extraction; nobiles experience coordination. The snare classification is confirmed because suppression (0.72) is high, extractiveness is high, and chi is amplified by scope. The false-summit risk in Perspective 6 (the Natural Law view) is documented in omega 4: if the oligarchic capture is naturalized as 'inherent to republics,' the institutional arrangement is misclassified as a law of political nature rather than a contingent constraint. The resolution is explicit: the oligarchic_capture_reading declares this as institutional extraction, not natural law, and the beneficiary/victim structure makes the false summit visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senate_advice_vs_command_boundary,
    'Is the Senate''s ''auctoritas'' (authority, weight, advice) genuinely non-binding on magistrates, or does the oligarchic consensus make defiance politically impossible?',
    'Historical documentation of magistrate behavior: count instances of explicit defiance vs. compliance; track outcomes of defiant magistrates (prosecution, career destruction, forced resignation); measure the interval between Senate ''advice'' and magisterial action',
    'If defiance is rare/severely punished: Senate operates as command authority (reinforces snare classification). If defiance is feasible without retaliation: Senate operates as genuine advisory body (shifts toward rope/tangled_rope from plebs perspective). The oligarchic_capture_reading assumes defiance is structurally impossible due to oligarchic solidarity and control of enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(senate_advice_vs_command_boundary, empirical, 'Whether Senate auctoritas is binding in fact despite formal non-binding status').

omega_variable(
    oligarchic_solidarity_mechanism,
    'What structural mechanism sustains oligarchic unity? Is it kinship-based (patrons and clients reproducing familial hierarchy), wealth-based (shared economic interest in estates and tribute), or procedural (controlled succession and consensus rules)?',
    'Prosopographical analysis of Senate families; reconstruction of client-patron networks; tracking of magistratic election patterns across generations; analysis of Senate meeting records (where preserved) for dissent rates',
    'If mechanism is fragile (rarely breaks): oligarchy is contingent and vulnerable to pressure (classification could shift toward scaffold from an organized perspective). If mechanism is robust (never breaks): oligarchy is self-perpetuating (reinforces snare). The oligarchic_capture_reading depends on robust mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligarchic_solidarity_mechanism, empirical, 'Structural basis of oligarchic solidarity within the Senate').

omega_variable(
    plebeian_coalition_counterfactual,
    'Could a coalition of plebeian magistrates and assembly reformers have overridden Senate authority by calling the formal assembly to vote and mobilizing popular support, or were the procedural barriers (religious calendar, elite veto, property requirements) insurmountable?',
    'Analysis of attempted Gracchan and populist reforms; documentation of their mechanisms and failure points; counterfactual modeling of alternative timelines based on historical contingencies (earlier or stronger reformer, different elite fracture, external military threat creating crisis)',
    'If barriers are insurmountable: Senate monopoly is structural (reinforces snare). If barriers are surmountable with sufficient coordination: plebs have latent agency (shifts toward tangled_rope). The oligarchic_capture_reading assumes insurmountable barriers created by oligarchic control of magistrates, priesthood, and military.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(plebeian_coalition_counterfactual, conceptual, 'Whether plebeian coalition could structurally overcome Senate authority').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the contested kernel ''senate_authority.'' Do the sibling readings (advisory_only_reading, deliberative_supremacy_reading) represent genuinely different constitutional structures, or are they alternative framings of the same oligarchic reality?',
    'Textual and institutional analysis: does the advisory_only reading''s claim (Senate legally non-binding) rest on different structural facts than the oligarchic_capture reading''s claim (Senate binding through political solidarity), or the same facts narrated differently? Do the readings predict different observable outcomes?',
    'If same facts, different framings: readings coexist_with each other (all live in a single historical moment). If different structural facts: one reading forecloses the other. If structural pressure from one to another: one reading influences the other. The oligarchic_capture_reading coexists_with advisory_only_reading (both live simultaneously in the historical record; different observers emphasize different observable behavior) but influences deliberative_supremacy_reading (oligarchic concentration creates the ''necessity'' for Senate deliberative authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between oligarchic_capture_reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senate_authority__oligarchic_capture_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(senate_cap_tr_t0, senate_authority__oligarchic_capture_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(senate_cap_tr_t150, senate_authority__oligarchic_capture_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement(senate_cap_tr_t300, senate_authority__oligarchic_capture_reading, theater_ratio, 300, 0.55).

% Extraction over time
narrative_ontology:measurement(senate_cap_be_t0, senate_authority__oligarchic_capture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(senate_cap_be_t150, senate_authority__oligarchic_capture_reading, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(senate_cap_be_t300, senate_authority__oligarchic_capture_reading, base_extractiveness, 300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(senate_cap_su_t0, senate_authority__oligarchic_capture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(senate_cap_su_t150, senate_authority__oligarchic_capture_reading, suppression_requirement, 150, 0.64).
narrative_ontology:measurement(senate_cap_su_t300, senate_authority__oligarchic_capture_reading, suppression_requirement, 300, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senate_authority__oligarchic_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(senate_authority__oligarchic_capture_reading, 0.14).
narrative_ontology:affects_constraint(senate_authority__oligarchic_capture_reading, senate_authority__advisory_only_reading).
narrative_ontology:affects_constraint(senate_authority__oligarchic_capture_reading, senate_authority__deliberative_supremacy_reading).
narrative_ontology:affects_constraint(senate_authority__oligarchic_capture_reading, gracchian_land_redistribution__oligarchic_veto).
narrative_ontology:affects_constraint(senate_authority__oligarchic_capture_reading, provincial_extraction__imperial_tribute_system).

% DUAL FORMULATION NOTE:
% The senate_authority kernel decomposes into three constraint stories: advisory_only_reading (low extractiveness, Senate as formal advice with political bluff as escape valve), deliberative_supremacy_reading (moderate extractiveness, Senate as necessary continuous deliberation), and oligarchic_capture_reading (high extractiveness, Senate as institutionalized nobility monopoly). Each reading ε-differs because each asks different structural questions and identifies different beneficiary/victim distributions. The oligarchic_capture_reading upstream-influences the other two: it shows that the 'need' for advisory authority arises from oligarchic control (if not oligarchic, advice would be defied more often), and that 'necessity' of continuous deliberation arises from oligarchic concentration requiring continuous coordination among the nobility. Network decomposition maps the kernel contest into network structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
