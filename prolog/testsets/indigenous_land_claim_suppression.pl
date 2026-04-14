% ============================================================================
% CONSTRAINT STORY: indigenous_land_claim_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indigenous_land_claim_suppression, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indigenous_land_claim_suppression
 *   human_readable: Indigenous Land Claim Suppression
 *   domain: political_economy/colonialism
 *
 * SUMMARY:
 *   Indigenous land claim suppression represents a foundational constraint of
 *   settler colonial states. The constraint operates through legal frameworks
 *   (property law denying indigenous title), economic mechanisms (resource
 *   extraction licensing that bypasses indigenous claims), police/military
 *   enforcement (eviction, criminalization of land occupation), and
 *   psychological mechanisms (epistemic dismissal of indigenous sovereignty
 *   claims, intergenerational trauma). The constraint exhibits all six DR
 *   types from different perspectives, making it a diagnostic exemplar for
 *   how structural extraction naturalizes itself through institutional
 *   legitimation. From the settler state's institutional perspective, the
 *   suppression appears as legitimate governance (Rope). From indigenous
 *   peoples' perspective, it appears as irreversible entrapment (Snare with
 *   identity_locked exit options). From international indigenous rights
 *   coalitions, it appears as a temporary problem being solved through
 *   norm-building (Scaffold). From colonial legal doctrine itself, it appears
 *   as a degraded ritual maintained through institutional inertia (Piton).
 *   The analytical observer risks naturalizing settler colonialism as an
 *   inherent feature of modernity (false Mountain). The constraint's
 *   extractiveness (0.68) reflects that significant land wealth, resource
 *   rights, and political sovereignty flow from indigenous peoples to settler
 *   state actors and corporations. The suppression (0.78) reflects multiple
 *   overlapping barriers: legal frameworks that deny indigenous title,
 *   resource dependency that makes resistance costly, geographic isolation of
 *   reserves, underfunded legal systems, and internalized trauma that makes
 *   claim assertion psychologically difficult. The theater ratio (0.65)
 *   reflects that the constraint's legitimation mechanisms are increasingly
 *   performative: courts hold land claim hearings, governments issue
 *   apologies and recognition statements, but actual land return and
 *   sovereignty acknowledgment remain minimal, creating a gap between
 *   symbolic legitimation and material change.
 *
 * KEY AGENTS:
 *   - Indigenous Peoples: Primary victims (powerless/trapped, identity_locked) — structurally and psychologically imprisoned in resource-depleted territories with no legal recourse within settler state frameworks
 *   - Settler State Apparatus: Primary beneficiary (institutional/arbitrage) — maintains monopoly on sovereignty and resource allocation authority through legal suppression of competing indigenous claims
 *   - Extractive Industry Corporations: Secondary beneficiary (institutional/arbitrage) — secure resource extraction rights and liability insulation through state suppression of indigenous land claims
 *   - Indigenous Rights Movements: Organized resistance (moderate/constrained) — face state suppression and co-optation risks but have built transnational networks and legal strategies
 *   - International Indigenous Rights Coalition: Organized counterforce (organized/mobile) — building alternative legal frameworks (UNDRIP, international court precedents) that create sunset pathways for state-based suppression
 *   - Colonial Legal Doctrine: Institutional framework (institutional/arbitrage) — performs legitimation function while actual effectiveness declines (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing settler colonialism as an inevitable feature of modernity, flagged as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indigenous_land_claim_suppression, 0.68).
domain_priors:suppression_score(indigenous_land_claim_suppression, 0.78).
domain_priors:theater_ratio(indigenous_land_claim_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indigenous_land_claim_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(indigenous_land_claim_suppression, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(indigenous_land_claim_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indigenous_land_claim_suppression, snare).
narrative_ontology:human_readable(indigenous_land_claim_suppression, "Indigenous Land Claim Suppression").
narrative_ontology:topic_domain(indigenous_land_claim_suppression, "political_economy/colonialism").

domain_priors:requires_active_enforcement(indigenous_land_claim_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indigenous_land_claim_suppression, settler_state_apparatus).
narrative_ontology:constraint_beneficiary(indigenous_land_claim_suppression, extractive_industry_corporations).
narrative_ontology:constraint_beneficiary(indigenous_land_claim_suppression, land_speculation_interests).
narrative_ontology:constraint_victim(indigenous_land_claim_suppression, indigenous_peoples).
narrative_ontology:constraint_victim(indigenous_land_claim_suppression, indigenous_sovereignty).
narrative_ontology:constraint_victim(indigenous_land_claim_suppression, ecological_stewardship_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS PEOPLES (SNARE) — Structurally trapped. Legal frameworks (colonial property law, sovereignty doctrine, resource extraction regimes) deny land claim recognition. Material barriers include underfunded legal systems, eviction enforcement, police violence. Psychological barriers include intergenerational trauma, epistemic dismissal of indigenous knowledge systems, and identity suppression. No exit option exists within the settler state's legal framework. Maximum experienced extraction: land wealth transfer, resource rights denial, cultural destruction. The constraint suppresses alternative pathways (sovereignty assertion, traditional governance, ecological management) through coercive mechanisms.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIGENOUS NATIONS (IDENTITY_LOCKED, SNARE) — Structurally mobile in principle (sovereignty theoretically possible, land claims litigable, governance alternatives exist) but identity-locked in practice. The binding mechanism is cognitive and relational: indigenous identity is constituted through the land and through collective memory of dispossession. The constraint operates by making exit unthinkable from within the indigenous identity frame. An individual indigenous person might migrate; an indigenous nation cannot 'exit' without ceasing to exist. The identity lock is not external coercion but internalization of dispossession as irreversible. At biographical horizon, this creates rope appearance (seems changeable in principle) but at civilizational horizon creates mountain appearance (irreversible through any achievable means). The snare classification reflects that the constraint is actively maintained through legal/police mechanisms, not inherent to nature.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(continental))).

% PERSPECTIVE 3: INDIGENOUS RIGHTS MOVEMENTS (TANGLED ROPE) — Constrained by funding limitations, legal barriers, co-optation risks, and state surveillance. Exit options exist but at high cost: organizing can trigger state violence, legal cases drain resources, international advocacy requires institutional partnerships that limit autonomy. But these movements also benefit from coordination mechanisms: shared legal strategies, international networks, media amplification. The constraint contains both genuine coordination function (organizing against dispossession) and asymmetric extraction (state suppression of organizing). Effective extraction is moderate because organized agents have some countervailing power.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLER STATE APPARATUS (ROPE) — Experiences the constraint as coordination: the suppression of indigenous land claims enables centralized state control, resource allocation authority, and national sovereignty assertion. The state perceives the constraint as a legitimate governance mechanism—coordination around property law, resource management, and territorial monopoly. Net beneficiary position: extraction flows toward the state. Exit options are arbitrage: the state can modify the constraint through law, but doing so would require abandoning the settler colonial sovereignty model. From the state's institutional perspective, the constraint is pure Rope—a coordination solution to the 'problem' of competing sovereignty claims.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXTRACTIVE INDUSTRY CORPORATIONS (ROPE) — Land claim suppression enables resource extraction licensing, investment security, and cost externalization. Corporations experience the constraint as pure coordination: secure property rights and suppressed competing claims reduce investment risk. The constraint coordinates capital allocation, supply chain certainty, and liability limitation. Net beneficiary: extraction flows toward corporations through land access, resource rights, and liability avoidance. Exit options are arbitrage: corporations could negotiate with indigenous nations directly, but doing so would require abandoning state legal frameworks and acknowledging competing sovereignty—economically suboptimal under current arrangements.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL INDIGENOUS RIGHTS COALITION (SCAFFOLD) — United Nations Declaration on the Rights of Indigenous Peoples (UNDRIP), international court precedents (Inter-American Court of Human Rights), and transnational indigenous organizing are building alternative verification pathways and enforcement mechanisms. This organized force sees the constraint as temporary—sunset logic applies because indigenous land rights recognition is increasingly enshrined in international law, creating alternative pathways that bypass settler state legal monopoly. Low effective extraction experienced by this perspective because the coalition has agency and perceives exit paths. Sunset horizon: 15-30 years for international norms to force domestic legal recognition (though resistance remains strong).
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: COLONIAL LEGAL DOCTRINE (PITON) — The doctrine of discovery, terra nullius, and state sovereignty over indigenous territory are largely performative—maintained through institutional inertia and epistemic closure, not because they are empirically defensible or functionally necessary. Modern states increasingly recognize indigenous rights in principle while maintaining suppression in practice through procedural delay, evidentiary standards, and legal technicalities. The theater ratio is high: courts hold hearings, governments issue reports, and land claim commissions function, but actual land transfer and sovereignty recognition remain minimal. The piton classification reflects that the legal architecture is degraded—kept standing through institutional momentum despite acknowledged injustice and reduced functional effectiveness.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — A tempting analytical position claims that land claim suppression is an immutable feature of state consolidation and modernity: all modern nation-states rest on prior dispossession, and indigenous claims will always be suppressed because recognizing them would dissolve state sovereignty. This perspective naturalizes contingent institutional arrangements (settler colonial law, resource extraction regimes, state monopoly on legitimate violence) as laws of nature. The engine flags this as a false summit: the structural data shows active enforcement, high theater, and organized beneficiaries—all indicators of contingent institutional extraction, not natural law.
constraint_indexing:constraint_classification(indigenous_land_claim_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indigenous_land_claim_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indigenous_land_claim_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indigenous_land_claim_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indigenous_land_claim_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indigenous_land_claim_suppression, TR),
    TR >= 0.70.

:- end_tests(indigenous_land_claim_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Land and resource wealth transfer is substantial—indigenous territories contain disproportionate mineral, timber, and fossil fuel reserves that flow to settler state control and corporate extraction. Political sovereignty extraction is near-total: indigenous peoples exercise minimal self-determination within settler state frameworks. The value reflects that extraction is severe but not absolute—some indigenous communities maintain partial land control and governance autonomy. The trajectory shows increase over the interval (0.55 → 0.68) reflecting intensifying resource extraction and land pressure as extractive industries accelerate. Suppression (0.78): Very high. Multiple overlapping enforcement mechanisms: legal (property law denying indigenous title), coercive (police/military eviction and incarceration), economic (resource dependency making resistance costly), epistemic (dismissal of indigenous knowledge systems as illegitimate), and psychological (intergenerational trauma, identity suppression). The suppression is not total because indigenous peoples maintain cultural continuity, organize resistance movements, and retain some land control. Critically, suppression operates through identity internalization—indigenous peoples' own cognitive frames (identity fusion with land, trauma patterns) become suppression mechanisms independent of external coercion. Theater ratio (0.65): Moderate-high and increasing (0.35 → 0.65). The constraint's legitimation mechanisms are increasingly performative: government apologies without land return, land claim commissions with minimal actual redistribution, consultation processes that don't affect policy, and symbolic recognition that doesn't alter material control. The rise in theater reflects that the constraint is becoming less sustainable through pure coercion alone—states are forced to perform legitimation while suppression persists. This is diagnostic of a constraint under stress (similar to the piton pattern where function decays and theater rises).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of perspectives: Indigenous peoples perceive snare (or snare with identity_locked exit), settler state perceives rope, corporations perceive rope, movements perceive tangled_rope, international coalition perceives scaffold, legal doctrine performs piton, and the analytical observer risks false mountain. The gaps are structural: beneficiaries experience the constraint as coordination (rope) because they benefit from the extraction flow; victims experience it as irreversible extraction (snare) because they face total exclusion and legal defeat; organized agents experience constraint (tangled_rope) because they have some countervailing power; international coalitions experience sunset (scaffold) because they see alternative legal pathways building; the constraint's own legitimating mechanisms perform degradation (piton) because they are increasingly symbolic; and the analytical observer risks naturalizing contingency (false mountain) by seeing settler colonialism as inevitable rather than contingent on specific institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position. Indigenous peoples are victims facing trapped/identity_locked exit—they derive high d (approaching 1.0) because extraction flows away from them and alternatives are suppressed. Settler state apparatus and corporations are beneficiaries with arbitrage exit—they derive low d (approaching 0.0 or negative) because extraction flows toward them. Organized movements are moderate victims with constrained exit—they derive moderate d (0.55-0.70) because they have some power but face serious barriers. International coalitions are organized agents with mobile exit—they derive moderate d because they see pathways out. The piton perspective (colonial legal doctrine) is an institutional beneficiary maintaining the constraint through inertia—it derives low d and experiences the constraint as coordination maintaining legitimacy, even though the doctrine is degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This is a high-extraction snare (ε = 0.68 > 0.70 threshold critically approached) where the mandatrophy is resolved by recognizing that the constraint operates through multiple DR types simultaneously. The mistake would be to ask 'is this really a snare or really a rope?' The answer is both: it is a snare to indigenous peoples and a rope to the settler state. They are measuring the same constraint from different structural positions. The mandatrophy resolves by accepting that (1) snare classification from the powerless/trapped perspective is correct for those agents, (2) rope classification from the institutional beneficiary perspective is correct for those agents, and (3) the constraint's stability derives from this perspectival split—beneficiaries see coordination, victims see extraction, and the gap between these perceptions is the mechanism that prevents collective action against the constraint. The false mountain perspective is a key diagnostic: the analytical observer must resist the temptation to naturalize this as an inevitable feature of state consolidation. The constraint is contingent—it persists because specific institutional arrangements (settler colonial law, corporate resource licensing, state monopoly on legitimate violence) continue to benefit specific actors. These are changeable institutional facts, not laws of nature. The UNDRIP perspective (scaffold) is the critical counter-move: it shows that international legal norms are building alternative pathways that bypass state-based suppression. If these norms consolidate over the next 15-30 years, the constraint's sustainability degrades, classification shifts toward scaffold, and exit pathways open. The mandatrophy is resolved by showing that all eight perspectives are internally coherent and that their combination reveals the constraint's fragility: it relies on maintaining the perspectival gap between rope-perceiving beneficiaries and snare-perceiving victims, on forestalling scaffold outcomes through institutional resistance, and on preventing piton recognition (that the legal doctrine is degraded). The constraint is stable only if these stratifications hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression primarily structural (external legal/police barriers) or primarily internalized (cognitive patterns, identity fusion, historical trauma that persists independent of external enforcement)?',
    'Longitudinal tracking of exit behavior post-legal-reform: if suppression dramatically decreases after legal recognition of land claims, it was primarily structural. If suppression persists through internalized barriers (trauma responses, epistemic closure, loss of cultural continuity), it was primarily internalized.',
    'If primarily structural: the constraint can be dissolved through legal/institutional change. If primarily internalized: legal change alone is insufficient; decolonization requires cultural/psychological recovery work over multiple generations. If both: exit trajectories will be cyclical (legal gains followed by internalized regression cycles).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism').

omega_variable(
    sovereignty_incompatibility,
    'Is indigenous sovereignty fundamentally incompatible with settler state sovereignty, or are hybrid arrangements (indigenous self-determination within federated state structures, co-governance regimes, treaty-based asymmetric sovereignty) structurally sustainable?',
    'Comparative analysis of existing hybrid arrangements (Navajo Nation within USA, Māori governance frameworks in New Zealand, indigenous councils in Bolivia post-plurinational constitution reform): assess whether they reduce extraction and expand indigenous exit options or merely create new layers of constraint.',
    'If incompatible: snare classification is stable, and exit requires state dissolution or secession. If hybrid arrangements work: classification shifts toward tangled_rope or scaffold—constraint becomes negotiable rather than irreversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_incompatibility, conceptual, 'Whether indigenous and settler sovereignties can coexist').

omega_variable(
    land_restitution_feasibility,
    'Is large-scale land restitution to indigenous peoples structurally achievable under any realistic political economy? What proportion of historical lands would constitute meaningful restitution vs. tokenistic redistribution?',
    'Analysis of successful land restitution precedents (South African land reform, Latin American indigenous land recognition, Australian Native Title Act outcomes): measure actual land area transferred, ecological viability, and indigenous control over resource extraction. Establish feasibility thresholds and identify political economy barriers.',
    'If restitution is structurally unfeasible: snare classification is durable, and movements face inevitable defeat (changes to organized/constrained from powerless/trapped, but not to escape). If feasible but blocked by political will: constraint remains snare but with identified exit pathway (changes classification if political economy shifts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_restitution_feasibility, empirical, 'Feasibility of meaningful land restitution').

omega_variable(
    ecological_stewardship_externality,
    'How much of the extraction in this constraint flows to settler state/corporate actors vs. how much represents lost ecological stewardship capacity (unmeasured, externalized to global commons)?',
    'Quantification of ecosystem services (carbon sequestration, biodiversity maintenance, watershed protection, soil preservation) under indigenous management vs. under settler state/corporate management. Assign monetary value and measure loss of stewardship capacity as indirect extraction from global environment.',
    'If stewardship loss is large: actual extraction is higher than measured (ε should increase 0.68 → 0.75+). If small: measurement is accurate. If externalities are enormous but unmeasured: the constraint''s true impact is systematically underestimated in DR analysis focused on direct transfers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecological_stewardship_externality, empirical, 'Lost ecological stewardship as unmeasured extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indigenous_land_claim_suppression, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ilcs_tr_t0, indigenous_land_claim_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ilcs_tr_t50, indigenous_land_claim_suppression, theater_ratio, 50, 0.52).
narrative_ontology:measurement(ilcs_tr_t100, indigenous_land_claim_suppression, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(ilcs_be_t0, indigenous_land_claim_suppression, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ilcs_be_t50, indigenous_land_claim_suppression, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(ilcs_be_t100, indigenous_land_claim_suppression, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indigenous_land_claim_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(indigenous_land_claim_suppression, resource_extraction_licensing).
narrative_ontology:affects_constraint(indigenous_land_claim_suppression, settler_state_sovereignty).
narrative_ontology:affects_constraint(indigenous_land_claim_suppression, indigenous_epistemic_suppression).

% DUAL FORMULATION NOTE:
% Indigenous land claim suppression decomposes into three linked constraints: (1) resource_extraction_licensing (state monopoly on mineral/timber/energy licensing that bypasses indigenous claims; ε ≈ 0.55, Rope from corporate perspective, Snare from indigenous perspective), (2) settler_state_sovereignty (legal doctrine that denies indigenous sovereignty claims; ε ≈ 0.42, Mountain from state perspective, Snare from indigenous perspective), and (3) indigenous_epistemic_suppression (dismissal of indigenous knowledge systems as illegitimate; ε ≈ 0.50, Piton from state perspective, identity_locked Snare from indigenous perspective). This story models the integrated constraint; decomposed stories would isolate specific mechanisms for more precise measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indigenous_land_claim_suppression, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
