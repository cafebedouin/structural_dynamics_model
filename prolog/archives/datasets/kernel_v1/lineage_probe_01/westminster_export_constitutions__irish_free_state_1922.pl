% ============================================================================
% CONSTRAINT STORY: westminster_export_constitutions__irish_free_state_1922
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_export_constitutions__irish_free_state_1922, []).

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
 *   constraint_id: westminster_export_constitutions__irish_free_state_1922
 *   human_readable: Westminster Export Constitution: Irish Free State (1922-1937)
 *   domain: political/legal/constitutional_design
 *
 * SUMMARY:
 *   The Irish Free State (1922-1937) instantiates a unique reading of the
 *   Westminster export kernel: a dominion constitution accepted under Treaty
 *   compulsion, then systematically amended to dissolve its own constraining
 *   provisions, culminating in a 1937 replacement that declared a sovereign
 *   republic explicitly outside the Commonwealth. This constraint captures
 *   the paradox of using the instrument of subordination (the dominion form)
 *   as the mechanism of escape from subordination. The Treaty imposed
 *   Westminster constitutionalism as the price of independence from civil
 *   war. The Irish state inherited the dominion framework and then proceeded
 *   to strip away the crown symbols, the Oath of Allegiance, the
 *   Governor-General's executive powers, and the Judicial Committee of the
 *   Privy Council's appellate jurisdiction through serial amendments
 *   (1933-1936), culminating in the 1937 Constitution that replaced the
 *   entire framework. The constraint is tangled rope at the Irish political
 *   establishment level (genuine coordination through Westminster
 *   institutional scaffolding alongside genuine extraction via suppression of
 *   republican sentiment) and snare at the level of the Treaty constraints
 *   apparatus (the mechanism meant to bind the Free State in perpetuity is
 *   dismantled by the very amendment procedures the Treaty established). From
 *   Westminster's civilizational perspective, the dominion constitution
 *   degraded into a piton: the crown, oath, and appeal survived as ceremonial
 *   forms with no actual enforcement capacity. The analytical observer sees a
 *   temporary scaffold: the dominion form provided institutional continuity
 *   during post-civil-war recovery and offered a legal pathway to exit the
 *   empire through amendment rather than revolution. The Irish republican
 *   forces see tangled rope: they coordinate state-building through
 *   Westminster procedures while extracting legitimacy by systematically
 *   removing imperial tethers. This constraint is one reading of the
 *   contested kernel 'Westminster Export Constitutions' — it coexists with
 *   the Australian federation (Washminster hybrid), Canadian codification
 *   (Westminster written down), and decolonization templates (Lancaster House
 *   exports), but follows a radically different trajectory: systematic
 *   de-imperializtion rather than gradual devolution or hybrid federation.
 *
 * KEY AGENTS:
 *   - Irish Independence Movement & Republican Forces: Primary beneficiary (organized/mobile) — the revolutionary mandate that won the civil war; uses dominion form as exit vehicle from empire
 *   - Treaty Constraints Framework: Primary victim (powerless/trapped) — the Oath, Governor-General, Judicial Committee appeal designed to enforce dominion subordination; dismantled by the instrument meant to entrench them
 *   - Irish Free State Government (1922-1937): Secondary beneficiary and partial victim (moderate/constrained) — genuinely coordinates state-building through Westminster procedures; partially suppressed by crown symbols and treaty tethers; navigates constrained amendment process
 *   - Westminster System Exporters (British Imperial Authority): Secondary beneficiary (institutional/arbitrage) — exported dominion model for imperial coordination; lost enforcement capacity when amendments dissolved the constraining provisions
 *   - Imperial Constitutional Authority (Civilizational): Degraded authority (institutional/arbitrage) — treaty settlement meant to bind dominions to crown perpetually; piton degradation as Irish Free State amends constraints into irrelevance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_export_constitutions__irish_free_state_1922, 0.58).
domain_priors:suppression_score(westminster_export_constitutions__irish_free_state_1922, 0.72).
domain_priors:theater_ratio(westminster_export_constitutions__irish_free_state_1922, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_export_constitutions__irish_free_state_1922, extractiveness, 0.58).
narrative_ontology:constraint_metric(westminster_export_constitutions__irish_free_state_1922, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westminster_export_constitutions__irish_free_state_1922, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_export_constitutions__irish_free_state_1922, tangled_rope).
narrative_ontology:human_readable(westminster_export_constitutions__irish_free_state_1922, "Westminster Export Constitution: Irish Free State (1922-1937)").
narrative_ontology:topic_domain(westminster_export_constitutions__irish_free_state_1922, "political/legal/constitutional_design").

domain_priors:requires_active_enforcement(westminster_export_constitutions__irish_free_state_1922).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_export_constitutions__irish_free_state_1922, '28b0d11b-94b0-4678-8fd1-25a205955e25').
narrative_ontology:cs_kernel_codification('28b0d11b-94b0-4678-8fd1-25a205955e25', formalized).
narrative_ontology:cs_authority_grounding('28b0d11b-94b0-4678-8fd1-25a205955e25', extraction).
narrative_ontology:cs_interpretation_layer_present('28b0d11b-94b0-4678-8fd1-25a205955e25').
narrative_ontology:cs_reading_relation('28b0d11b-94b0-4678-8fd1-25a205955e25', westminster_export_constitutions__australian_federation_1901, coexists_with).
narrative_ontology:cs_reading_relation('28b0d11b-94b0-4678-8fd1-25a205955e25', westminster_export_constitutions__canadian_confederation_1867, coexists_with).
narrative_ontology:cs_reading_relation('28b0d11b-94b0-4678-8fd1-25a205955e25', westminster_export_constitutions__decolonization_constitutions, influences).
narrative_ontology:cs_axiom('28b0d11b-94b0-4678-8fd1-25a205955e25', foundational, dominion_form_as_imperial_exit_vehicle).
narrative_ontology:cs_axiom_status(dominion_form_as_imperial_exit_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('28b0d11b-94b0-4678-8fd1-25a205955e25', dominion_form_as_imperial_exit_vehicle, instrumental).
narrative_ontology:cs_axiom('28b0d11b-94b0-4678-8fd1-25a205955e25', foundational, amendment_power_as_sovereign_reconstitution).
narrative_ontology:cs_axiom_status(amendment_power_as_sovereign_reconstitution, holdable).
narrative_ontology:cs_axiom_grounding('28b0d11b-94b0-4678-8fd1-25a205955e25', amendment_power_as_sovereign_reconstitution, deontological).
narrative_ontology:cs_reference_frame('28b0d11b-94b0-4678-8fd1-25a205955e25', imperial_dominion_settlement).
narrative_ontology:cs_drift_state('28b0d11b-94b0-4678-8fd1-25a205955e25', post_amendment_era_1933_1937, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('28b0d11b-94b0-4678-8fd1-25a205955e25', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(westminster_export_constitutions__irish_free_state_1922, westminster_export_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__irish_free_state_1922, irish_independence_movement).
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__irish_free_state_1922, irish_republican_forces).
narrative_ontology:constraint_victim(westminster_export_constitutions__irish_free_state_1922, treaty_constraints_framework).
narrative_ontology:constraint_victim(westminster_export_constitutions__irish_free_state_1922, imperial_constitutional_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREATY CONSTRAINTS (SNARE) — The Treaty's constitutional settlement cannot exit its own logic. The dominion form was designed to entrench imperial oversight (Governor-General, Oath of Allegiance, Judicial Committee appeal), but the Irish state treats these entrenching mechanisms as temporary scaffolding to be dismantled. The constraints meant to bind the Free State in perpetuity are being removed clause by clause. From the perspective of treaty enforcement, this is extraction: the apparatus meant to constrain the agent is being weaponized against its own purpose. No appeal mechanism can restore what amendment repeals.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRISH POLITICAL ESTABLISHMENT (TANGLED ROPE) — The Free State government genuinely coordinates its new state apparatus through the dominion form: cabinet structure, Oireachtas, civil service organization all follow Westminster models because Westminster codification provides functional scaffolding. But the same constitutional form suppresses the republican mandate that won the civil war. The government benefits from Westminster's institutional maturity while paying the cost of legitimacy suppression — the oath and the crown are coordinating mechanisms that also extract legitimacy from the majority republican position. Active enforcement required: each amendment must navigate the treaty constraint and imperial authority. Mixed extraction and coordination.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTMINSTER SYSTEM EXPORTERS (ROPE) — From London's perspective in 1922, the dominion constitution is pure coordination: it exports the Westminster model to a new dominion, enabling governance and standardizing the imperial relationship across self-governing colonies. The Oath and judicial appeal are coordination mechanisms that bind the dominions together around a common crown. The extractiveness that the Irish state experiences (suppression of republicanism) is, from Westminster's view, simply the cost of membership in the dominion commonwealth. No alternative extraction occurs — Westminster gains predictability and constitutional harmony. However, Westminster's arbitrage option is constrained by the fact that Ireland can amend away the very mechanisms meant to coordinate the relationship.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: IRISH REPUBLICAN FORCES (TANGLED ROPE) — The revolutionary movement that won the civil war sees the dominion constitution as a temporary prison with an exit door. The movement coordinates its state-building through the Westminster form (it inherited the only functioning civil administration) while simultaneously extracting legitimacy by dismantling crown symbols clause by clause. The constitutional amendments are both functional coordination (fixing legal text to match practice) and extractive strategy (removing the symbols of subordination). The movement has substantial agency (organized power) and the mobile exit option because it can choose to replace the entire constitutional framework, as it does in 1937. The tangled rope reflects that genuine coordination and genuine extraction are both happening simultaneously.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL TRANSITION ANALYST (SCAFFOLD) — From a generational/continental view, the dominion constitution is a temporary transitional form: it provides institutional continuity during civil war recovery, enables the state apparatus to function using tested Westminster procedures, and offers a legitimate exit path from empire through legal amendment rather than revolutionary rupture. The theater is moderately high (crown symbols persist while actual sovereignty expands) but the sunset is structurally real: 1937 replaces the entire framework with a new constitution that removes the crown entirely and declares the state a sovereign republic. The dominion form serves exactly this sunset function — it buys time for post-civil-war state-building while leaving the path open to full republican form. Low effective extraction because the endpoint is predetermined and the institutional scaffolding is genuinely useful during transition.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 6: IMPERIAL CONSTITUTIONAL AUTHORITY (PITON) — From the civilizational/global view, the Treaty settlement represents the last effort to preserve empire through constitutional form rather than force. The Oath, the Governor-General, the Judicial Committee appeal, and the royal title are the institutional machinery that was meant to bind dominions to crown perpetually. But by 1937, all these mechanisms have been dismantled by the same instrument (constitutional amendment) that was supposed to entrench them. The imperial authority sees its own scaffolding as degraded: the constitutional forms persist in other dominions but in Ireland they have been rendered performative and then abandoned. The piton theater ratio (0.68) reflects that imperial authority after 1922 consists largely of ceremony and legal formalism with no actual enforcement capacity — the empire cannot prevent Ireland from amending its own constitution. The mechanism survives through inertia (the Governor-General office persists until 1936, the Judicial Committee retains nominal jurisdiction) but its functional authority is gone.
constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_export_constitutions__irish_free_state_1922_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_export_constitutions__irish_free_state_1922, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westminster_export_constitutions__irish_free_state_1922, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westminster_export_constitutions__irish_free_state_1922, TR),
    TR >= 0.70.

:- end_tests(westminster_export_constitutions__irish_free_state_1922_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. At T=0 (1922 Treaty), extractiveness is moderate (0.48) because the dominion form, while suppressing republicanism, provides genuine institutional scaffolding for state-building; the Irish leadership accepts treaty terms as the price of independence. At T=7 (1929 midpoint), extractiveness rises to 0.58 as amendments accumulate (Oath modified 1933, Governor-General powers curtailed, Judicial Committee jurisdiction eliminated) and the Free State government increasingly treats the treaty constraints as impositions to be removed rather than foundations to be maintained. At T=15 (1937 replacement), extractiveness reaches 0.72 as the entire dominion framework is replaced: the constraint's original extractive mechanism (imperial tethering) has been completely dissolved. Suppression (0.72): Moderate-high, declining over the interval. At T=0 (1922), suppression is highest (0.78) because the Oath and crown symbols represent genuine barriers to the Irish state's republican self-conception; the civil war wound is fresh and the dominion form is experienced as an imperial imposition. At T=7 (1929), suppression begins to decline (0.72) as amendments remove the most visible tethers. At T=15 (1937), suppression drops sharply (0.48) because the entire constraining framework has been replaced. Theater ratio (0.68): Moderate, rising over the interval. At T=0 (1922), theater is moderate (0.52) because the dominion constitution performs genuine state-building functions, even though it also suppresses republicanism. By T=7 (1929), theater rises to 0.68 as the Irish state continues to use Westminster procedures for functional governance while increasingly treating the crown symbols as performative formalism. By T=15 (1937), theater reaches 0.85 as the dominion apparatus becomes pure ceremony — the Governor-General is a figurehead without power, the Judicial Committee has been stripped of appellate jurisdiction, the Oath has been repealed, and the entire framework is replaced. The rising theater reflects that the constraining mechanisms lose functional force (they become performative) before they are formally abolished. Claimed type (Tangled Rope) reflects that the Irish political establishment genuinely coordinates state-building through Westminster institutional maturity while simultaneously extracting legitimacy by removing imperial constraints. The beneficiary/victim structure reverses over time: at T=0, Westminster exporters benefit from imperial coordination; by T=15, Irish republican forces have extracted the exit mechanism and Westminster authority is the victim of constraint removal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from identical structural data. The Irish republican movement sees the dominion constitution as a temporary exit vehicle: accepting Westminster form buys institutional credibility and enables amendment-based sovereignty exit. Westminster exporters see pure coordination: the dominion settles a civil war, provides tested institutional procedures, and binds the dominion into imperial community. The Irish political establishment sees mixed coordination and suppression: Westminster scaffolding enables state-building, but crown symbols and the Oath represent real legitimacy costs. The Treaty constraints apparatus sees pure extraction and reversal: the mechanisms meant to bind the Free State perpetually are dismantled by the very amendment process the Treaty established. Imperial authority sees ceremonial degradation: the crown's symbolic and legal tethers survive as piton theater with no enforcement force. The constitutional transition analyst sees temporary scaffolding with a predetermined sunset: the dominion form serves exactly the function of bridging from civil war recovery to republican constitution, and the 1937 replacement is the predictable endpoint. No single type is correct — the constraint IS the perspectival divergence. This is the kernel reading's signature: Westminster exports can be weaponized for imperial exit if the revolutionary movement retains agency through the amendment process.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural position of each perspective relative to the constraint's extraction flow. Irish republican forces: beneficiary + organized + mobile → d ≈ 0.30 (beneficiaries with exit agency experience low effective extraction). Irish political establishment: mixed (partial beneficiary, partial victim) + moderate + constrained → d ≈ 0.55 (moderate power with genuine constraints but also real agency). Westminster exporters: beneficiary + institutional + arbitrage → d ≈ 0.15 (institutional beneficiaries with high exit options experience minimal extraction against them). Treaty constraints apparatus: victim + powerless + trapped → d ≈ 0.92 (mechanisms designed to constrain face maximum extraction when those constraints are dismantled by the very procedures meant to entrench them). Imperial authority: degraded beneficiary + institutional + arbitrage (constrained by amendment power) → d ≈ 0.65 (institutional actor losing extractive power). Constitutional transition analyst: observer position → d ≈ 0.72 (analytical neutrality with recognition of generational structural shift). The perspectival gap is maximal: Irish republicans see beneficial coordination (Tangled Rope/Scaffold), Westminster sees harmless dominion integration (Rope), the Treaty apparatus sees existential extraction (Snare), and imperial authority sees degraded ceremonialism (Piton). This gap reveals the constraint's core paradox: the same constitutional form that provides institutional scaffolding also suppresses republican identity, and the amendment mechanism meant to entrench constraints enables their systematic removal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading's structural uniqueness: the Irish Free State is a Westminster export that instantiates the anti-imperial reading of the dominion form. Unlike the Australian Washminster hybrid (which fused Westminster with Washington to create a novel form) or the Canadian codification (which imported Westminster into federal structure), the Irish reading systematically removes the imperial tethers while maintaining Westminster institutional structure. Mandatrophy is resolved by recognizing that the tangled rope classification reflects genuine coordination (Westminster scaffolding) and genuine extraction (republican suppression) coexisting, and that the extractive mechanism is not stable — it is designed to be removed through the amendment process. The constraint does not ask whether dominion constitutionalism is coordination or extraction; it demonstrates that it is both, and that the Irish case weaponizes the form to tip toward republican exit. The rising extractiveness over the interval (0.48 → 0.58 → 0.72) reflects this transformation: the constraint begins as mixed coordination-extraction and becomes primarily extractive (in reverse) as the amendment apparatus is used to dissolve imperial constraints. By 1937, the original constraint (dominion form as imperial tether) has been entirely replaced. The piton classification of imperial authority reflects that the enforcement machinery (crown, oath, appeal) survives after functional power is gone, persisting through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_amendment_scope_ambiguity,
    'Did the Treaty grant the Irish Free State the power to amend itself out of its own constraining provisions, or did the attempt to do so constitute a breach of the Treaty?',
    'Historical analysis of imperial legal opinion and Irish legal arguments 1922-1936; examination of whether Westminster would have had grounds to challenge amendment validity; comparison with Australian and Canadian amendment precedents.',
    'If amendment power was genuine: the Treaty contained the seeds of its own dissolution (Tangled Rope is correct — genuine coordination AND genuine extraction coexist). If amendment power was contested: the Free State was engaged in progressive legal nullification of the Treaty (classification shifts toward Snare). If power was ambiguous: the constraint''s classification hinges on which party''s reading wins (kernel reading contest is empirically underdetermined).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_amendment_scope_ambiguity, conceptual, 'Whether the Treaty granted amendment power that could dissolve its own constraints').

omega_variable(
    westminster_model_functional_necessity,
    'Was the dominion constitution functionally necessary for Irish state-building 1922-1937, or was it chosen for imperial leverage despite alternative indigenous models being available?',
    'Counterfactual analysis: what were the alternative institutional paths available to the Irish Free State? Did any dominion reject Westminster form and succeed? What was the actual vs. theoretical cost of writing a novel constitution during civil war recovery?',
    'If functionally necessary: the constraint shows high coordination value (extractiveness lower; classification more Rope-leaning). If chosen despite alternatives: the constraint is more purely extractive (extractiveness higher; classification more Snare-leaning). If ambiguous: extractiveness reflects the genuine mixed coordination-extraction, supporting Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westminster_model_functional_necessity, empirical, 'Whether Westminster model was functionally necessary or chosen despite alternatives').

omega_variable(
    suppression_mechanism_transformation,
    'Did the Oath and crown symbols suppress genuine Irish republican sentiment, or did they formalize a legitimate imperial relationship that Irish leaders (at least temporarily) accepted as the price of independence?',
    'Analysis of Irish political rhetoric 1922-1937; examination of whether the Oath was presented as temporary negotiated settlement vs. permanent constraint; assessment of public opinion trajectories; comparison of suppression indicators before/after repeal (1933-1937).',
    'If genuine suppression: the constraint''s extractiveness and suppression scores are justified; beneficiary/victim classifications reflect real asymmetry. If formalized settlement: the extractiveness is overstated; the Free State government was genuinely accepting terms in 1922, and the later amendment reflected preference shift rather than constraint removal. If mixed: the suppression mechanism itself changed over time (T=0: formalized settlement accepted; T=15: perceived as extractive suppression to be repealed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_transformation, empirical, 'Whether Oath suppressed sentiment or formalized accepted settlement').

omega_variable(
    kernel_reading_contest,
    'Is the Irish Free State dominion constitution best understood as a Westminster export (sibling reading with Australian, Canadian, decolonization templates), or as a unique anti-imperial reading that weaponizes the export form to exit the empire?',
    'Comparative institutional analysis: does the Irish case follow the pattern of other Westminster exports (gradual devolution of power, growing autonomy within imperial framework), or does it follow a radically different trajectory (systematic amendment to remove imperial tethers, replacement with novel republican form)? What explains the difference — Irish revolutionary mandate, treaty civil war, or structural properties of Westminster form itself?',
    'If Irish case is a Westminster export variant: the reading coexists with Australian and Canadian readings as different paths within the dominion framework. If Irish case is anti-imperial reading: the reading forecloses the imperial-continuity readings, revealing the kernel contest as a contest over whether dominion constitutionalism can be weaponized for exit. If ambiguous: the kernel is genuinely under-determined and the reading contest reflects real structural indeterminacy in what dominion constitutionalism means.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Irish Free State is Westminster export variant or anti-imperial weaponization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_export_constitutions__irish_free_state_1922, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ifs_theater_treaty_1922, westminster_export_constitutions__irish_free_state_1922, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ifs_theater_midpoint_1929, westminster_export_constitutions__irish_free_state_1922, theater_ratio, 7, 0.68).
narrative_ontology:measurement(ifs_theater_repeal_1937, westminster_export_constitutions__irish_free_state_1922, theater_ratio, 15, 0.85).

% Extraction over time
narrative_ontology:measurement(ifs_extract_treaty_1922, westminster_export_constitutions__irish_free_state_1922, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ifs_extract_midpoint_1929, westminster_export_constitutions__irish_free_state_1922, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(ifs_extract_repeal_1937, westminster_export_constitutions__irish_free_state_1922, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ifs_suppress_treaty_1922, westminster_export_constitutions__irish_free_state_1922, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(ifs_suppress_midpoint_1929, westminster_export_constitutions__irish_free_state_1922, suppression_requirement, 7, 0.72).
narrative_ontology:measurement(ifs_suppress_repeal_1937, westminster_export_constitutions__irish_free_state_1922, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_export_constitutions__irish_free_state_1922, enforcement_mechanism).
narrative_ontology:affects_constraint(westminster_export_constitutions__irish_free_state_1922, westminster_export_constitutions__australian_federation_1901).
narrative_ontology:affects_constraint(westminster_export_constitutions__irish_free_state_1922, westminster_export_constitutions__canadian_confederation_1867).
narrative_ontology:affects_constraint(westminster_export_constitutions__irish_free_state_1922, westminster_export_constitutions__decolonization_constitutions).

% DUAL FORMULATION NOTE:
% The Irish Free State reading is upstream in the network: it demonstrates that Westminster export constitutions can be weaponized for imperial exit through amendment procedures. The decolonization_constitutions reading is downstream: Lancaster House texts exported to post-1945 colonies follow the Irish precedent (constitutional form accepted, later amended or replaced). Australian and Canadian readings are lateral: they demonstrate alternative paths (Washminster hybrid and Westminster codification) that do not follow the Irish trajectory. All four readings share the kernel (dominion constitutionalism) but diverge structurally in how the export form relates to imperial authority. The Irish reading's extractiveness (0.58, rising to 0.72) reflects that the constraint's function reverses: initially a mechanism of imperial binding, it becomes a mechanism of imperial exit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westminster_export_constitutions__irish_free_state_1922, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
