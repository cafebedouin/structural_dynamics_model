% ============================================================================
% CONSTRAINT STORY: revolutionary_constitutionalism__french_constitution_1791
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolutionary_constitutionalism__french_constitution_1791, []).

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
 *   constraint_id: revolutionary_constitutionalism__french_constitution_1791
 *   human_readable: French Constitution of 1791: Revolutionary Rupture and Consumed Extraction
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The French Constitution of 1791 is the revolutionary constitution as
 *   rupture — the attempt to rebuild the state from first principles of
 *   declared natural rights, enumerating the inviolable rights of man and
 *   citizen and the structure of legitimate authority. It is also the
 *   revolutionary constitution that consumed itself within a year, collapsing
 *   into the radical purge of the Jacobin dictatorship and ultimately into
 *   Napoleonic reconstruction. This story instantiates ONE READING of the
 *   contested kernel 'revolutionary constitutionalism': the reading that
 *   emphasizes rupture, the enumeration of rights, the brief window of
 *   bourgeois-liberal constitutional order, and the inherent instability that
 *   consumed it. This reading foregrounds the constraint's extraction of
 *   political power from the majority (passive citizens, clergy,
 *   sans-culottes) and redirection of that power to the active citizen
 *   propertied class. The Constitution declares universal rights while
 *   systematically restricting their exercise. It claims to embody natural
 *   law while serving identifiable class interests. It enumerates constraints
 *   on power while arming the Assembly with tools to bypass those
 *   constraints. The extractiveness trajectory (0.42 → 0.72) reflects the
 *   accelerating gap between declared rights and actual suppression as the
 *   Constitution's contradictions intensified: the royal family's attempted
 *   flight, the Assembly's accumulation of emergency powers, the growing
 *   pressure from excluded groups, the foreign invasion threat, all of which
 *   drove suppression ever higher until the framework collapsed.
 *
 * KEY AGENTS:
 *   - Active Citizens and Propertied Class: Primary beneficiary (institutional/arbitrage) — the Constitution consolidates their property rights, voting power, administrative control
 *   - Passive Citizens: Primary victim (powerless/trapped) — explicitly denied voting rights and political participation via property qualification; no exit from the constitutional framework
 *   - Dispossessed Clergy: Secondary victim (powerful/constrained at start, degrading to trapped) — stripped of property and institutional authority; can exit by emigration but at catastrophic cost to family, fortune, legitimacy
 *   - Revolutionary Legislative Assembly: Both coordinator and extractor (institutional/constrained) — enacts the Constitution (coordination) while using it to consolidate Assembly power against King and popular pressure (extraction)
 *   - Louis XVI and Crown: Constrained victim (powerful/constrained) — nominally preserved as constitutional monarch but stripped of substantive power; exit is impossible without regime collapse
 *   - Republican Coalition and Radical Clubs: Organized challengers (organized/mobile) — view the Constitution as a temporary scaffold with explicit revision clauses; mobilize pressure for expanded franchise
 *   - Foreign Powers and Émigrés: External pressure vectors — use the Constitution's weakness as entry point for intervention and attempt to restore ancien régime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolutionary_constitutionalism__french_constitution_1791, 0.58).
domain_priors:suppression_score(revolutionary_constitutionalism__french_constitution_1791, 0.78).
domain_priors:theater_ratio(revolutionary_constitutionalism__french_constitution_1791, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolutionary_constitutionalism__french_constitution_1791, extractiveness, 0.58).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__french_constitution_1791, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__french_constitution_1791, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolutionary_constitutionalism__french_constitution_1791, tangled_rope).
narrative_ontology:human_readable(revolutionary_constitutionalism__french_constitution_1791, "French Constitution of 1791: Revolutionary Rupture and Consumed Extraction").
narrative_ontology:topic_domain(revolutionary_constitutionalism__french_constitution_1791, "political/legal/constitutional").

domain_priors:requires_active_enforcement(revolutionary_constitutionalism__french_constitution_1791).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolutionary_constitutionalism__french_constitution_1791, '3aa988d8-712d-497c-a7b4-e4232459346b').
narrative_ontology:cs_kernel_codification('3aa988d8-712d-497c-a7b4-e4232459346b', fixed_text).
narrative_ontology:cs_authority_grounding('3aa988d8-712d-497c-a7b4-e4232459346b', extraction).
narrative_ontology:cs_interpretation_layer_present('3aa988d8-712d-497c-a7b4-e4232459346b').
narrative_ontology:cs_reading_relation('3aa988d8-712d-497c-a7b4-e4232459346b', revolutionary_constitutionalism__soviet_constitution_1936, coexists_with).
narrative_ontology:cs_reading_relation('3aa988d8-712d-497c-a7b4-e4232459346b', revolutionary_constitutionalism__us_constitution, influences).
narrative_ontology:cs_axiom('3aa988d8-712d-497c-a7b4-e4232459346b', foundational, universal_rights_enumeration_binds_authority).
narrative_ontology:cs_axiom_status(universal_rights_enumeration_binds_authority, holdable).
narrative_ontology:cs_axiom_grounding('3aa988d8-712d-497c-a7b4-e4232459346b', universal_rights_enumeration_binds_authority, deontological).
narrative_ontology:cs_axiom('3aa988d8-712d-497c-a7b4-e4232459346b', foundational, property_qualification_legitimate_franchise_restriction).
narrative_ontology:cs_axiom_status(property_qualification_legitimate_franchise_restriction, overridden).
narrative_ontology:cs_axiom_grounding('3aa988d8-712d-497c-a7b4-e4232459346b', property_qualification_legitimate_franchise_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('3aa988d8-712d-497c-a7b4-e4232459346b', natural_rights_constitutionalism).
narrative_ontology:cs_drift_state('3aa988d8-712d-497c-a7b4-e4232459346b', month_twelve_august_1792, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('3aa988d8-712d-497c-a7b4-e4232459346b', '').
narrative_ontology:cs_kernel_id(revolutionary_constitutionalism__french_constitution_1791, revolutionary_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolutionary_constitutionalism__french_constitution_1791, active_citizens_propertied_class).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__french_constitution_1791, passive_citizens).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__french_constitution_1791, dispossessed_clergy).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__french_constitution_1791, ancien_regime_nobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE CITIZENS & DISPOSSESSED CLERGY (SNARE) — Explicitly stripped of voting rights via property qualification; clergy stripped of institutional power and property. The Constitution declares universal rights while systematically denying exercise of those rights to these groups. Exit is impossible — the constraint is embedded in the legal framework itself. No exit option, maximum suppression, maximum experienced extraction.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PETTY BOURGEOISIE & MINOR OFFICEHOLDERS (TANGLED ROPE) — Benefit from expanded administrative opportunities and the abolition of feudal dues, but face constraint from the active citizen property threshold that blocks most from the National Guard and electoral participation. Mixed: genuine gains from the revolutionary restructuring, but also extraction through exclusion from political franchise. The constraint both enables and constrains their mobility.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACTIVE CITIZENS & PROPERTIED CLASS (ROPE) — Primary beneficiaries. The Constitution enumerates rights designed to consolidate their position: property protection, voting power, control of local administration, abolition of feudal obligations. They experience the constraint as pure coordination — the written framework coordinates the new order in their favor. Net benefit with low perceived extraction; the framework serves their interests by design.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REVOLUTIONARY LEGISLATIVE ASSEMBLY (TANGLED ROPE) — Genuine coordination function: the Constitution operationalizes the revolutionary project by formalizing the separation of powers and establishing rules for lawmaking. But also extracts: the Assembly uses the Constitution to consolidate its own power against both the King and popular pressure. The framework serves both coordination (necessary institutional structure) and extraction (Assembly supermajority, restricted suffrage that excludes pressure from below). Theater ratio reflects the gap between declared universal rights and narrow actual franchise.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOUIS XVI & RESIDUAL CROWN AUTHORITY (SNARE) — The Constitution nominally preserves the monarchy but strips the Crown of substantive power: taxation authority, legislative veto, appointment control all circumscribed. The King is constitutionally subordinate to the Assembly. Exit is constrained — abdication is theoretically possible but politically catastrophic (family safety, legitimacy). The Crown is trapped in a framework designed to neutralize it. Maximum suppression with high extraction.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REPUBLICAN COALITION & RADICAL CLUBS (SCAFFOLD) — The Constitution is perceived as a temporary coordination mechanism with an explicit sunset clause embedded in Article III: the Constitution is declared revocable by the next regular constitutional convention. Radical republicans see a transitional framework that will expire and be replaced by a more democratic successor. The scaffold classification reflects the built-in revision mechanism; low effective extraction because organized actors have an explicit exit path via constitutional amendment.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: THE CONSTITUTIONAL FORM ITSELF (PITON) — From a civilizational perspective, the Constitution persists as an institutional form (preamble invoked, structure referenced) but with severely degraded functional capacity: the framework could not prevent royal flight, could not integrate popular pressure, could not reconcile contradictions between declared rights and restricted franchise. The Constitution is maintained through theater (ceremonial invocation of the sacred text) while actual power flows through parallel structures (Assembly supermajority, mob pressure, King's private counsels). Theater ratio 0.65 reflects this gap.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, the Constitution might be read as embodying immutable principles of natural law and natural rights — that all humans possess inalienable rights independent of state grant, that legitimate authority derives from consent of the governed. This perspective sees the Constitution as discovering and enumerating universal truths rather than constructing a contingent political order. However, the structural data reveals this as a false summit: the Constitution's own design (property qualification, passive citizens, clergy dispossession) contradicts the universal claims it makes. The natural law reading naturalizes a particular class interest.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolutionary_constitutionalism__french_constitution_1791_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__french_constitution_1791, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(revolutionary_constitutionalism__french_constitution_1791, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolutionary_constitutionalism__french_constitution_1791, TR),
    TR >= 0.70.

:- end_tests(revolutionary_constitutionalism__french_constitution_1791_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 base, increasing to 0.72): The Constitution extracts political power from the majority and concentrates it in the active citizen propertied minority — approximately 10% of the adult population. This is not merely a coordination problem but a deliberate structural choice: voting qualifications, National Guard enrollment, taxation power, all designed to advantage the propertied class. The extractiveness increases over the 12-month interval because the contradictions between declared universal rights and restricted franchise create mounting pressure that the framework cannot contain. As passive citizens, sans-culottes, and radical clubs demand inclusion, the Assembly responds by intensifying suppression (increasing martial law declarations, restrictions on assembly, controls on press), which raises the extractiveness measure: extraction is no longer subtle coordination difference but overt coercion. Suppression (0.78 base, increasing to 0.85): Very high. The Constitution explicitly restricts political participation via property qualification; it subordinates the Crown and any potential alternative power source; it arms the Assembly with emergency powers to suppress challenges from below. The suppression trajectory reflects the accelerating need for coercive enforcement as the Constitution's class basis becomes ever more visible. By the end of the interval (month 12, roughly August 1792), martial law is the de facto norm, passive citizens are being hunted by authorities, foreign war is providing justification for emergency powers. Theater ratio (0.52 → 0.78): Rising. The Constitution's performative content increases as its functional capacity declines. The formal structure remains in place (National Assembly meets, laws are enacted per constitutional procedure) but actual power is increasingly concentrated in Assembly supermajorities and emergency committees. By the end of the interval, the Constitution is functionally dead but ceremonially invoked — a classic piton signature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR types across its perspectives, revealing the structure underneath the constitutional language. The propertied beneficiary class genuinely perceives the Constitution as coordination — it solves the problem of consolidating the revolutionary gains, establishing property rights, allocating power through the Assembly. The passive citizens perceive it as a snare — they are explicitly named in the Constitution ('passive citizen' is an enumerated status), stripped of voting rights, offered universal rights they cannot exercise. The radical clubs perceive it as a scaffold — the Constitution itself declares it revocable, and they organize to force its revision toward universal suffrage. The residual Crown perceives it as a snare — nominally preserved but substantively powerless. The Legislative Assembly perceives it as tangled rope — the Constitution enables their coordination (lawmaking procedure, separation of powers) while allowing them to accumulate emergency powers that exceed constitutional bounds. The analytical observer at civilizational scope risks reading it as natural law (mountain) — discovering immutable principles of human rights — but this reading naturalizes what is actually a particular class interest embedded in property qualification and franchise restriction. The constitutionalism perspective, divorced from context, makes the document appear universal; the structural analysis reveals it as a bounded order serving specific interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) depends on the agent's structural position relative to the constraint. The sigmoid directionality function f(d) computes how the base extractiveness (ε = 0.58) scales for each agent based on their power level, exit options, and beneficiary/victim status. Passive citizens and dispossessed clergy are trapped victims with no exit (d ≈ 0.95) — they experience maximum χ as the constraint extracts political authority and property from them. Petty bourgeoisie face constrained exit with mixed beneficiary/victim status (d ≈ 0.60) — they gain from abolished feudal dues but lose from franchise restriction, experiencing moderate-to-high χ. Active citizens are beneficiaries with arbitrage options (d ≈ 0.15) — the constraint subsidizes their position, they experience low or negative χ. The Crown is a powerful actor facing constrained exit with victim status (d ≈ 0.75) — even with monarchical prestige, the constitutional subordination is extractive. Radical clubs are organized actors with mobile options (d ≈ 0.45) — they perceive the constraint as extractive but temporary, with clear paths to revision. The core insight: the same constraint produces radically different χ values for different agents because their structural relationships to it are different. The beneficiary (active citizen) experiences it as coordination (Rope); the victim (passive citizen) experiences it as pure extraction (Snare); the transitional actor (Assembly) experiences it as mixed (Tangled Rope); the organized challenger (radical club) experiences it as temporary (Scaffold).
 *
 * MANDATROPHY ANALYSIS:
 *   The 1791 Constitution is a 'consumed rupture' exemplar because its classification is not mandatrophic ambiguity but multiple coherent truths from different positions. It genuinely IS coordination (Rope) for the active citizens — it establishes rule of law, property rights, stable government. It genuinely IS extraction (Snare) for passive citizens and clergy — it strips them of power and property via enumerated constitutional clause. It genuinely IS scaffolding (Scaffold) for radical republicans — it explicitly declares itself revocable and is perceived as transitional. The mandatrophy resolution is to recognize that all classifications are correct given their perspectives; the question is not 'what type is the Constitution really?' but 'why does it function as all six types simultaneously?' The answer is that the constraint's extractiveness is built on a buried premise: that property qualification is legitimate, that passive citizens exist as a constitutional category, that clergy can be expropriated by state decree. Once that premise is challenged (as radical clubs challenge it), the whole structure becomes unstable. The Constitution consumes itself because it tries to enumerate universal rights while restricting their exercise to a propertied minority — a logical contradiction that no amount of structural reinforcement can suppress. The teatre ratio rising from 0.52 to 0.78 models the increasing performative gap: the Constitution remains on paper and in ceremony, but actual governance flows through emergency committees and mob pressure. The final collapse (month 12, the royal family's flight and the storming of the Tuileries in August 1792) is the moment when the contradiction becomes explicit and the beneficiary class loses the military/police power to maintain suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_framing,
    'Is the 1791 Constitution a genuine rupture with the ancien régime, or a masked continuation of feudal property hierarchies under new legal language?',
    'Comparative analysis of property rights, taxation authority, administrative structure before and after 1791. Examine whether feudal extraction mechanisms (corvée, tithes, feudal dues) were abolished or redirected into new institutional forms.',
    'If genuine rupture: Constitution is a clean coordination mechanism (Rope from more perspectives). If masked continuity: extractiveness is higher (the constraint redirects extraction rather than reducing it), classification shifts toward Snare and away from Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_vs_continuity_framing, conceptual, 'Whether the 1791 Constitution represents genuine rupture or masked continuity of extraction').

omega_variable(
    active_citizen_suffrage_rationality,
    'Was the active citizen property qualification designed as a temporary transition mechanism to educate passive citizens for democratic participation, or as a permanent mechanism to guarantee bourgeois class control?',
    'Historical record: statements by constitutional framers about the intention to expand the franchise; comparative analysis with contemporary democratic theory; examination of whether constitutional amendments during 1792-1793 expanded or contracted the franchise.',
    'If temporary/pedagogical: the scaffold classification holds (sunset logic, organized paths to franchise expansion). If permanent: the classification shifts to Tangled Rope or Snare with clearer extraction intent; beneficiary intent is revealed as class consolidation rather than popular sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_citizen_suffrage_rationality, conceptual, 'Whether active citizen qualification was temporary transition or permanent class control mechanism').

omega_variable(
    written_constitution_effectiveness,
    'Does a written enumeration of rights actually constrain state power, or does it serve primarily as a legitimacy claim that can be superseded by de facto power structures?',
    'Longitudinal institutional analysis: correlation between constitutional constraints and actual executive/legislative behavior. Examine cases where de facto power violated constitutional provisions (royal flight, Assembly overreach, Jacobin dictatorship) and whether the written framework actually prevented or merely delayed violation.',
    'If effective: Constitution represents genuine coordination mechanism with real constraint force (Rope classification more defensible). If primarily legitimacy theater: extractiveness is higher, classification shifts toward Piton or Snare; the written rights are revealed as performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_constitution_effectiveness, empirical, 'Whether written constitutional constraints actually bind state power').

omega_variable(
    revolutionary_consumption_mechanism,
    'Why did the 1791 Constitution consume itself within a year? Was it: (a) internal contradictions between universal rights claims and restricted franchise forcing radicalization? (b) external pressure from émigré powers and foreign intervention? (c) structural instability in the separation-of-powers design? (d) incompatibility between constitutional order and the actual distribution of organized force (military, mob)?',
    'Temporal analysis of constitutional collapse: track which provisions failed first, what triggered each failure, which agents drove the transitions (Legislative Assembly actions, mob pressure, foreign intervention, King''s actions). Correlate with measurement data on suppression and extractiveness during the interval.',
    'If (a): the constraint''s own logic produces self-destruction (extractiveness from restricted franchise builds pressure that explodes the framework). If (b): the constraint''s effectiveness depends on external security conditions. If (c): design flaw suggests lower durability of this constitutional reading vs sibling readings. If (d): extractiveness measure underestimates actual suppression because it doesn''t account for latent force distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revolutionary_consumption_mechanism, empirical, 'Root cause of the Constitution''s collapse within one year').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the French 1791 Constitution logically foreclose the Soviet 1936 reading, or merely influence its form? Does it coexist with the US Constitution reading, or do they foreclose each other?',
    'Comparative constitutional analysis. Examine whether the 1791 reading''s core premises (that a written enumeration of rights can constrain power, that property qualification is legitimate, that separation of powers prevents tyranny) are logically incompatible with 1936 premises (that rights are conditional on state grant, that party apparatus supersedes written law, that centralized power is necessary) or merely different policy choices. Same question for US (durable, amendments absorb transformation) vs FR (consumed within a year).',
    'If 1791 forecloses 1936: the readings are exclusive within any single framework (either written rights constrain or they don''t). If coexist: they are alternative readings held by different parties (both live possibilities). Classification of reading_relations determines the engine''s constraint coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Logical relationship between French 1791 reading and sibling constitutional readings').

omega_variable(
    natural_law_vs_construction,
    'Does the 1791 Constitution discover pre-existing natural rights (mountain perspective), or construct a new political order that happens to invoke natural rights language (Tangled Rope/Snare perspective)?',
    'Philosophical and historical analysis. Examine the Enlightenment intellectual context (Rousseau, Montesquieu, Locke) that framed natural rights as pre-political. Examine whether the Constitution treats rights as pre-existing constraints on sovereignty or as state-granted privileges. Track institutional design choices (property qualification, passive citizen status, clergy dispossession) and ask whether they follow from natural law logic or contradict it.',
    'If natural law discovery: the mountain classification is defensible; the constraint is immutable. If construction: the mountain is a false summit; the constraint is contingent and benefits identifiable agents (active citizens, propertied class). FSM engine signature fires if beneficiaries are declared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_construction, conceptual, 'Whether 1791 Constitution discovers natural law or constructs contingent political order').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolutionary_constitutionalism__french_constitution_1791, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr1791_theater_t0, revolutionary_constitutionalism__french_constitution_1791, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fr1791_theater_t6, revolutionary_constitutionalism__french_constitution_1791, theater_ratio, 6, 0.65).
narrative_ontology:measurement(fr1791_theater_t12, revolutionary_constitutionalism__french_constitution_1791, theater_ratio, 12, 0.78).

% Extraction over time
narrative_ontology:measurement(fr1791_extract_t0, revolutionary_constitutionalism__french_constitution_1791, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fr1791_extract_t6, revolutionary_constitutionalism__french_constitution_1791, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fr1791_extract_t12, revolutionary_constitutionalism__french_constitution_1791, base_extractiveness, 12, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fr1791_suppress_t0, revolutionary_constitutionalism__french_constitution_1791, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fr1791_suppress_t6, revolutionary_constitutionalism__french_constitution_1791, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(fr1791_suppress_t12, revolutionary_constitutionalism__french_constitution_1791, suppression_requirement, 12, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolutionary_constitutionalism__french_constitution_1791, enforcement_mechanism).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__french_constitution_1791, revolutionary_constitutionalism__soviet_constitution_1936).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__french_constitution_1791, revolutionary_constitutionalism__us_constitution).

% DUAL FORMULATION NOTE:
% The French 1791 Constitution is one reading of the contested kernel 'revolutionary constitutionalism.' The sibling readings (Soviet 1936, US Constitution) are separate constraint stories with different epsilon values, beneficiary structures, and lifecycle patterns. This story foregrounds the 1791 reading's unique characteristics: rupture via enumeration, consumption via internal contradiction, extraction redirected from feudal to bourgeois authority. The network links all three readings as members of the constraint family 'revolutionary constitutionalism,' enabling comparative analysis of how different revolutionary projects instantiate constitutional authority differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
