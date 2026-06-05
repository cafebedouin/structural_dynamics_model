% ============================================================================
% CONSTRAINT STORY: westminster_export_constitutions__australian_federation_1901
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_export_constitutions__australian_federation_1901, []).

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
 *   constraint_id: westminster_export_constitutions__australian_federation_1901
 *   human_readable: Washminster Hybrid: Westminster Responsible Government Inside US Federal Structure
 *   domain: constitutional_law/political_institutions
 *
 * SUMMARY:
 *   Australia's 1901 federation created an unprecedented constitutional
 *   fusion: Westminster responsible government (the Crown's ministers must
 *   retain the confidence of the lower house to control supply and govern
 *   day-to-day) was embedded inside a US-style federal structure (a bicameral
 *   parliament with an elected Senate representing states equally, not by
 *   population, holding genuine veto power). This 'Washminster' hybrid was
 *   not accidental — it was a deliberate design choice to solve two
 *   simultaneous problems: how to create a federal system that would bind the
 *   colonies into a nation-state while preserving Westminster's principle
 *   that the elected representatives could hold the executive accountable
 *   through supply control. The tension between these two logics —
 *   Westminster's confidence mechanism and federalism's coordinate
 *   sovereignty — creates a structural constraint that has proven impossible
 *   to resolve cleanly. The 1975 constitutional crisis, when the
 *   Governor-General dismissed the Prime Minister after the Senate blocked
 *   supply, exposed that the Westminster confidence doctrine cannot function
 *   when the second chamber has genuine veto power and is not subject to a
 *   confidence vote. This reading instantiates ONE interpretation of the
 *   Westminster export kernel: that Australia deliberately fused the two
 *   models, accepting the inherent tension as the price of federal stability.
 *   Other readings (Canadian confederation, Irish independence,
 *   decolonization constitutions) chose different resolutions — Canada
 *   imported Westminster without the federalist veto power, Ireland
 *   eventually discarded Westminster form entirely, decolonization states
 *   often inherited the template but rewrote it.
 *
 * KEY AGENTS:
 *   - The Federal Parliament (both chambers as institution): Coordinates legislative governance; extracts through dual legitimacy; beneficiary of bicameral passage
 *   - The States (organized through Senate): Preserve autonomy; benefit from federal balance mechanism; constrained by federal power
 *   - The Executive/Prime Minister: Governs through confidence mechanism; victim of Senate supply blockade; powerful but extraction-constrained
 *   - The Pure Westminster Confidence Doctrine: Embedded constitutional principle; trapped by the Senate's veto power; victim of structural incompatibility
 *   - The Senate as Second Chamber: Represents federalism; blocks supply; extracts policy concessions; benefits from the hybrid structure
 *   - The Lower House/House of Representatives: Governs day-to-day under confidence principle; suppressed by Senate veto; but sole supplier of ministers
 *   - The Analytical Observer (Universal Perspective): Sees the contradiction as immutable; at risk of false summit naturalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_export_constitutions__australian_federation_1901, 0.38).
domain_priors:suppression_score(westminster_export_constitutions__australian_federation_1901, 0.52).
domain_priors:theater_ratio(westminster_export_constitutions__australian_federation_1901, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_export_constitutions__australian_federation_1901, extractiveness, 0.38).
narrative_ontology:constraint_metric(westminster_export_constitutions__australian_federation_1901, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westminster_export_constitutions__australian_federation_1901, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_export_constitutions__australian_federation_1901, tangled_rope).
narrative_ontology:human_readable(westminster_export_constitutions__australian_federation_1901, "Washminster Hybrid: Westminster Responsible Government Inside US Federal Structure").
narrative_ontology:topic_domain(westminster_export_constitutions__australian_federation_1901, "constitutional_law/political_institutions").

domain_priors:requires_active_enforcement(westminster_export_constitutions__australian_federation_1901).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_export_constitutions__australian_federation_1901, 'de6b2630-c327-4486-931b-017d019b33d2').
narrative_ontology:cs_kernel_codification('de6b2630-c327-4486-931b-017d019b33d2', formalized).
narrative_ontology:cs_authority_grounding('de6b2630-c327-4486-931b-017d019b33d2', extraction).
narrative_ontology:cs_interpretation_layer_present('de6b2630-c327-4486-931b-017d019b33d2').
narrative_ontology:cs_reading_relation('de6b2630-c327-4486-931b-017d019b33d2', westminster_export_constitutions__canadian_confederation_1867, coexists_with).
narrative_ontology:cs_reading_relation('de6b2630-c327-4486-931b-017d019b33d2', westminster_export_constitutions__irish_free_state_1922, influences).
narrative_ontology:cs_reading_relation('de6b2630-c327-4486-931b-017d019b33d2', westminster_export_constitutions__decolonization_constitutions, influences).
narrative_ontology:cs_axiom('de6b2630-c327-4486-931b-017d019b33d2', foundational, bicameral_federalism_tension_is_productive).
narrative_ontology:cs_axiom_status(bicameral_federalism_tension_is_productive, overridden).
narrative_ontology:cs_axiom_grounding('de6b2630-c327-4486-931b-017d019b33d2', bicameral_federalism_tension_is_productive, deontological).
narrative_ontology:cs_axiom('de6b2630-c327-4486-931b-017d019b33d2', foundational, elected_senate_represents_federalism_adequately).
narrative_ontology:cs_axiom_status(elected_senate_represents_federalism_adequately, holdable).
narrative_ontology:cs_axiom_grounding('de6b2630-c327-4486-931b-017d019b33d2', elected_senate_represents_federalism_adequately, conventional).
narrative_ontology:cs_reference_frame('de6b2630-c327-4486-931b-017d019b33d2', bicameral_westminster_federalism_synthesis).
narrative_ontology:cs_drift_state('de6b2630-c327-4486-931b-017d019b33d2', post_1975_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('de6b2630-c327-4486-931b-017d019b33d2', '2026-02-26T14:22:33Z').
narrative_ontology:cs_kernel_id(westminster_export_constitutions__australian_federation_1901, westminster_export_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__australian_federation_1901, federal_balance_mechanism).
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__australian_federation_1901, state_autonomy_preservation).
narrative_ontology:constraint_victim(westminster_export_constitutions__australian_federation_1901, pure_westminster_confidence_chamber_doctrine).
narrative_ontology:constraint_victim(westminster_export_constitutions__australian_federation_1901, parliamentary_supply_control_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PURE WESTMINSTER CONFIDENCE CHAMBER DOCTRINE (SNARE) — The constitutional principle that a government must retain the confidence of the lower chamber to control supply is structurally trapped in a bicameral system where the upper chamber can block supply without losing a confidence vote. The 1975 Australian crisis demonstrated that the Senate can extract its demands by weaponizing supply blockade while the government cannot dissolve the Senate or force confidence votes upon it. The doctrine cannot exit this trap — it is embedded in the constitutional text. Maximum experienced extraction: the confidence mechanism is suppressed by the second chamber's veto power.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE STATES (ORGANIZED/CONSTRAINED) (ROPE) — The states, coordinating through the Senate, experience the hybrid as a coordination mechanism that preserves their autonomy without completely paralyzing the federal government. The Senate represents state interests and can block federal supply overreach, but states cannot completely escape federal authority. Exit is constrained by the constitutional structure (they cannot amend the Constitution unilaterally), but the coordination is genuine — the hybrid solves a real collective action problem: how to keep the federal government from absorbing state power while maintaining a functioning national government. The beneficiary is the federal balance itself.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXECUTIVE (POWERFUL/MOBILE) (TANGLED ROPE) — The Prime Minister and Cabinet experience both coordination and extraction. They coordinate with both chambers to pass legislation, but the Senate's supply-blocking power extracts concessions: legislation the House would pass alone is compromised or abandoned. The executive has more exit options than the trapped doctrine — they can negotiate, compromise, or call elections (especially elections for House, or in rare cases trigger dual elections). But the Senate's power is real and constraining. The beneficiary/victim relationship here is asymmetric: the executive gains some coordination (legitimacy through bicameral passage) but loses supply control (extraction). Not purely snare because the executive retains negotiating power and exit options.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL PARLIAMENT AS INSTITUTION (INSTITUTIONAL/ARBITRAGE) (TANGLED ROPE) — The Parliament itself (both chambers functioning as an institution) experiences the hybrid as a genuine coordination mechanism with embedded extraction. The lower House coordinates day-to-day governance (responsible government principle); the upper Senate coordinates federalism (state representation). Together they extract legitimacy from their dual function. But the extraction mechanism is real: the Senate can extract policy concessions from the House by threatening supply blockade, and the House can extract compliance from the Senate by threatening to override it through double dissolution or political pressure. The institution survives because neither chamber can completely defeat the other. Exit is possible through constitutional amendment (arbitrage — the institution can rewrite its own rules), but this is expensive and rare.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WESTMINSTER CONVENTION LAYER (INSTITUTIONAL/ARBITRAGE) (PITON) — The unwritten conventions of responsible government that supposedly govern the Senate's use of supply power have largely degraded. The 1975 crisis exposed that the Governor-General's reserve powers (once thought to be constrained by convention) can be deployed to dismiss the government, and the Senate's supply-blocking power is limited only by political opinion, not by constitutional or conventional rule. The convention layer persists through theater: senators claim to respect norms they actually routinely violate; the fiction of 'responsible' use of Senate power continues even when the power is wielded as pure extraction. Theater ratio is high because the convention language persists in political speech even as practice has emptied it. The layer is maintained by institutional inertia — the Commonwealth still invokes Westminster language — but the functional substance is gone.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a sufficiently civilizational/universal perspective, the tension between Westminster responsible government and US-style federalism is an immutable structural property: any attempt to fuse parliamentary accountability with genuine bicameral federalism creates an inherent contradiction between supply control (Westminster logic) and second-chamber veto (federalism logic). This perspective sees the Washminster hybrid as an inevitable, unchangeable collision of two incompatible principles, not as a contingent institutional arrangement. However, the structural data contradicts the mountain classification — the 'incompatibility' is not a law of nature but a political choice to embed both principles without resolving which takes priority. The false summit framing naturalizes what is actually a deliberate and revisable design.
constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_export_constitutions__australian_federation_1901_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_export_constitutions__australian_federation_1901, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(westminster_export_constitutions__australian_federation_1901, TR),
    TR >= 0.70.

:- end_tests(westminster_export_constitutions__australian_federation_1901_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, increased from initial design 0.22 to current 0.38): The constraint's extractive power has accumulated over the federation's history as the Senate has increasingly weaponized supply blockade and the convention of restraint has eroded. In 1901, the hybrid was experienced as primarily coordinative — the Senate was expected to defer to the confidence mechanism, and supply was rarely blocked. By 1975, the Senate's extractive power was fully exposed. The measurement trajectory shows this drift: extractiveness rises as the convention layer degrades and the mechanism becomes zero-sum (Senate extracts concessions by threatening supply blockade; executive extracts legitimacy from lower-house support; neither completely defeats the other). Current 0.38 reflects that the extraction is real but not maximal — the system still functions, and neither chamber has completely suppressed the other's authority. Suppression (0.52): The confidence doctrine is substantially suppressed (supply-blocking power prevents it from operating as written); federal balance is somewhat suppressed (the executive can often override state interests through Commons majorities); but neither is completely suppressed. The increase over time (0.30 → 0.52) reflects that suppression mechanisms have become more active and explicit as the convention layer failed. Theater ratio (0.58): Moderate. Westminster responsible government language persists (the fiction that the Senate respects conventions of restraint), but the actual practice has emptied much of the theater — supply blockade is deployed openly and justified explicitly, not cloaked in claims of deference. The ratio has risen over time as the language-practice gap has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a classic tangled_rope perspectival gap. The states see coordination (rope) — the Senate genuinely preserves federal balance. The executive sees mixed coordination and extraction (tangled_rope) — they must negotiate with the Senate, sacrificing policy goals, but they retain the confidence mechanism and lower-house support. The pure Westminster confidence doctrine sees snare — its core principle (supply control as confidence mechanism) is structurally trapped and suppressed. The federal parliament as institution sees tangled_rope — both coordination and extraction, neither completely dominant. The convention layer sees piton — the Westminster language persists as theater even as the substance has degraded. The analytical observer risks seeing mountain — the incompatibility of Westminster and federalism as an immutable structural truth — but the structural data reveals this as a false summit, a choice to embed both without resolving the tension. The perspectival gap reveals that the 'tension' is not natural law but institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives based on structural position and exit options. The states (organized/constrained) are beneficiaries of federal balance — their d-value is low (around 0.35), producing negative or low effective extraction. The executive (powerful/mobile) is a partial victim of supply extraction but retains significant agency — their d-value is moderate (around 0.55). The confidence doctrine (powerless/trapped) is a complete victim with zero exit options — its d-value is maximal (around 0.95), producing maximal experienced extraction. The federal parliament (institutional/arbitrage) is simultaneously beneficiary and victim depending on which function is active — their d-value is split (beneficiary function d≈0.20, victim function d≈0.70). The convention layer (institutional/arbitrage) is an inert residue — d-value is moderate but static (around 0.50) because the institution can rewrite its own rules but chooses not to. The analytical observer at universal scope has no structural position and sees the abstract contradiction — canonical d ≈ 0.73, but this is a structural illusion because the observer is not inside the mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_blockade_doctrine_limits,
    'Is the Senate''s power to block supply a constitutional limit (inherent to bicameralism) or a convention-dependent restraint that could be overridden by the lower chamber through electoral or legislative action?',
    'Historical analysis of supply crises in other Westminster bicameral systems (UK House of Lords, Canada, etc.); comparison of formal constitutional language vs. actual practice; examination of whether lower chambers in other systems have successfully overridden upper chamber supply vetoes without amending the constitution.',
    'If inherent constitutional limit: the hybrid is unavoidably extractive (tangled_rope). If convention-dependent restraint: the state/federal balance can be rewritten through political choice without amendment (more rope-like, less snare-like in the confidence doctrine perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_blockade_doctrine_limits, empirical, 'Whether Senate supply blockade is constitutional or conventional').

omega_variable(
    federation_stability_counterfactual,
    'Did the Washminster design actually prevent federal absorption of state power (the stated beneficiary outcome), or did federation degrade for other structural reasons (economic integration, population mobility, federal tax capacity)?',
    'Comparative analysis: Australia''s state autonomy trajectory vs. US federalism (same hybrid origin) vs. Canadian federalism (Westminster-only); measurement of state revenue autonomy, regulatory capacity, and political relevance over 1901–2026.',
    'If Washminster design prevented absorption: beneficiary/victim classification is correct, and the constraint is genuinely coordinative. If federation degraded for other reasons: the Senate''s supply-blocking power is extraction, not coordination (snare vs tangled_rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_stability_counterfactual, empirical, 'Whether Washminster design actually preserved state autonomy').

omega_variable(
    id_1975_crisis_foreclosure,
    'Did the 1975 crisis foreclose the pure Westminster reading by proving that the confidence doctrine cannot function in a bicameral system, or was 1975 merely a one-off political choice that could be reversed through convention repair?',
    'Analysis of post-1975 Senate behavior: has the Senate actually exercised supply blockade power again, or has it reverted to convention-based restraint? Examination of whether political leaders (especially governments with Senate control) have explicitly renounced the crisis precedent or treated it as binding.',
    'If foreclosed: the pure Westminster reading is dead, and the Australian constitution is permanently locked into hybrid tension (supports tangled_rope/piton classifications). If convention-repairable: the readings coexist, and the choice between them remains live (influences relationship between readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_1975_crisis_foreclosure, conceptual, 'Whether 1975 foreclosed the pure Westminster reading').

omega_variable(
    exportability_and_sibling_readings,
    'Is the Washminster hybrid (Westminster inside federalism) a general exportable principle, or did it work only in Australia''s specific context (British-origin, federal ambitions, settler-colonial state), and does this difference determine how other Westminster exports relate to the Australian reading?',
    'Comparative institutional analysis: Canada (confederation with Westminster, no US-style second chamber), Ireland (dominion to republic, rejected Westminster form entirely), decolonization states (imported both Westminster and federalism, often discarded both). Identify which elements traveled and which did not.',
    'If Australia-specific: the sibling readings (Canadian, Irish, decolonization) coexist but do not foreclose each other. If general principle: some sibling readings foreclose others (e.g., if the principle requires elected federalism, decolonization variants may foreclose Canadian Westminster-only path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exportability_and_sibling_readings, conceptual, 'Whether Washminster is exportable principle or Australia-specific').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_export_constitutions__australian_federation_1901, 1, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westminster_aus_theater_1901, westminster_export_constitutions__australian_federation_1901, theater_ratio, 1, 0.35).
narrative_ontology:measurement(westminster_aus_theater_1950, westminster_export_constitutions__australian_federation_1901, theater_ratio, 50, 0.52).
narrative_ontology:measurement(westminster_aus_theater_1975, westminster_export_constitutions__australian_federation_1901, theater_ratio, 74, 0.58).

% Extraction over time
narrative_ontology:measurement(westminster_aus_extract_1901, westminster_export_constitutions__australian_federation_1901, base_extractiveness, 1, 0.22).
narrative_ontology:measurement(westminster_aus_extract_1950, westminster_export_constitutions__australian_federation_1901, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(westminster_aus_extract_1975, westminster_export_constitutions__australian_federation_1901, base_extractiveness, 74, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(westminster_aus_suppress_1901, westminster_export_constitutions__australian_federation_1901, suppression_requirement, 1, 0.3).
narrative_ontology:measurement(westminster_aus_suppress_1950, westminster_export_constitutions__australian_federation_1901, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(westminster_aus_suppress_1975, westminster_export_constitutions__australian_federation_1901, suppression_requirement, 74, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_export_constitutions__australian_federation_1901, enforcement_mechanism).
narrative_ontology:affects_constraint(westminster_export_constitutions__australian_federation_1901, senate_supply_blockade_power).
narrative_ontology:affects_constraint(westminster_export_constitutions__australian_federation_1901, governor_general_reserve_powers).
narrative_ontology:affects_constraint(westminster_export_constitutions__australian_federation_1901, federal_state_revenue_allocation).

% DUAL FORMULATION NOTE:
% The Washminster hybrid decomposes into structurally distinct constraints: (1) Westminster responsible government (confidence mechanism for supply control), (2) US-style federalism (elected second chamber with genuine veto), and (3) the collision between them (the subject of this story). Each has different ε values: responsible government ≈ 0.05 (pure coordination), federalism ≈ 0.15 (pure coordination), collision ≈ 0.38 (tangled_rope). This story models the collision. Upstream constraints include Senate supply-blocking power (ε ≈ 0.65, snare from executive perspective) and Governor-General reserve powers (ε ≈ 0.72, snare from parliamentary perspective). Downstream is federal-state revenue allocation, which inherits the tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westminster_export_constitutions__australian_federation_1901, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
