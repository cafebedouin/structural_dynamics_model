% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Regime as Impermissible Restriction on Freedom of Movement
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   The freedom_primary reading of the border_normative_status kernel asserts
 *   that freedom of movement is a fundamental human right, and that borders
 *   which restrict it are presumptively impermissible. Exclusion requires
 *   extraordinary justification that almost no contemporary border regime
 *   meets. The standing arrangement under contest is the global border
 *   regime: a system of territorial exclusion enforced by wealthy states
 *   against would-be migrants, asylum seekers, and stateless persons. From
 *   this reading, the regime extracts life chances, economic opportunity, and
 *   bodily autonomy from the globally disadvantaged, transferring them to
 *   citizenries of wealthy states and to border enforcement apparatuses. The
 *   coordination story (security, welfare state sustainability, democratic
 *   self-determination) is evaluated as cover; the empirical record shows
 *   borders harden as migration pressure rises, not as security threats
 *   materialize. The constraint is classified as a snare: pure extraction
 *   maintained by coercion, with alternatives (open borders, regional free
 *   movement, visa liberalization) actively suppressed.
 *
 * KEY AGENTS:
 *   - excluded_migrants: Primary target (powerless/trapped) — bears extraction of freedom, opportunity, life
 *   - asylum_seekers: Primary target (powerless/trapped) — bears extraction plus persecution risk
 *   - displaced_domestic_workers: Secondary target (moderate/constrained) — bears wage suppression and moral injury from racialized hierarchy
 *   - citizenries_of_wealthy_states: Primary beneficiary (institutional/arbitrage) — collects rent from exclusion, controls rule-making
 *   - border_enforcement_agencies: Agenda setter (institutional/analytical) — administers extraction, expands mission
 *   - capital_owners_in_destination_states: Secondary beneficiary (powerful/arbitrage) — captures labor arbitrage from segmented markets
 *   - stateless_persons: Excluded victim (powerless/trapped) — bears double exclusion
 *   - migration_advocates: Observer (organized/analytical) — documents violation, litigates, organizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.82).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.88).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Regime as Impermissible Restriction on Freedom of Movement").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '76adb680-b9a8-46c4-aac5-d8fec106c885').
narrative_ontology:cs_kernel_codification('76adb680-b9a8-46c4-aac5-d8fec106c885', distributed).
narrative_ontology:cs_authority_grounding('76adb680-b9a8-46c4-aac5-d8fec106c885', distributed).
narrative_ontology:cs_reading_relation('76adb680-b9a8-46c4-aac5-d8fec106c885', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('76adb680-b9a8-46c4-aac5-d8fec106c885', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('76adb680-b9a8-46c4-aac5-d8fec106c885', foundational, freedom_of_movement_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('76adb680-b9a8-46c4-aac5-d8fec106c885', freedom_of_movement_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('76adb680-b9a8-46c4-aac5-d8fec106c885', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('76adb680-b9a8-46c4-aac5-d8fec106c885', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_axiom('76adb680-b9a8-46c4-aac5-d8fec106c885', secondary, place_premium_as_unjust_extraction).
narrative_ontology:cs_axiom_status(place_premium_as_unjust_extraction, holdable).
narrative_ontology:cs_axiom_grounding('76adb680-b9a8-46c4-aac5-d8fec106c885', place_premium_as_unjust_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('76adb680-b9a8-46c4-aac5-d8fec106c885', open_borders_natural_right).
narrative_ontology:cs_drift_state('76adb680-b9a8-46c4-aac5-d8fec106c885', contemporary_border_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('76adb680-b9a8-46c4-aac5-d8fec106c885', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, citizenries_of_wealthy_states).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, capital_owners_in_destination_states).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, stateless_persons).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, freedom_of_movement_as_fundamental_human_right).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, border_abolitionism).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, equality_of_opportunity_across_borders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would-be migrants from low-income countries who are denied legal pathways to move. They bear the full cost of the border regime: wage differentials of 10x-20x (place premium), family separation, physical danger in irregular movement, exploitation in informal labor markets. Exit is trapped — they cannot change their nationality, and legal pathways are effectively closed. Some are identity_locked: their self-concept and life plans are constituted by the places they cannot reach.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% People fleeing persecution who encounter the border regime as a barrier to safety. They bear extraction plus mortal risk: pushbacks, detention, refoulement, years in camps. The regime's coordination story (refugee protection) is directly falsified by its operation — the right to seek asylum exists in law but is made practically inaccessible by enforcement. Exit is trapped: they cannot return, and forward movement is blocked.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Workers in wealthy countries who experience wage suppression and degraded conditions from labor market segmentation enforced by borders. The border regime creates a racialized, precarious migrant workforce that employers use to discipline domestic labor. These workers are structurally co-victims with migrants, but political rhetoric pits them against each other. Exit is constrained: they can vote, organize, move within the country, but cannot easily escape the national labor market structure.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Citizens of high-income states who capture the place premium — the massive wage and opportunity differential created by border exclusion. They control the political agenda on migration through democratic processes that exclude the affected. Their exit is arbitrage-grade: they can move freely to most countries, access global labor markets, and opt out of the costs of enforcement (which are socialized). They benefit from both the exclusion rent and the cheap goods/services produced by excluded labor.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, citizenries_of_wealthy_states, beneficiary,
    institutional, generational, arbitrage, global).

% The administrative and coercive apparatus that designs, implements, and expands border enforcement: CBP, ICE, Frontex, national border guards, detention systems, deportation machinery. They set the operational agenda, lobby for expanded budgets and authorities, and produce the knowledge that justifies their own expansion. Their exit is analytical: they understand the system from inside but are structurally committed to its perpetuation. They are the constraint's administrators and its most concentrated institutional beneficiary.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, global).

% Employers and investors in wealthy countries who benefit from segmented global labor markets. Borders create a reserve army of labor that cannot fully enter, depressing wages and disciplining domestic workers. They lobby for 'guest worker' programs that maximize exploitation while minimizing rights. Their exit is arbitrage-grade: capital moves freely while labor is trapped. They are secondary beneficiaries — the primary extraction flows to citizenries as political rents, but they capture economic rents.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, capital_owners_in_destination_states, beneficiary,
    powerful, biographical, arbitrage, global).

% People with no recognized nationality who bear double exclusion: denied the rights of citizens everywhere, and denied the minimal protections of the refugee regime. The border regime renders them permanently rightless. Exit is trapped in the deepest sense: there is no state that claims them, and no state that will admit them. They are the limit case of the border regime's extraction.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, stateless_persons, payer,
    powerless, generational, trapped, global).

% NGOs, lawyers, activists, scholars, and international organizations that document border violence, litigate rights violations, provide humanitarian aid, and advocate for open borders or expanded pathways. They see the full structure but cannot directly alter it. Their exit is analytical: they can leave the field, but their professional identity is often fused with the struggle. They are the analytical seat that names the constraint as snare.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migration_advocates, observer,
    organized, generational, analytical, global).

% Countries of origin that lose human capital through emigration but gain remittances. They are structurally excluded from the rule-making that determines their citizens' mobility. Some cooperate with enforcement (readmission agreements) in exchange for visa concessions or aid. Their exit is constrained: they cannot unilaterally open destination states' borders, but they can resist externalization and organize regionally (e.g., African Union free movement protocol).
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sending_states, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, citizenries_of_wealthy_states).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages cross-border movement through centralized state control, allocating territory-based rights, distributing public goods, and maintaining interstate order. Claims to solve: security threats, welfare state sustainability, democratic self-determination, cultural cohesion.
% TRANSFER_FUNCTION: Moves freedom of movement, economic opportunity (place premium), bodily safety, and life chances from would-be migrants and asylum seekers to citizenries of wealthy states and capital owners. Moves enforcement costs (taxpayer-funded) to domestic publics. Moves political agency from the globally affected to national electorates.
% ABSENT_VOICES: Would-be migrants who never reach borders (deterred by visa regimes, carrier sanctions, information barriers). Stateless persons with no state to speak for them. Future generations who will inherit a segmented world. Non-human animals and ecosystems bisected by border walls. The global poor who never get to vote on the regimes that exclude them.
% DISAPPEARANCE_RATIONALE: If the border regime vanished overnight, global labor markets would integrate, place premiums would collapse, remittances would be replaced by wages earned at destination, refugee camps would empty, detention centers would close, and the global distribution of income and opportunity would fundamentally reorganize. Wealthy states would lose their exclusion rent; capital would lose its segmented labor markets; domestic workers would gain bargaining power from global solidarity; migrants would gain full mobility. The world would rearrange radically.
% FOUNDING_PROBLEM: The Westphalian system built borders to manage interstate conflict and allocate territorial sovereignty after religious wars (1648). The modern border regime was consolidated in the late 19th/early 20th century to control labor migration (Chinese Exclusion, European emigration controls) and solidify nation-states.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars (e.g., John Mueller, Steven Pinker) document the long decline of interstate war among major powers, attributed to nuclear deterrence, economic integration, and democratic peace — not border enforcement. Migration historians (e.g., Adam McKeown, Aristide Zolberg) show that modern border controls were built for labor exclusion and racial hierarchy, not interstate security. The interstate war problem is solved; the border regime persists for extraction.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is very high: the border regime transfers the vast majority of global income variance (place premium) from would-be migrants to destination-state citizenries. Suppression (0.88) is near-maximal: walls, detention, deportation, visa regimes, carrier sanctions, and externalization (e.g., EU-Turkey deal, US Remain in Mexico) create a layered enforcement stack. Theater ratio (0.45) is moderate: security theater (terrorism, crime) and welfare theater (fiscal burden) are real but diminishing as justifications; the enforcement stack increasingly serves only exclusion. Accessibility collapse (0.91) is extreme: for most of humanity, legal migration pathways are effectively nonexistent. Resistance (0.73) is high: migrants resist through irregular movement, asylum claims, legal challenges; advocacy networks resist through litigation, sanctuary, direct action; some sending states resist through diplomatic pressure. The claimed_type is snare: the constraint extracts from the powerless, benefits the powerful, requires active enforcement, and suppresses alternatives. No genuine coordination function survives scrutiny from this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (border_enforcement_agencies), the constraint appears as necessary coordination: managing sovereignty, security, and public order. From the payer seats (excluded_migrants, asylum_seekers, displaced_domestic_workers), it appears as violent extraction: denying life chances for accidents of birth. From the beneficiary seats (citizenries_of_wealthy_states, capital_owners), it appears as protective coordination: preserving wages, welfare, and cultural cohesion. The engine computes per-seat χ from these structural positions. The freedom_primary reading denies the agenda_setter's coordination claim as legitimate, but the structural positions remain: the agenda_setter administers, the payers pay, the beneficiaries collect.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizenries_of_wealthy_states and capital_owners are structural beneficiaries: they collect the place premium (d ≈ 0.1-0.2). Border_enforcement_agencies are agenda_setters who also benefit institutionally (budget, mission, authority) (d ≈ 0.15). Excluded_migrants, asylum_seekers, and stateless_persons are full targets: trapped, identity-locked (cannot change birth nationality), bearing the full extraction (d ≈ 0.95-1.0). Displaced_domestic_workers are partial targets: they bear wage suppression and hierarchy effects but have citizenship exit options (d ≈ 0.6-0.7). Migration_advocates are analytical observers (d = 0.5 by definition). The directionality derivation from beneficiary/victim declarations + exit options produces these d values; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The border regime's founding problem (Westphalian interstate conflict management) is dead — interstate war among wealthy states has been eliminated by nuclear deterrence and economic integration, not by borders. The regime persists as a zombie arrangement extracting from the globally poor. The mandatrophy is unresolved: the constraint's mandate (manage conflict) has evaporated, but its extraction machinery has hypertrophied. This is not a scaffold (no sunset clause) and not a piton (active enforcement, concentrated beneficiaries). It is a snare whose coordination cover story has collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_freedom_primary,
    'This constraint is one reading (freedom_primary) of the contested kernel border_normative_status. Sibling readings: sovereignty_primary (states have foundational authority to exclude), qualified_sovereignty (states retain control but must exercise proportionately). What structural elements do the readings genuinely disagree on versus merely emphasis?',
    'Map each reading''s beneficiary/victim structure, enforcement justification, and disappearance verdict. The freedom_primary reading declares excluded_migrants as rights-holders whose exclusion is violation; sovereignty_primary declares them as legitimate non-members; qualified_sovereignty declares them as subjects of proportionate control.',
    'If readings share the same ε referent (the standing border regime) but author different beneficiary/victim structures, they are distinct constraints linked by network.affects_constraints. If they author different ε referents, they are different topics entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_freedom_primary, conceptual, 'Commitment kernel decomposition: border_normative_status has three declared readings with different structural grammars').

omega_variable(
    coordination_function_absence,
    'From the freedom_primary reading, does the border regime possess ANY genuine coordination function, or is the coordination story (security, order, welfare state sustainability) entirely cover for extraction?',
    'Test whether open borders arrangements (e.g., Schengen internal borders, historical open borders eras) produce coordination collapse or functional alternatives. If coordination persists without exclusion, the coordination story is falsified for this reading.',
    'If no coordination function exists, claimed_type = snare is structurally correct. If a minimal coordination function exists (e.g., infectious disease control), claimed_type = tangled_rope with very high extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_absence, empirical, 'Whether the border regime''s coordination claims are structurally real or rhetorical cover').

omega_variable(
    suppression_mechanism_borders,
    'Is the suppression of movement structural (walls, laws, detention, deportation) or partially internalized (would-be migrants self-exclude due to perceived illegitimacy, fear, or identity fusion with ''citizen'' vs ''foreigner'' categories)?',
    'Post-exit suppression trajectory: if suppression persists after physical barriers are removed (e.g., internalized belief that ''I have no right to move''), reclassify as partially internalized. Compare migration flows when legal pathways open vs. remain closed.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This affects χ computation for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_borders, empirical, 'Structural vs. internalized suppression in border enforcement').

omega_variable(
    displaced_domestic_worker_victimhood,
    'In what structural sense are ''displaced domestic workers'' victims of the border regime from the freedom_primary reading? Is this wage competition, moral injury from complicity, or lost solidarity?',
    'Trace the causal chain: border regime -> restricted labor supply -> wage effects for domestic workers. But also: border regime -> racialized labor hierarchy -> degraded conditions for all workers. Distinguish extraction via borders from extraction via capital.',
    'If domestic workers are co-victims with migrants, cross-class coalition becomes structurally possible. If they are beneficiaries (wage protection), the victim set is narrower and coalition harder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_domestic_worker_victimhood, conceptual, 'Whether domestic workers are victims or beneficiaries of border restrictions in this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(bord_tr_t1995, border_normative_status__freedom_primary, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(bord_tr_t2000, border_normative_status__freedom_primary, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__freedom_primary, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__freedom_primary, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__freedom_primary, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(bord_tr_t2020, border_normative_status__freedom_primary, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(bord_be_t1995, border_normative_status__freedom_primary, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(bord_be_t2000, border_normative_status__freedom_primary, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__freedom_primary, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__freedom_primary, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__freedom_primary, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(bord_be_t2020, border_normative_status__freedom_primary, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(bord_su_t1995, border_normative_status__freedom_primary, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement(bord_su_t2000, border_normative_status__freedom_primary, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__freedom_primary, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__freedom_primary, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__freedom_primary, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(bord_su_t2020, border_normative_status__freedom_primary, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.1).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, global_migration_governance).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, asylum_system).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, citizenship_regimes).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, labor_market_segmentation).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, global_inequality_structure).

% DUAL FORMULATION NOTE:
% This constraint (freedom_primary) and sovereignty_primary are dual formulations of the border_normative_status kernel. They share the same referent (the global border regime) but author opposite beneficiary/victim structures and opposite claimed_types (snare vs. rope/mountain). The qualified_sovereignty reading occupies an intermediate structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
