% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause — Partial/Discretionary Withdrawal Reading
 *   domain: international law/diplomatic history/treaty interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242 (adopted 22 November 1967, after the June
 *   War) calls on Israel to withdraw 'from territories occupied in the recent
 *   conflict.' The English text carries an indefinite article; the French
 *   authoritative text reads 'des territoires occupes' (from THE occupied
 *   territories). This file instantiates the PARTIAL/DISCRETIONARY reading of
 *   that clause as a standing constraint: withdrawal scope is not fixed by
 *   the text but determined through negotiation toward 'secure and recognized
 *   boundaries,' permitting conditional, phased withdrawal and
 *   temporary-to-open-ended retention of strategic territory. Under this
 *   reading the clause operates as a Ledger converting textual indefiniteness
 *   into negotiating leverage. Epsilon's referent is the standing arrangement
 *   under contest - the discretionary-retention regime operating since 1967 -
 *   assessed by THIS reading's own lights, which regard the discretion as
 *   intended and lawful; the reading still scores the arrangement's actual
 *   extraction on claimants as substantial-but-moderate (0.66), discounted
 *   for its conditional, phased character (Sinai was returned under this
 *   framework; the West Bank, Gaza, and Golan retention compounded).
 *   CONSTRAINT FAMILY: the colloquial label 'the 242 withdrawal clause'
 *   decomposes into three structurally distinct stories per
 *   epsilon-invariance - this file (scope is discretionary; moderate
 *   epsilon), unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   (withdrawal is mandatory from all territories per the Charter Article
 *   2(4) territorial-integrity default and the French definite article;
 *   higher epsilon, unconditional extraction), and
 *   unsc_242_withdrawal_clause__interpretive_authority_structure (the contest
 *   over WHO resolves the ambiguity: ICJ judicial interpretation versus
 *   drafting-state authorial intent versus occupying-state customary
 *   practice). Each carries its own epsilon, beneficiaries, and victims; they
 *   are linked, not merged.
 *
 * KEY AGENTS:
 *   - occupying_power: agenda-setting beneficiary (institutional/mobile) - administers the retention regime on the ground, decides the pace and scope of any withdrawal, collects retained territory and negotiating discretion
 *   - mediating_superpowers: secondary beneficiaries and co-agenda-setters (institutional/arbitrage) - control the phasing and brokerage of withdrawal packages, collecting diplomatic leverage while absorbing enforcement costs
 *   - arab_claimant_states: primary targets (institutional/constrained) - bear open-ended territorial loss with no enforceable withdrawal line; differentiated internally by capacity to strike separate bargains
 *   - occupied_territory_residents: primary targets, doubly positioned as excluded (powerless/trapped) - bear direct daily costs of the retention regime while having no seat at the tables where withdrawal scope is set
 *   - international_legal_community: analytical observer (institutional/analytical) - tracks the widening gap between the discretionary frame and Charter-based acquisitive-illegality doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.66).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Withdrawal Clause — Partial/Discretionary Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international law/diplomatic history/treaty interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '2c0b3c67-37e9-412a-a35b-eda6941a5685').
narrative_ontology:cs_kernel_codification('2c0b3c67-37e9-412a-a35b-eda6941a5685', fixed_text).
narrative_ontology:cs_authority_grounding('2c0b3c67-37e9-412a-a35b-eda6941a5685', lineage).
narrative_ontology:cs_interpretation_layer_present('2c0b3c67-37e9-412a-a35b-eda6941a5685').
narrative_ontology:cs_reading_relation('2c0b3c67-37e9-412a-a35b-eda6941a5685', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('2c0b3c67-37e9-412a-a35b-eda6941a5685', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('2c0b3c67-37e9-412a-a35b-eda6941a5685', foundational, indefinite_article_grants_discretionary_scope).
narrative_ontology:cs_axiom_status(indefinite_article_grants_discretionary_scope, holdable).
narrative_ontology:cs_axiom_grounding('2c0b3c67-37e9-412a-a35b-eda6941a5685', indefinite_article_grants_discretionary_scope, conventional).
narrative_ontology:cs_axiom('2c0b3c67-37e9-412a-a35b-eda6941a5685', foundational, secure_boundaries_permit_boundary_adjustment).
narrative_ontology:cs_axiom_status(secure_boundaries_permit_boundary_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('2c0b3c67-37e9-412a-a35b-eda6941a5685', secure_boundaries_permit_boundary_adjustment, instrumental).
narrative_ontology:cs_reference_frame('2c0b3c67-37e9-412a-a35b-eda6941a5685', negotiated_discretion_baseline).
narrative_ontology:cs_drift_state('2c0b3c67-37e9-412a-a35b-eda6941a5685', contemporary_normalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c0b3c67-37e9-412a-a35b-eda6941a5685', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_superpowers).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, arab_claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_territory_residents).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafter_intent_controls_treaty_meaning).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, secure_recognized_boundaries_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the territories captured in June 1967 and administers them directly: decides unilaterally which areas to retain, which to trade, and on what timetable, subject only to great-power pressure it can usually deflect. Collects retained strategic depth, water resources, and settlement infrastructure. Its generational horizon reflects a multi-decade investment in the retention enterprise; exit would mean either full withdrawal on terms it does not set or formal annexation absorbing the enforcement burden openly - both available, neither preferred.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary).

% Control the phasing and brokerage of every withdrawal package: shuttle diplomacy, aid flows calibrated to movement or stillness, and the veto that shields the discretionary frame from Council enforcement. Collect diplomatic leverage, regional influence, and the mediator role itself. Absorb real costs - credibility expenditure on failed rounds, domestic political exposure, occasional rupture with regional partners. Can reposition or reduce engagement (arbitrage) but find continued brokerage more valuable than exit.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_superpowers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_superpowers, agenda_setter).

% Demand recovery of territory lost in 1967 under an arrangement whose enforcement line dissolves at the boundary of their claims. Bear continuing military preparedness costs, lost resources, and the political cost of any compromise with the retained-territory reality. Exit is constrained but real: a separate negotiated peace (as Egypt executed in 1979) recovers specific territory at the price of splitting the collective maximalist line and accepting the discretionary frame's legitimacy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, arab_claimant_states, payer,
    institutional, generational, constrained, regional).

% Live under the day-to-day administration of the retained territories: settlement expansion onto land they hold or claim, movement restrictions, dual legal regimes, and expropriation. Have no sovereign seat at any table where withdrawal scope is negotiated - excluded from Geneva in 1973 and Camp David in 1978, admitted only to subordinate roles thereafter. Cannot leave the territory in any way that escapes the constraint; their biographical horizons are set by decisions taken in rooms they do not enter.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_territory_residents, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_territory_residents, excluded).

% Tracks the widening distance between the discretionary frame and the Charter-based doctrine that territory cannot be acquired by force, issuing advisory analyses (including court opinions on the wall and on the occupation's legality) that the negotiating channel routes around rather than answers. Holds no enforcement lever over the arrangement; its product is doctrinal record and legitimacy accounting, consumed by future tribunals and treaty moments rather than by the current process.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common textual anchor around which irreconcilable parties can transact: withdrawal scope, sequencing, and destination boundaries become staged exchange items ('land for peace') rather than a single take-it-or-leave-it legal line. This made partial deals possible that a rigid mandatory-line mandate would have vetoed into stalemate, and gave every subsequent treaty a shared interpretive foundation.
% TRANSFER_FUNCTION: Moves decision rights over withdrawal scope from fixed legal determination to the occupying power and the mediating powers who administer phasing; moves territorial control of strategic areas from claimant states to the occupying power pending negotiated exchange; moves diplomatic leverage and brokerage rents to the mediators; extracts daily administrative and territorial costs from the occupied residents.
% ABSENT_VOICES: The people governed by the arrangement had no seat where its meaning was fixed or applied: occupied-territory representation was excluded from the Geneva conference (1973) and Camp David (1978), entering the process only later in subordinate channels. Legal authorities favoring the definite-article construction were routed around the negotiating channel into General Assembly resolutions and advisory opinions with no enforcement path. Both groups reside outside the bilateral-mediation circuit that the discretionary frame constitutes.
% DISAPPEARANCE_RATIONALE: If the discretionary framework vanished overnight - replaced by a legally fixed withdrawal line or simply ceasing to operate - the territorial settlement economy would reorganize immediately: the Egypt-Israel and Jordan-Israel treaties and the entire Oslo-era architecture are anchored interpretively to this clause and would lose their foundation; open-ended retention would convert into either enforced withdrawal or formalized annexation, each with radically different coalition consequences; and the normalization agreements that currently cite the resolution while bypassing the withdrawal question would face renegotiation or abandonment.
% FOUNDING_PROBLEM: After the June 1967 war the Council confronted a triple bind: a ceasefire with no political horizon, an occupying power refusing return to 1949 armistice lines it deemed indefensible, and claimant states demanding unconditional full withdrawal. The drafting problem was to produce text all permanent members and both sides could accept WITHOUT resolving how much withdrawal sufficed - yielding an English indefinite article over a French definite one, and a secure-boundaries proviso whose grammatical object remains disputed to this day.
% FOUNDING_PROBLEM_CORROBORATION: Attestation exists outside the benefiting parties: the UN verbatim records and the UK Foreign Office and US State Department drafting files - documented by historians including Quandt and Shlaim - independently confirm both the deliberate indefiniteness of the English text and the stabilization motive behind the resolution. Victim-side corroboration is unusually strong: Sadat's 1971 initiative, the 1981 Fahd Plan, and the 2002 Arab Peace Initiative all accept the resolution as baseline while disputing its reading, demonstrating that the original problem was real and remains unresolved even in the judgment of the parties bearing its costs. No material element of the genealogy rests solely on occupying-power or mediator assertion.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on independent structural grounds: the arrangement possesses BOTH a genuine coordination function (a common textual anchor enabling land-for-peace exchange that a rigid mandate would have blocked - the Egypt-Israel treaty returning Sinai is the paradigm transaction) AND asymmetric extraction (open-ended retention of West Bank, East Jerusalem, and Golan with no enforceable endpoint for claimants), held together by active enforcement (veto protection, patronage conditionality, military facts on the ground, and process management). Metrics are authored descriptively, not tuned to the claim. Extractiveness (0.66 at interval end) is moderate-high rather than severe because the constraint is conditional and phased: the Sinai return (1982) demonstrates real transfer capacity, but Golan annexation (1981), settlement consolidation, and post-normalization de facto annexation trends push the cumulative burden upward. Suppression (0.62) is predominantly STRUCTURAL - veto-shielded enforcement, superpower patronage, and military reality foreclose claimant alternatives - with a smaller internalized component (victim-side parties increasingly invoke 242 itself as their baseline; see the claimant_consent omega; rough proportion 80% structural / 20% internalized). Theater_ratio (0.55) rose steadily as the process layer (road maps, Annapolis, Quartet ceremonies) decoupled from withdrawal outcomes; late-period normalization agreements cite the resolution rhetorically while bypassing the withdrawal question entirely. Accessibility_collapse (0.42) stays below mountain range because alternatives persist at cost: separate treaties, unilateral disengagement, and the ICJ advisory route all remain available. Resistance (0.70) is sustained across modes - initial rejection front, the 1973 war, General Assembly majorities, intifadas, boycott movements, and advisory-opinion litigation. CYCLICAL DYNAMICS: the mild oscillation in the extractiveness series (dip at t=24, corresponding to the Madrid-process opening) follows a crisis-negotiation-relaxation-accumulation cycle; the oscillation itself functions partly as an extraction mechanism - each negotiation round acts as a pressure valve that releases resistance and then legitimates renewed retention - and the base_properties scalars reflect the late-phase (post-normalization) state. COALITION CHECK: the two victim classes differ sharply in coalition capacity. Claimant states assembled transient coalition leverage (the 1973 oil weapon temporarily reversed extraction terms and produced the Sinai II disengagement), but occupied residents lack statehood instruments and unified representation; coalition power is therefore asymmetric across victim classes and cannot be aggregated.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from identical structural data. From the agenda-setting beneficiary seat (occupying_power), the arrangement presents as a rope it built and administers: a framework that produced two peace treaties and converted existential insecurity into negotiable packages. From the constrained payer seat (arab_claimant_states), the same structure operates as enforced extraction: a resolution whose enforcement line dissolves exactly where their claims begin. The trapped payer seat (occupied_territory_residents) experiences the constraint at maximum intensity with zero procedural voice. The mediator seat experiences managed-process satisfaction punctuated by credibility costs. The analytical observer seat sees the legality drift accumulating beneath the diplomatic surface. The engine computes this per-seat divergence from the declared power/exit/role data; the authored claim does not adjudicate among the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The occupying_power is declared beneficiary and sits at the deep-beneficiary end of d (roughly 0.05-0.1): the constraint subsidizes it with retained territory, discretion over timing, and veto-protected position; its mobile exit options dampen effective extraction further. mediating_superpowers are declared beneficiaries with arbitrage-grade exits (low d, roughly 0.2): they collect brokerage rents but absorb real enforcement costs (reputational expenditure, veto deployment, process-management overhead), placing them slightly off the pure-beneficiary pole. arab_claimant_states are declared victims (high d, roughly 0.85): they pay in territory, security, and foregone enforcement, with constrained exit (separate peace is available but costly to the collective line - Egypt paid that price). occupied_territory_residents are declared victims with trapped exit (highest d, roughly 0.95): no mobility, no procedural seat, direct daily exposure. NO DIRECTIONALITY OVERRIDES are authored, deliberately: the override mechanism keys on power_atom, and three of the four directional seats share the 'institutional' atom - any override would distort the claimant states and the legal community along with the seats it intends to correct. The structural declarations plus exit-option differentiation carry the needed separation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents symmetric mislabeling. Reading the clause as a pure rope (the occupying power's and mediators' framing) renders the victim classes invisible and launders open-ended retention as neutral facilitation. Reading it as a pure snare (some maximalist advocacy) erases the demonstrated coordination yield - Sinai was actually returned under this framework, a transaction a pure snare cannot produce. The mandatrophy interview locates the residual: the founding problem (stabilizing a ceasefire without resolving how much withdrawal suffices) was genuinely solved enough to enable treaties but left the core distributional question open; status is therefore contested rather than dead, and no mandatrophy_resolved declaration is authored. The mismatch consumer should note the risk vector: if the founding problem were scored dead while disappearance_verdict stays world_rearranges, the zombie flag would fire - and the late-period theater rise is the leading indicator of exactly that transition. Identity-lock dynamics are concentrated in the agenda-setting seat: the occupying power's generational time horizon reflects institutional identity fusion between the state and the settlement enterprise it administers; if that identity frame broke (settlement constituency dissolved as a governing coalition), the discretion would convert rapidly from leverage into liability, since the constraint's beneficiary value depends on wanting the retained territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafter_intent_archival_record,
    'Was the English indefinite article in ''withdrawal from territories occupied'' a deliberate grant of negotiating discretion, or an artifact of reconciling the French definite-article text with US and UK preferences?',
    'Systematic comparison of UN verbatim records, UK Foreign Office and US State Department drafting files, and the five official language versions; testimony of surviving drafters recorded contemporaneously.',
    'If deliberateness is established, the discretion is a constructed policy feature benefiting identifiable agents (supports the extractive component of this story); if artifact, the discretion is unintended and the arrangement''s persistence rests on post-hoc exploitation of a drafting accident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafter_intent_archival_record, empirical, 'Whether textual indefiniteness was engineered or incidental.').

omega_variable(
    intent_governs_meaning_question,
    'Does drafter intent govern the meaning of the clause at all, or is the adopted text''s public meaning controlling regardless of what drafters privately held?',
    'Conceptual analysis under Vienna Convention Articles 31-32, which demote travaux preparatoires to supplementary status; examination of how the interpretive community weights intent versus text.',
    'If public-textual meaning controls, the discretionary construction loses its authorial warrant and stands or falls on subsequent practice alone; if intent controls, the indefinite article is a legitimate delegation of scope-setting to negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_governs_meaning_question, conceptual, 'Interpretive theory dependency of the discretionary warrant.').

omega_variable(
    secure_boundaries_clause_function,
    'Does ''secure and recognized boundaries'' license retention of territory beyond the 1949 armistice lines, or does it merely qualify the boundaries TO WHICH withdrawal occurs after full withdrawal?',
    'Clause-order and grammatical analysis alongside the negotiating history of the secure-boundaries provision; comparison of how each subsequent treaty (Egypt-Israel, Jordan-Israel) operationalized the term.',
    'If retention-permitting, the extraction measured here is licensed within the constraint itself; if withdrawal-qualifying only, the retention component is extra-contractual appropriation riding on the clause''s ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_clause_function, conceptual, 'Whether the secure-boundaries clause is a retention license or a destination qualifier.').

omega_variable(
    claimant_consent_structural_or_internalized,
    'Is claimant-state acquiescence in the discretionary frame coerced by the veto-and-patronage structure, or partially internalized (claimants now invoke Resolution 242 as their own negotiating baseline)?',
    'Counterfactual analysis of claimant behavior under enforcement scenarios: Sadat''s 1971 initiative, the 1981 Fahd Plan, and the 2002 Arab Peace Initiative all accept the 242 frame while demanding its maximalist reading; track whether acceptance survives hypothetical removal of great-power protection.',
    'If internalized, the constraint is more durable than its coercive scaffolding suggests and suppression measures understate persistence; if purely coerced, removal of patron protection collapses the discretionary frame rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claimant_consent_structural_or_internalized, empirical, 'Structural versus internalized basis of claimant participation.').

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the partial_withdrawal_reading of kernel unsc_242_withdrawal_clause; how would the sibling readings restructure the constraint, and where exactly is the disagreement located?',
    'Cross-file comparison within the constraint family: the maximal_withdrawal_reading relocates the victim/beneficiary sets (occupying power becomes pure target; claimants become intended beneficiaries) and raises epsilon by removing the conditional-phased discount; the interpretive_authority_structure reading relocates the contest to the meta-level of WHO resolves the ambiguity (ICJ judicial interpretation versus drafting-state authorial intent versus occupying-state customary-practice claims).',
    'The disagreement is located in two specific structural elements: the semantic force of the English indefinite article, and the normative function assigned to the secure-boundaries clause. Classification of THIS file as tangled_rope does not adjudicate the kernel; family-linked stories must be compared per-seat, never averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame routing: kernel membership, sibling deltas, disagreement locus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(unsc_tr_t8, observed).
narrative_ontology:measurement(unsc_tr_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(unsc_tr_t16, observed).
narrative_ontology:measurement(unsc_tr_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(unsc_tr_t24, observed).
narrative_ontology:measurement(unsc_tr_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement_basis(unsc_tr_t32, observed).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(unsc_tr_t40, observed).
narrative_ontology:measurement(unsc_tr_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 48, 0.52).
narrative_ontology:measurement_basis(unsc_tr_t48, observed).
narrative_ontology:measurement(unsc_tr_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 56, 0.55).
narrative_ontology:measurement_basis(unsc_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(unsc_be_t8, observed).
narrative_ontology:measurement(unsc_be_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(unsc_be_t16, observed).
narrative_ontology:measurement(unsc_be_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(unsc_be_t24, observed).
narrative_ontology:measurement(unsc_be_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(unsc_be_t32, observed).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(unsc_be_t40, observed).
narrative_ontology:measurement(unsc_be_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 48, 0.64).
narrative_ontology:measurement_basis(unsc_be_t48, observed).
narrative_ontology:measurement(unsc_be_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 56, 0.66).
narrative_ontology:measurement_basis(unsc_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(unsc_su_t8, observed).
narrative_ontology:measurement(unsc_su_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(unsc_su_t16, observed).
narrative_ontology:measurement(unsc_su_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(unsc_su_t24, observed).
narrative_ontology:measurement(unsc_su_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement_basis(unsc_su_t32, observed).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(unsc_su_t40, observed).
narrative_ontology:measurement(unsc_su_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(unsc_su_t48, observed).
narrative_ontology:measurement(unsc_su_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement_basis(unsc_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, resource_allocation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per epsilon-invariance: the natural-language label 'the Resolution 242 withdrawal clause' conflates three structurally distinct constraints. (1) THIS FILE - the partial/discretionary scope claim: withdrawal extent is negotiable, secure boundaries permit retention; epsilon 0.66, conditional and phased, tangled_rope. (2) maximal_withdrawal_reading - the mandatory-full-scope claim grounded in Charter Article 2(4) and the French definite article; epsilon materially higher (unconditional extraction from the claimant side's perspective), different victim/beneficiary orientation. (3) interpretive_authority_structure - the meta-level claim about who owns the ambiguity's resolution; its epsilon attaches to legitimacy production rather than territorial flow. The upstream/downstream structure runs from the text's adoption (shared upstream fact) through the scope readings (this file and the maximal sibling) into the authority contest; each file links its siblings via affects_constraints and none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
