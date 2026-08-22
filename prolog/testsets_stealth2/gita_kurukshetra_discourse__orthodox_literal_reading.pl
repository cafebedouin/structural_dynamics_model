% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Gita: Birth-Fixed Caste Duty and Righteous-War Mandate
 *   domain: religious/hermeneutic/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita functions in this corpus as a contested kernel: one
 *   text, several incompatible readings, each instantiating a different
 *   constraint. This story authors the orthodox literal reading — the
 *   arrangement in which the text's authority is administered as a command to
 *   birth-fixed social duty (svadharma) and to battle fought as duty in a
 *   righteous cause. Under this reading the varna hierarchy enters the
 *   beneficiary set as a divinely ordained order: the priestly class holds
 *   interpretive monopoly, and the warrior class receives legitimation for
 *   violence its members would otherwise have to justify or refuse. The
 *   victim set comprises the merchant, serving, and outcaste orders locked
 *   into inherited station, and those killed when war is consecrated as duty.
 *   Two sibling stories — the Gandhian allegorical reading and the
 *   universalist devotional reading — instantiate the same kernel with
 *   different epsilon, different beneficiaries, and different victims; per
 *   the epsilon-invariance principle they are separate files linked through
 *   the network section, never averaged into this one. The claim/metric split
 *   is deliberate: claimed_type records the structure judged true of this
 *   arrangement; the metrics record its observed operation; divergence
 *   between them and any computed verdict is data, not error. KEY AGENTS (by
 *   structural relationship): - brahmin_interpretive_class: agenda-setter and
 *   principal collector ([institutional]/[arbitrage]) — administers the
 *   text's meaning, collects the interpretive rents -
 *   kshatriya_warrior_class: dual-positioned beneficiary-payer
 *   ([powerful]/[identity_locked]) — receives legitimation for force, bears
 *   compelled fighting - vaishya_merchant_castes: payer
 *   ([moderate]/[constrained]) — funds the upper orders under fixed
 *   occupational duty - shudra_service_castes: payer ([powerless]/[trapped])
 *   — inherited service obligation, no scripted exit -
 *   dalit_outcaste_communities: payer ([powerless]/[trapped]) — outside the
 *   scheme, bearing its polluting labors - dharmic_war_dead: payer
 *   ([powerless]/[trapped]) — the killed, removed from moral accounting by
 *   the consecration of the war - heterodox_rejection_traditions: excluded
 *   ([organized]/[mobile]) — deny the frame entirely from parallel
 *   institutions - academic_textual_scholars: analytical observer
 *   ([institutional]/[analytical]) — sees the full structure, takes no seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.68).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Gita: Birth-Fixed Caste Duty and Righteous-War Mandate").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/hermeneutic/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '8be3f115-1529-4ab4-a586-364f1e053f12').
narrative_ontology:cs_kernel_codification('8be3f115-1529-4ab4-a586-364f1e053f12', fixed_text).
narrative_ontology:cs_authority_grounding('8be3f115-1529-4ab4-a586-364f1e053f12', lineage).
narrative_ontology:cs_interpretation_layer_present('8be3f115-1529-4ab4-a586-364f1e053f12').
narrative_ontology:cs_reading_relation('8be3f115-1529-4ab4-a586-364f1e053f12', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_reading_relation('8be3f115-1529-4ab4-a586-364f1e053f12', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('8be3f115-1529-4ab4-a586-364f1e053f12', foundational, svadharma_divinely_ordained_by_birth).
narrative_ontology:cs_axiom_status(svadharma_divinely_ordained_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('8be3f115-1529-4ab4-a586-364f1e053f12', svadharma_divinely_ordained_by_birth, theological).
narrative_ontology:cs_axiom('8be3f115-1529-4ab4-a586-364f1e053f12', foundational, duty_fought_violence_karmically_clean).
narrative_ontology:cs_axiom_status(duty_fought_violence_karmically_clean, holdable).
narrative_ontology:cs_axiom_grounding('8be3f115-1529-4ab4-a586-364f1e053f12', duty_fought_violence_karmically_clean, deontological).
narrative_ontology:cs_reference_frame('8be3f115-1529-4ab4-a586-364f1e053f12', divine_ordination_of_birth_duty).
narrative_ontology:cs_drift_state('8be3f115-1529-4ab4-a586-364f1e053f12', contemporary_pluralist_reception, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8be3f115-1529-4ab4-a586-364f1e053f12', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_merchant_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_service_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dharmic_war_dead).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_social_order_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, nishkama_karma_action_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose the commentaries, decide which verses carry normative force, and administer the ritual economy through which the text's authority reaches households and courts. Their standing rests on being the sole authorized readers of the text; they collect honor, fees, and first claim on social deference, and they can absorb rival interpretations by ranking them as subordinate readings rather than losing control of the text.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, beneficiary).

% Hold ruling and military office by birth and receive scriptural legitimation for the use of force: fighting in a righteous cause is framed as their appointed duty rather than a crime requiring excuse. The same mandate binds them personally — refusal to fight when duty calls is condemned as dereliction, and the text's paradigm scene is a warrior trying to lay down his bow and being argued back onto the field. Their identity as rulers and soldiers is constituted by the duty they are assigned; stepping out of it means stepping out of who they are.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, payer).

% Trade, farm, and lend under a duty assignment that fixes them in productive occupations and obliges them to support the upper orders through taxes, gifts, and ritual payments. Movement upward is closed by birth; their recourse is prosperity within their station, and their obligations fund the priestly and warrior establishments above them.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_merchant_castes, payer,
    moderate, biographical, constrained, regional).

% Are assigned service to the three higher orders as a birth-fixed, transmissible occupation, with access to Vedic learning formally barred and religious standing mediated entirely by their superiors. Exit has no scripted form: the duty is inherited, taught as divinely arranged, and upheld by household, village, and court alike.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_service_castes, payer,
    powerless, generational, trapped, regional).

% Sit outside or beneath the fourfold scheme altogether, performing the polluting labors the scheme requires while being denied the ritual standing the scheme grants everyone inside it. The text's near-silence about them is administered as exclusion: purity boundaries, residential segregation, and denial of temple entry keep them fixed at the bottom of an order presented as divinely graded.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, regional).

% Are the soldiers and noncombatants killed when a war is fought under the righteous-war mandate. The doctrine that consecrates the fighting also removes their deaths from moral accounting: dying in a dutiful war is framed as heaven-earning for the right-born and as karma for the rest, so no party inside the arrangement owes them redress, mourning beyond ritual, or an account of why the battle was necessary.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dharmic_war_dead, payer,
    powerless, immediate, trapped, regional).

% Buddhist, Jain, and materialist schools rejected Vedic authority, caste as religious fact, and the sacralization of killing outright, building parallel monastic institutions and arguing that liberation is open to anyone regardless of birth. They stand outside the conversation the reading adjudicates: the text answers them only obliquely, and the interpretive establishment classifies their objections as foreign rather than engaging them as rival readers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, heterodox_rejection_traditions, excluded,
    organized, civilizational, mobile, regional).

% Apply philological and historical methods to the text: dating its layers, tracing compositional seams, comparing manuscript variants, and situating the discourse in the argument between Vedic ritualism and the renouncer currents of its era. They take no side in the dispute over what the text commands; their analyses are available to every reading and to none.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, academic_textual_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the warrior's crisis of conscience by relocating the decision to fight from personal inclination to assigned duty; allocates social functions across a birth-graded division of labor (teaching and ritual, ruling and fighting, production, service); disciplines warfare with rules distinguishing a righteous battle — declared, between combatants, proportionate — from raiding and massacre; and gives every estate a theologically anchored account of why it occupies its place.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual fees, and social deference to the priestly class; moves legitimation and honored status to the warrior class; moves labor, produce, and tax from the producing and serving orders upward; and moves the moral cost of killing off the individual killer and onto the impersonal order of duty, so the warrior acts without owning the act's burden.
% ABSENT_VOICES: Those killed under the righteous-war mandate have no seat anywhere in the arrangement; the serving and outcaste orders appear in the text mainly as recipients of instruction, speaking only through voices above them; the heterodox schools that deny the text's authority altogether argue from outside its frame; and modern anti-caste readers contest the reading from a vantage its interpreters classify as disloyalty to the tradition.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading ceased overnight — if no one read the text as commanding birth-fixed duty and sanctified battle — the scriptural anchor for caste-as-divine-order would drop away, courts and households would lose the warrant they cite for inherited station, righteous-war language would lose its consecrating text, and the interpretive economy built on authorized reading would reorganize around whichever rival reading captured the vacuum. The social order would not vanish, but its oldest justification would have to be rebuilt from other materials.
% FOUNDING_PROBLEM: A warrior facing battle sees teachers, cousins, and grandfathers on the opposing line, lays down his bow, and argues that victory, kingdom, and life itself are worthless bought at that price; the text exists, in the form it was received, to answer him — to show why the fight must be joined anyway and on what terms killing in it leaves the killer unbound.
% FOUNDING_PROBLEM_CORROBORATION: The epic frame itself attests the crisis independently of any later interpreter: the despondency scene is embedded in the Mahabharata's war narrative, not added by the reading's beneficiaries. Philological scholarship corroborates that the discourse answers a real historical collision between warrior duty and the renouncer-and-nonviolence currents of its composition era. Outside the benefiting parties, however, no one attests that the founding problem still requires THIS reading as its solution: Gandhian and universalist readers attest the crisis is real but claim other readings answer it better, and anti-caste scholarship attests that the duty-framework this reading supplies now does more harm than the crisis it once addressed.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the arrangement transfers real goods on unequal terms — labor and deference upward, legitimation downward to one armed estate — while removing the killed from accounting entirely; it is not maximal because the arrangement also delivers goods its subjects plausibly want: crisis resolution, war discipline, a livable account of social place. Suppression is 0.72 and is authored as a raw structural quantity, unscaled by power or scope: persistence depends on active machinery — authorized commentary, ritual gatekeeping, sanction on transgressors, court patronage — not on spontaneous assent. Theater ratio 0.30: most of the apparatus performs real work (counseling, adjudication, war regulation), with a growing share of activity defending the interpretive monopoly itself. Accessibility collapse 0.60: inside the reading's frame alternatives collapse hard — renunciation is argued down, heterodox paths are classified as foreign — but the alternatives survive outside the frame, which is why this is not a natural-law profile. Resistance 0.55: fourteen centuries of heterodox competition, devotional egalitarianism, and eventually anti-caste revolt. Coordination type is authored as identity_coordination: the arrangement's primary function is boundary maintenance — defining who one is by birth and what one therefore owes — and its failure mode is the dissolution of membership claims, which is exactly what the sibling readings threaten. The measurement series share one seven-point grid (t=0 to t=60, roughly one unit equal to thirty years, spanning early classical consolidation to the eve of colonial-era disruption) so no metric is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the priestly seat the arrangement is the thing it administers: authority, continuity, and the text itself look like one benevolent object, and effective extraction reads near zero or negative. From the serving and outcaste seats the same structure is a birth sentence: the identical verses that assign the priest his honor assign them their service, with no exit the text recognizes. The warrior seat is the hinge — the arrangement pays it in legitimation and charges it in compulsion, and the text's own dramatic center is a member of that seat attempting resignation and being argued back onto the field. Identity-lock operates here as professional-relational fusion: the warrior's self-concept is constituted by the duty, so exit (renunciation) is not merely costly but self-dissolving; were that identity frame to break, the warrior seat's classification would converge toward the payer seats'. The killed have no seat at all; their absence is the arrangement's quietest success. Coalition potential among the lower seats existed historically — devotional and anti-caste movements were exactly such coalitions — which is why resistance is authored mid-range rather than low. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The priestly class (agenda-setter, arbitrage exit) derives near the fully-subsidized end: it writes the meanings, collects the fees, and can absorb rivals by reclassification. The warrior class is declared beneficiary but carries an override to 0.42 because derivation from the beneficiary tag alone would miss the compulsion side of the same mandate — legitimation and obligation are one structure, and the obligated half is what the text spends its persuasive force maintaining; the override weights the seat slightly toward target because compelled killing, battlefield death, and condemned desertion outweigh legitimation for many holders of the station. The serving, outcaste, and killed seats derive near the full-target end from their victim declarations plus trapped exits. Merchant castes sit high but not maximal: constrained rather than trapped, with real if narrow room to prosper inside station. Heterodox traditions sit outside the derivation entirely — they reject the frame rather than occupying a position in it — which is why they are authored as excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what keeps both mislabelings visible. Reading the arrangement as pure coordination (the traditionalist self-description) would erase the victims the structure requires to hold — the lower orders' fixity is not a side effect but a load-bearing wall. Reading it as pure extraction (some anti-caste polemic) would erase the genuine coordination the arrangement delivers — the crisis-resolution function is real, the war-discipline rules are real, and adherents who collect nothing hold the arrangement voluntarily. The founding-problem interview locates the residual question: the founding crisis (a warrior paralyzed before kin) is textually attested and perennial in form, but whether it still requires THIS reading as its answer is disputed by the sibling readings — hence founding_problem_status contested, and the mismatch consumer should watch this story for capture signals if the status flips to dead while the arrangement persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the orthodox_literal_reading of the gita_kurukshetra_discourse kernel; the disagreement with the gandhian_allegorical_reading and universalist_devotional_reading siblings is located in which structural element carries the text''s authority — the command to birth-fixed duty and sanctioned battle, the inward allegory of struggle, or the caste-transcending call to surrender?',
    'No in-frame resolution exists: the readings are competing commitments over one text. Resolution arrives only through which reading successive communities adopt and transmit — cross-cohort comparison of the three sibling stories'' classifications.',
    'If the allegorical or devotional reading displaces the literal one as the transmitted default, the beneficiary set collapses (no divinely ordained hierarchy left to subsidize), the victim set shifts from castes and war-dead to whatever the successor reading disciplines, and this story''s classification lapses into historical record.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings change the victim and beneficiary sets entirely.').

omega_variable(
    compositional_layer_authorship,
    'Are the verses anchoring birth-fixed duty and sanctioned battle — the varna-creation statement and the better-one''s-own-duty exhortations — part of the discourse''s earliest layer, or accretions from a later redaction working in a more hierarchical milieu?',
    'Philological and text-critical analysis: stylistic stratification, manuscript variant comparison, and dependence mapping against the epic''s compositional history.',
    'If the duty-and-battle verses are a later layer, the reading''s foundational axiom rests on accretion rather than core, drift toward axiom-overriding accelerates, and the sibling readings'' claim to the text''s center strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compositional_layer_authorship, empirical, 'Whether the mandate verses are constitutive of the text or interpolated into it.').

omega_variable(
    righteous_war_constraining_power,
    'Does the righteous-war framework actually restrain violence — excluding raids on noncombatants, limiting escalation, requiring formal declaration — or does it function chiefly to consecrate whatever violence rulers already intend?',
    'Comparative analysis of battles waged under explicit dharmic-war framing versus contemporaneous wars without it: casualty patterns, treatment of noncombatants, post-war settlement terms.',
    'If the framework genuinely restrains, a measurable share of its operation is coordination cost rather than extraction and effective extractiveness falls; if it merely sanctifies, the coordination story is cover and the arrangement sits nearer pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(righteous_war_constraining_power, empirical, 'Whether dharmic-war rules bind conduct or bless intent.').

omega_variable(
    enforcement_conviction_composition,
    'How much of the reading''s persistence across the interval reflects settled conviction among its adherents, and how much reflects enforcement — ritual gatekeeping, social sanction against transgressors, court patronage conditioned on orthodoxy?',
    'Historical analysis of episodes where enforcement slackened (patronage shifts, sectarian competition, frontier regions thin in authorized interpreters): if adherence held where enforcement lapsed, conviction dominates; if it eroded, enforcement dominates.',
    'A high enforcement share pushes the arrangement toward enforced extraction; a high conviction share leaves it nearer voluntary coordination sustained by belief, with correspondingly lower effective coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_conviction_composition, empirical, 'Conviction versus enforcement as the persistence mechanism.').

omega_variable(
    mobility_pressure_undercount,
    'Do documented episodes of mobility — devotional movements raising low-born teachers, new scribal castes rising, mercenary recruitment bypassing birth — mean the fixity attributed to the lower orders is overstated?',
    'Social-historical measurement of mobility rates across the interval against the doctrinal bar the reading maintains.',
    'Material mobility without doctrinal recognition leaves the trap intact, since status rather than income is what the order rations; doctrinally recognized mobility would soften the victim structure and lower effective extraction for the affected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobility_pressure_undercount, empirical, 'Whether observed mobility softens the fixity the doctrine declares.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question 'what does the Gita teach' covers at least three structurally distinct arrangements. This story is the historically upstream member — the earliest consolidated reading, cited as authoritative by later traditionalists — and its dominance shaped the conditions under which the siblings emerged: the Gandhian reading is organized as a refutation of the martial-literal command, and the universalist devotional reading defines itself against caste-gated religious standing. Edges run from this story to both siblings. Epsilon differs sharply across the family: this member carries the caste-fixity and war-dead victim sets that the siblings dissolve (allegory) or redistribute (universal devotion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
