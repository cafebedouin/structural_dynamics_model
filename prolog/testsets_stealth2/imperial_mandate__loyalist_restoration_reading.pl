% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Unmediated Imperial Sovereignty Requirement (Loyalist Restoration Reading)
 *   domain: political philosophy / comparative constitutional systems / east asian history
 *
 * SUMMARY:
 *   This story instantiates the loyalist_restoration_reading of the
 *   imperial_mandate kernel: the claim that divine mandate requires the
 *   emperor's unmediated exercise of sovereignty, so that political
 *   legitimacy stands or falls with active imperial governance. Historically
 *   this reading mobilized the anti-bakufu coalition, destroyed the shogunate
 *   in 1868, abolished the domains, stripped the samurai estate, and anchored
 *   the Meiji state — and it condemned as usurpation every structure that
 *   stood between throne and administration. The epsilon referent is the
 *   standing arrangement under contest: the unmediated-imperial-sovereignty
 *   order as it actually operated from 1868 to 1945, assessed by the
 *   reading's own lights. Those lights cut both ways: the reading endorses
 *   the sacred core as non-negotiable, but its own standard — the emperor
 *   must actively rule — condemns the sanctified oligarchy that actually
 *   emerged, and the reading's own radical wing (the Showa Restoration
 *   ideologues and young officers) prosecuted that judgment with
 *   assassinations and revolt. Reading-indexed epsilon therefore lands high
 *   but not maximal: 0.85 at interval end, reflecting the levies the reading
 *   itself acknowledges as sacrificial costs and the mediation its own lights
 *   condemn, discounted by the sincerity of the devotional core. This file is
 *   one member of a two-story family: the sibling bakufu_delegation_reading
 *   authors the same kernel's delegated arrangement as coordination with a
 *   different victim set and materially lower epsilon; the decomposition
 *   follows the epsilon-invariance principle — one colloquial label, two
 *   structurally distinct constraints, linked in network.affects_constraints.
 *   Claim and metrics are independent authored facts: the claimed type is
 *   tangled_rope from structural analysis (a real coordination function and
 *   an asymmetric transfer running through the same enforced structure); the
 *   metrics describe the arrangement's actual operation, and any divergence
 *   between claim and computed classification is the datum, not an error to
 *   repair.
 *
 * KEY AGENTS:
 *   - meiji_oligarchy: agenda-setter and primary collector (institutional/arbitrage) — administers the mandate and certifies what counts as imperial will
 *   - imperial_household: nominal apex and declared beneficiary (powerful/identity_locked) — legitimacy source captive to its own sanctity
 *   - state_shinto_establishment: ideological beneficiary (institutional/identity_locked) — supplies the doctrine's ritual and pedagogical infrastructure
 *   - samurai_class: principal early bearer of costs (organized/trapped) — stipends commuted then cancelled, class dissolved, revolt broken by a conscript army
 *   - agrarian_households: fiscal bearers (powerless/trapped) — cash land tax financed industrialization and war
 *   - conscripted_commoners: bodily bearers (powerless/trapped) — universal service from 1873 onward
 *   - political_dissenters: thought bearers (moderate/constrained) — socialists, anarchists, liberals surveilled, imprisoned, executed
 *   - popular_sovereignty_advocates: excluded seat — their premise was unspeakable under the doctrine
 *   - constitutional_scholarship: analytical observer — the Minobe organ-theory school that described the mediation the orthodoxy denied, purged in 1935
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.85).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.9).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Unmediated Imperial Sovereignty Requirement (Loyalist Restoration Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political philosophy / comparative constitutional systems / east asian history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'fa041789-c619-4dca-a078-4bc80d3b0efe').
narrative_ontology:cs_kernel_codification('fa041789-c619-4dca-a078-4bc80d3b0efe', fixed_text).
narrative_ontology:cs_authority_grounding('fa041789-c619-4dca-a078-4bc80d3b0efe', lineage).
narrative_ontology:cs_interpretation_layer_present('fa041789-c619-4dca-a078-4bc80d3b0efe').
narrative_ontology:cs_reading_relation('fa041789-c619-4dca-a078-4bc80d3b0efe', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('fa041789-c619-4dca-a078-4bc80d3b0efe', foundational, legitimacy_requires_active_imperial_governance).
narrative_ontology:cs_axiom_status(legitimacy_requires_active_imperial_governance, holdable).
narrative_ontology:cs_axiom_grounding('fa041789-c619-4dca-a078-4bc80d3b0efe', legitimacy_requires_active_imperial_governance, theological).
narrative_ontology:cs_axiom('fa041789-c619-4dca-a078-4bc80d3b0efe', secondary, intermediary_governance_constitutes_usurpation).
narrative_ontology:cs_axiom_status(intermediary_governance_constitutes_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('fa041789-c619-4dca-a078-4bc80d3b0efe', intermediary_governance_constitutes_usurpation, theological).
narrative_ontology:cs_reference_frame('fa041789-c619-4dca-a078-4bc80d3b0efe', unmediated_tenno_sovereignty).
narrative_ontology:cs_drift_state('fa041789-c619-4dca-a078-4bc80d3b0efe', showa_militarist_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fa041789-c619-4dca-a078-4bc80d3b0efe', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, meiji_oligarchy).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_household).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, state_shinto_establishment).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, agrarian_households).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, conscripted_commoners).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, political_dissenters).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unbroken_imperial_lineage_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, kokutai_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, state_shinto_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small circle of Restoration leaders (later called genro) who directed the new state: they drafted the constitution, defined what counted as the emperor's will, decided war and peace, and allocated revenue. They invoked imperial authority for every major act while insulating their own deliberations from view. Their position let them reshape the rules binding everyone else, including the rules about who could speak for the throne; revenue, manpower, and unchallengeable authority flowed to the offices they held.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, meiji_oligarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, meiji_oligarchy, beneficiary).

% The dynasty restored to formal supremacy in 1868. It received veneration, a large endowment, and the constitutional position of sacred inviolability at the apex. The reigning emperor sanctioned decisions prepared by others, toured the country, opened diets, and in the end 'ordained' the surrender; his household ministry managed access to him. Leaving the position was unavailable in any practical sense — the office's sanctity was the state's foundation, the occupant was raised from birth inside it, and even the succession crisis of a disabled emperor (Taisho) was managed by regency rather than by letting the office stand empty.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_household, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, imperial_household, agenda_setter).

% The shrine hierarchy and doctrinal offices that taught the emperor's descent from the sun goddess and administered the rites of national unity. State-funded and state-protected after 1868, it supplied the curriculum, the calendar rituals, and the funeral rites through which the population learned loyalty. Its existence was identical with the doctrine it served; shrines were merged, defunded, or suppressed when their practices diverged from the national rite.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, state_shinto_establishment, beneficiary,
    institutional, generational, identity_locked, national).

% The hereditary warrior estate whose leaders carried out the Restoration. Once the new government consolidated power, their stipends were commuted to bonds and then effectively cancelled, their swords and class privileges removed, and their domains abolished. Some rose into the new bureaucracy and officer corps; the rest entered professions or poverty. Those who rose in revolt in 1877 were defeated by a conscript army drawn from the commons they had once ruled.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    organized, biographical, trapped, national).

% Farming families who bore the revised land tax, fixed in cash at a heavy share of harvest value, which financed factories, railways, and wars. Bad harvests forced tenancy, debt, and daughters into mill labor; tenant disputes met police and, after 1925, the thought-security apparatus. They had no franchise for most of the period and no realistic way to stop farming.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, agrarian_households, payer,
    powerless, generational, trapped, national).

% Men subject to universal military service from 1873, taken from villages for training and for the wars of 1894, 1904, and after — and, in the final decade, from the colonies as well. Families lost sons and remittances; villages lost labor. Desertion was a crime, and questioning the wars' purpose approached the criminal.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, conscripted_commoners, payer,
    powerless, biographical, trapped, continental).

% Socialists, anarchists, liberal parliamentarians, and Christians who argued for popular rights, pacifism, or republicanism. They faced surveillance, prison, dissolved organizations, and — in the 1910-11 High Treason case — execution. Some emigrated; most moderated or went silent. The space available to them widened briefly in the Taisho years and closed again in the 1930s.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, political_dissenters, payer,
    moderate, biographical, constrained, national).

% Those whose basic premise — that legitimate authority derives from the people — could not be stated in public without criminal exposure. They stood outside the constitutional conversation the oligarchy structured; the few who insisted, Kotoku Shusui among them, were prosecuted for treason and executed.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, popular_sovereignty_advocates, excluded,
    moderate, biographical, constrained, national).

% University jurists who interpreted the constitution, notably the school of Minobe Tatsukichi, which described the emperor as an organ of the state — an attempt to reconcile the sacred-apex text with actual ministerial government. The orthodoxy tolerated this reading for decades, then in 1935 drove Minobe from the House of Peers, banned his books, and prosecuted him; the purge marked the limit of describable reality inside the arrangement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, constitutional_scholarship, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, meiji_oligarchy).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified a fragmented archipelago of semi-autonomous domains into a single sovereign state with one legitimacy focal point, enabling centralized law, taxation, conscription, and rapid industrial-military modernization under existential foreign pressure.
% TRANSFER_FUNCTION: Moves labor (conscription), wealth (cash land tax, commuted and cancelled stipends, requisitioned rice and labor), and obedience (compulsory rites, criminalized dissent) from samurai households, agrarian producers, conscripts, and dissenters to the oligarchic state, the military establishment, and the imperial household — all routed through certified imperial will.
% ABSENT_VOICES: Popular-sovereignty advocates (premise unspeakable; the insistently vocal executed), bakufu adherents (written out as rebels after 1868), Ainu and Okinawan communities (assimilated without consent), and after 1910 Korean subjects (annexed, taxed, and conscripted with no seat anywhere in the arrangement). They are absent because they were executed, exiled, silenced, or colonized outside the conversation.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight in, say, 1885, the new state loses its sole uncontested legitimacy source: the oligarchs' sanctions lose their force, domain loyalties and samurai claims revive, the tax and conscription machines lose their justification, and the modernization program fragments along the fault lines it was built to close. Arrangements across law, schooling, the army, and the shrines were organized around it; their simultaneous unraveling is the definition of rearrangement.
% FOUNDING_PROBLEM: Political fragmentation under the bakuhan system, combined with foreign encroachment (unequal treaties, gunboat diplomacy), threatened national survival; reformers needed an uncontestable legitimacy source to break the feudal intermediaries and centralize power faster than the foreign threat matured.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary attests the death, which is the point. Postwar Japanese political historiography (Maruyama Masao and the scholarship following him) and the Tokyo tribunal record attest from outside the beneficiary set that the survival problem was substantially solved by the early twentieth century and that the arrangement's persistence thereafter served continental expansion and domestic control; prewar liberal jurists of the purged Minobe school corroborate from inside the era but outside the beneficiary coalition; the Showa Restoration radicals corroborate perversely, prosecuting the same judgment with bombs.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.85 by 1945 because the arrangement's transfers compounded: the cash land tax took a heavy fixed share of harvests to finance industry and war; stipend commutation converted samurai hereditary income into depreciating bonds; conscription took bodies from every village for the 1894, 1904, and continental wars; and total-war mobilization requisitioned labor and rice across the home islands and the colonies, all routed through claims of imperial will. Suppression is the highest metric (0.90) because exit did not exist in any category: imperial subjecthood was unavoidable, lèse-majesté criminalized even verbal irreverence, the 1925 Peace Preservation Law and the Special Higher Police made dissenting thought itself prosecutable, and the 1910-11 High Treason executions marked the outer boundary. Theater rises from 0.20 to 0.70: in the 1870s the emperor's initiative was real enough (tours, the Charter Oath era, the 1881 constitution promise), but after 1889 the sanction machinery formalized mediation-as-unmediation — constitutions 'granted,' wars 'decided,' surrender 'ordained,' all scripted by others — until by the 1930s field officers were bypassing the unmediated sovereign in his own name (gekokujo) while official profession of direct rule grew louder. The three series run on one shared seven-point grid (1868, 1877, 1889, 1905, 1918, 1932, 1945) so every metric is authored at every examined time point; the 1918 softening in suppression and extraction is the Taisho party-cabinet interlude, a real relaxation, not a grid artifact. Accessibility collapse is 0.72: popular sovereignty and republicanism were driven to the criminal margin, but Taisho pluralism kept partial alternatives alive for a generation. Resistance is 0.60: the Boshin War, the Satsuma Rebellion, repeated peasant tax risings, the Freedom and People's Rights Movement, and the terrorist wing of the restorationist left — substantial, recurrent, and always defeated.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the oligarchy's seat the arrangement is the coordination they built, staffed, and certified — a legitimacy machine that solved fragmentation under foreign pressure, and their arbitrage position (they wrote the rules defining imperial will) makes the structure look like statesmanship. From the samurai and dissenter seats the same structure computes as enforced taking with no exit: the class that made the restoration was dispossessed by it, and thinkers were executed for the premise of popular rule. From the imperial seat it computes as gilded captivity — the beneficiary whose sanction was compulsory and whose doubt was unthinkable. The jurist-observer seat registers the hybrid plainly: Minobe's organ theory was an attempt to describe, inside the orthodoxy, exactly the mediation the doctrine denied, and his 1935 purge shows what the orthodoxy did to accurate description. Same-level differentiation matters: samurai and conscripted commoners were both national subjects of similar formal standing, but the constraint disarmed the former while arming the latter, converting a potential payer coalition into the enforcement instrument — the 1877 revolt was broken by a peasant army, and the fiscal victims were recruited to suppress the class victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the oligarchy, the imperial household, and the shrine establishment near the beneficiary end of the directionality scale; the victim declarations place the four paying groups near the target end. Exit conditions grade the targets: trapped samurai, farm households, and conscripts sit at or near the full-target pole, while the dissenters' constrained exit (emigration was possible, silence was cheaper) leaves them somewhat less exposed. The oligarchy's arbitrage exit pulls it hard toward the subsidized end — it could redefine the constraint's content at will, and did. One correction beyond the derivation is warranted: the imperial household is declared a beneficiary, and the structural derivation would read the powerful atom as strongly subsidized, but the same arrangement consumed the occupant's agency — sanction was compulsory, abdication unavailable, public doubt impossible, and the household ministry managed access to the throne itself. A directionality override sets the powerful atom to d=0.30: a mostly-subsidized seat that nonetheless surrenders real freedom to the structure it anchors. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the arrangement's national-to-continental scope, which amplifies effective extraction as verification of 'imperial will' grew harder across the empire.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — political fragmentation that invited foreign predation, solvable only by rapid centralization around an uncontestable legitimacy source — was substantially solved by 1905, when Japan stood as a recognized great power. The arrangement persisted for four more decades and escalated. Authored honestly, this yields founding_problem_status=dead against disappearance_verdict=world_rearranges: the mismatch flag the battery is built to catch, cross-checked against the theater path (0.70 terminal) — a zombie signature. But the mandatrophy discipline prevents two symmetrical errors. First, it blocks the retrospective laundering move: the genuine early coordination (a real solution to a real collective-action problem, bought at terrible price) does not sanctify the later taking, because the founding problem's death severed the justification while the transfers continued. Second, it blocks the reverse error of reading the late theater as proof there was never a coordination function — the arrangement avoided the piton's inertial fate precisely by acquiring new uses (continental expansion, total mobilization) that kept its enforcers invested; this is weaponized persistence, not decay. The theater ratio measures the wrapping; the extractiveness series measures the engine underneath it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the imperial_mandate kernel — the loyalist_restoration_reading, holding that legitimacy is inseparable from the emperor''s active, unmediated governance. Its sibling, the bakufu_delegation_reading, holds the mandate''s legitimacy-granting function separable from the governing function. What would adopting the sibling change structurally?',
    'Comparative institutional analysis of the two readings'' governance arrangements against the documentary record of actual imperial agency (court diaries, genro deliberations, household ministry records): whichever reading''s structure matches who actually originated decisions identifies which constraint was operative.',
    'Under the sibling reading, mediating institutions become legitimate coordinators rather than usurpers, this constraint''s victim set largely dissolves, and epsilon falls toward coordination-cost levels; under this reading, the same institutions are the usurpation the arrangement exists to destroy and the victim set stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of the imperial_mandate kernel; disagreement located at the separability of the mandate''s legitimacy-granting and governing functions.').

omega_variable(
    divine_naturalness_vs_constructed_interest,
    'Is the unmediated-rule requirement a cosmic fact (an unbroken divine line whose mandate inherently demands personal exercise) or a constructed doctrine whose specific shape served identifiable collectors?',
    'Doctrinal genealogy: trace how the requirement''s content shifted with each collector''s needs (restoration-era anti-bakufu mobilization, oligarchic sanction machinery, military invocation of imperial will) — content that tracks collector interest marks construction.',
    'If constructed, the constraint is a defended artifact with beneficiaries (supporting the tangled_rope reading and its enforcement requirements); if genuinely natural-law-like for its holders, part of the measured suppression reflects sincere obligation rather than coercive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_naturalness_vs_constructed_interest, conceptual, 'Natural-law versus constructed-doctrine ambiguity in the mandate''s demand for unmediated rule.').

omega_variable(
    unmediation_paradox,
    'The reading demands unmediated personal rule, yet the arrangement it produced developed the most opaque mediation in Japanese history — sanctified oligarchy ruling through compulsory imperial sanction. Was theatrical unmediation inherent to the doctrine (personal rule at national scale necessarily requires hidden intermediaries) or contingent on the particular oligarchy?',
    'Counterfactual institutional comparison: examine restoration moments where the emperor did originate (early tours, the 1881 promise of a constitution, the 1945 surrender decision) and test whether direct initiative was structurally possible or always absorbed by intermediaries.',
    'If inherent, the reading is self-undermining — its ideal generates the usurpation it condemns — and the constraint''s classification drifts toward theatrical maintenance; if contingent, a differently configured restoration could have realized the reference frame, and the theater measurements index a specific coalition''s choices rather than the doctrine''s nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unmediation_paradox, conceptual, 'Whether the gap between demanded unmediation and delivered mediation is structural or historical.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression was structural (police, prosecutions, lèse-majesté law, the Peace Preservation Law) and how much internalized (Rescript-on-Education memorization, shrine rites, classroom loyalty drilling that persisted as conviction after the coercive apparatus was removed)?',
    'Post-1945 trajectory: when occupation reforms dismantled the enforcement machinery, observe how quickly dissenting and irreverent expression normalized — rapid normalization indicates structural dominance; persistent self-censorship and taboo indicate internalized carryover.',
    'If substantially internalized, the constraint''s effective suppression exceeded its structural measure and outlived its enforcement — raising true suppression above the authored 0.90 and explaining the low cost of postwar dismantling relative to prewar resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of the constraint''s suppressive force.').

omega_variable(
    emperor_captive_or_principal,
    'Was the imperial household the arrangement''s principal (directing through the sanction it granted) or its captive (compelled to sanction what intermediaries decided, unable to refuse, abdicate, or publicly doubt)?',
    'Interior record: the Konoe and Kido diaries, the Showa emperor''s 1946 monologue, and household ministry papers on access management — evidence of attempted refusals, blocked initiatives, and the handling of Taisho''s incapacity.',
    'If captive, the household''s directionality sits materially nearer the target end than its beneficiary declaration suggests, the override toward d=0.30 understates its subjection, and the arrangement''s gains concentrate even more narrowly on the oligarchy; if principal, the household shares responsibility for the extraction its sanction enabled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emperor_captive_or_principal, empirical, 'Whether the nominal apex collected the arrangement or was consumed by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1868, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.2).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1868, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1877, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1877, 0.25).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1877, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1889, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1889, 0.4).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1889, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1905, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1905, 0.48).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1905, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1918, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1918, 0.52).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1918, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1932, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1932, 0.62).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1932, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t1945, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1945, 0.7).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t1945, observed).

% Extraction over time
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.55).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1868, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1877, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1877, 0.68).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1877, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1889, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1889, 0.66).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1889, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1905, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1905, 0.72).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1905, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1918, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1918, 0.7).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1918, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1932, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1932, 0.79).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1932, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t1945, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1945, 0.85).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t1945, observed).

% Suppression requirement over time
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.7).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1868, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1877, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1877, 0.75).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1877, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1889, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1889, 0.72).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1889, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1905, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1905, 0.74).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1905, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1918, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1918, 0.69).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1918, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1932, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1932, 0.84).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1932, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t1945, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t1945, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two structurally distinct constraints per the epsilon-invariance principle. This file (loyalist_restoration_reading) authors the unmediated-sovereignty requirement: terminal epsilon 0.85, victims = the mediating classes and dissenters, enforcement through war and thought police. The sibling (bakufu_delegation_reading) authors the delegated arrangement as coordination: materially lower epsilon, a different victim set (imperial activists as disruptors of functioning delegation), no rupture requirement. The upstream/downstream asymmetry runs from the established delegated order to the restoration challenge that destroyed it; each file links the other in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
