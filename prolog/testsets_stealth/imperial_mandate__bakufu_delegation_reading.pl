% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate — Bakufu Delegation Reading (Bifurcated Sovereignty)
 *   domain: political philosophy / comparative constitutional systems / East Asian history
 *
 * SUMMARY:
 *   From the Kamakura bakufu's founding (1192) through the Meiji Restoration
 *   (1868), Japanese sovereignty operated as a bifurcated settlement: the
 *   emperor, sacrosanct and administratively abstinent, granted the warrant
 *   to govern; a military government — Kamakura, then Muromachi, then
 *   Tokugawa — exercised it. Each regime secured fresh imperial appointment
 *   and inherited the machinery of delegation; the samurai stratum staffed
 *   and was sustained by the arrangement; peasant producers funded it; the
 *   court's political agency was progressively confined. This story
 *   instantiates ONE reading of the imperial_mandate kernel — the
 *   bakufu_delegation_reading, which holds the mandate operates through
 *   institutional delegation and the emperor's legitimacy-granting function
 *   is separable from governing. The sibling loyalist_restoration_reading is
 *   a separate constraint in a separate file. Per the epsilon-referent rule,
 *   extractiveness here assesses the standing delegation arrangement as THIS
 *   reading sees it: endorsing the settlement's legitimacy while
 *   acknowledging the burdens it actually imposed. The claim and the metrics
 *   are independent authored facts: the reading claims tangled_rope; the
 *   metrics describe what the arrangement did.
 *
 * KEY AGENTS:
 *   - shogunal_bakufu: agenda-setter (institutional/arbitrage) — administers the delegated authority, writes and rewrites the rules of attendance, succession, and speech, and redesigns the settlement at each regime transition
 *   - samurai_governing_class: primary beneficiary (organized/identity_locked) — the stipended governing stratum whose livelihood, law, and honor code are built around service to the delegation order
 *   - imperial_household: dual-positioned legitimacy grantor (moderate/trapped/civilizational) — grants the warrant and performs the rites, receives stipends and primacy, bears confinement of its political agency; its leverage is patience and sacral scarcity
 *   - peasant_taxpayers: primary target (powerless/trapped/immediate) — grow the rice that funds the entire order, registered to villages, liable for tax and corvee
 *   - fudai_daimyo: secondary beneficiary (powerful/constrained) — hereditary allies who fill the cabinet and key posts in exchange for uncompensated service and capital attendance
 *   - tozama_daimyo: secondary target (powerful/constrained) — large incorporated-by-surrender domains barred from office, financing expensive attendance and hostage residence, watching and waiting
 *   - kuge_court_nobles: excluded voice (powerless/trapped/generational) — hereditary Kyoto aristocrats displaced from the governance they once exercised, with no forum to press the claim
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical/global) — study the settlement as a case of divided sovereignty and track which pole actually performed which function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.66).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.8).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate — Bakufu Delegation Reading (Bifurcated Sovereignty)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political philosophy / comparative constitutional systems / East Asian history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19').
narrative_ontology:cs_kernel_codification('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', fixed_text).
narrative_ontology:cs_authority_grounding('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', lineage).
narrative_ontology:cs_interpretation_layer_present('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19').
narrative_ontology:cs_reading_relation('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', foundational, mandate_transmissible_through_formal_delegation).
narrative_ontology:cs_axiom_status(mandate_transmissible_through_formal_delegation, holdable).
narrative_ontology:cs_axiom_grounding('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', mandate_transmissible_through_formal_delegation, conventional).
narrative_ontology:cs_axiom('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', secondary, imperial_ritual_purity_requires_delegated_coercion).
narrative_ontology:cs_axiom_status(imperial_ritual_purity_requires_delegated_coercion, holdable).
narrative_ontology:cs_axiom_grounding('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', imperial_ritual_purity_requires_delegated_coercion, theological).
narrative_ontology:cs_reference_frame('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', delegated_dual_sovereignty).
narrative_ontology:cs_drift_state('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', meiji_restoration_moment, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('873ecc78-ec0c-41f6-b1a9-d7d4bf10ff19', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_bakufu).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, imperial_household).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, fudai_daimyo).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasant_taxpayers).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, tozama_daimyo).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, kuge_court_nobles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_household).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, delegated_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, kenmon_taisei_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the generalissimo title renewed by imperial appointment at each accession, maintains the councils and magistracies that govern in the emperor's name, commands the vassal bands, regulates daimyo marriage and castle repair, and manages the court's calendar and purse through stewards in the capital. When a regime falls, its successor negotiates a fresh appointment and inherits the machinery. Its exit is redesign: it writes and rewrites the rules of attendance, succession, and speech.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunal_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives stipends measured in rice from domain granaries and the shogunal treasury, staffs every administrative post from village intendant to senior councillor, and is bound to the hierarchy by hereditary registration, sword law, and service obligation. Leaving the class forfeits livelihood, rank, and legal identity; the class's honor code and its children's schooling are built around service to the delegation order.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_class, beneficiary,
    organized, biographical, identity_locked, national).

% Grants the titles and edicts that authorize a military government to act, performs the rites that keep the realm in cosmic order, and receives court stipends and ceremonial deference in return. It may not legislate, tax, or command troops; its attempts to do so directly have been crushed militarily. Its leverage is patience and sacral scarcity: it can withhold or delay a grant, and it has outlasted every regime that managed it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_household, beneficiary,
    moderate, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, imperial_household, payer).

% Grow the rice that funds the entire order, pay a share of harvest to domain and shogunal collectors, supply corvee labor and construction levies, and are registered to their villages under travel and occupation restrictions. Village heads petition for relief through channels the authorities permit; flight to another domain is illegal and kin can be held liable.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasant_taxpayers, payer,
    powerless, immediate, trapped, local).

% Hereditary allies enfeoffed near the great cities; they fill the shogunal cabinet and rotate through key intendancies, gaining office and influence in exchange for uncompensated service and attendance duty in the capital. Their lands and successions require shogunal confirmation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, fudai_daimyo, beneficiary,
    powerful, generational, constrained, national).

% Large western and southern domains incorporated by surrender rather than partnership: barred from national office, required to maintain costly second residences and periodic attendance in the capital with families kept as courteous hostages, and watched by resident inspectors. They finance this from rich provincial estates and quietly trade, arm, and correspond beyond official sanction.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, tozama_daimyo, payer,
    powerful, generational, constrained, regional).

% Hereditary capital aristocrats who once administered provinces and now perform ceremony, compose, and preserve the archives; their offices and incomes are regulated by the military government's court ordinances. Those who recall their pre-bakufu governing role have no forum in which to press it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, kuge_court_nobles, excluded,
    powerless, generational, trapped, local).

% Study the arrangement as a case of divided sovereignty: a sacral grantor whose authorization is indispensable and an administrator whose coercion is indispensable. They compare it to viceregal and regency systems elsewhere and track which pole actually performed which function across the three regimes.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates the grant of legitimacy from its exercise so governance can continue continuously under a sacrosanct sovereign who cannot personally administer: one recognized grantor, one recognized wielder, hereditary transmission of both, and a single command hierarchy for the warrior class that forecloses private war over who may rule.
% TRANSFER_FUNCTION: Moves rice surplus, corvee labor, and attendance-and-hostage service upward from peasant producers through domain treasuries to the warrior stratum and shogunal works; moves legitimacy downward from emperor through shogun to daimyo; moves information and family members inward to the centers of control.
% ABSENT_VOICES: Peasant producers had no seat in any council that set their tax share; village grievance reached authority only as petition or riot. Displaced court nobles had no forum. During suppression campaigns the loyalist literati met in secret societies and domain schools outside official channels — the strongest dissenting voices were precisely the ones the enforcement machinery pushed out of the room.
% DISAPPEARANCE_RATIONALE: If the delegation settlement vanished overnight, every domain's legal basis for collecting tax and commanding force evaporates with it: warrior bands revert to local warlordism, the court cannot reassume administration it has not exercised for generations, and succession disputes reopen at every level. When the settlement actually ended in 1868, the entire domain system, class registry, and fiscal base were dismantled and rebuilt within a generation — the world rearranged.
% FOUNDING_PROBLEM: How can a realm be governed continuously when its sacrosanct sovereign's person cannot be subjected to the blood and bargaining of administration — who may exercise coercive power, and on what transmitted warrant?
% FOUNDING_PROBLEM_CORROBORATION: The court's own conduct corroborates that the settlement was contested, not consensual: Emperor Go-Daigo's restoration attempt in 1333 and repeated court-bakufu clashes show one party never accepted the delegation premise. Outside both beneficiary sets, Mito-school scholars (Aizawa Seishisai's Shinron) argued the delegation was usurpation requiring correction, and mid-nineteenth-century foreign envoys recorded the de jure/de facto split as a standing obstacle to treaty-making. No disinterested source attests the founding problem was dead; the parties disputed whether it was live.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 (end of interval) because the settlement's principal channel — the land-tax share of the rice harvest flowing from peasant producers to the warrior stratum — was heavy and persistent, though it purchased real governance. Suppression is authored raw and unscaled at 0.80: the settlement's persistence depended on active machinery (post-Jokyu military stewardship over the court, the Tokugawa court ordinances and attendance-hostage system, censorship and purge campaigns), and the terminal figure reflects maximum coercion exerted precisely as the arrangement collapsed. Theater ratio ends at 0.50: ritual maintenance was genuinely functional within the settlement (granting legitimacy was real work), but by the terminal decades a growing share of activity defended the form of delegation while its capacity decayed. Accessibility collapse is moderate (0.55): direct imperial rule was militarily foreclosed for centuries, yet the loyalist alternative remained conceivable and was ultimately executed. Resistance is substantial (0.55): the Jokyu war, the Kenmu restoration, domain and village uprisings, and the sonno Joi movement. The temporal series runs on one shared eight-point grid (all three metrics authored at every point) and is cyclical rather than monotonic: enforcement collapsed in the Sengoku trough (suppression 0.38, theater 0.48 as the shogunal title persisted while its function fragmented), then was rebuilt under Tokugawa recentralization. The cycle was driven by exogenous shocks — war and foreign pressure — not by intermittent reinforcement as an extraction mechanism; the oscillation is documented here as lifecycle data, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the shogunal seat the settlement is a working constitution its holders operate and periodically redesign; from the peasant seat the same structure is a tax machine with no exit; from the court seat it is gilded captivity — primacy and stipend purchased with political abstinence; from the tozama seat it is a discriminatory burden that financed its own eventual destroyers. The engine computes these divergent per-seat classifications from power, exit, and directional position; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (shogunal_bakufu, samurai_governing_class, imperial_household, fudai_daimyo) drive those seats toward the beneficiary end; victim declarations (peasant_taxpayers, tozama_daimyo, kuge_court_nobles) drive those seats toward the target end. The imperial household is deliberately dual-positioned: it collects stipends and ritual primacy while bearing suppressed political agency, so its derived directionality should sit intermediate rather than at either pole. No directionality overrides are authored: the derivation chain can distinguish the court from the peasants because the two seats carry different power atoms (moderate versus powerless) — the court's power in this system IS the delegable legitimacy, which is the whole point of the settlement — and different exit profiles. Trapped exits (court, peasants, kuge) sit nearer the full-target end than the constrained daimyo seats, whose residual mobility the engine should register.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling the settlement a snare ignores that its coordination function was genuine and repeatedly rebuilt: it solved legitimacy transmission across three regime changes, unified warrior command, and ended private war over who may rule — each collapse was followed by reconstruction of the same settlement, which pure extraction does not motivate. Calling it a rope ignores the asymmetric extraction (an entire producer class funding a warrior stratum), the suppressed court, and the enforcement machinery the settlement could not survive without. The R5 interview locates the mandatrophy question precisely: the founding problem (who may exercise coercive power under a sacrosanct sovereign) is CONTESTED, not dead — one party never accepted the delegation premise — yet the arrangement drifted terminally toward ceremony-maintained operation (theater_ratio 0.50 at interval end) before the Perry shock let the rival reading execute. The mismatch between contested founding status and world_rearranging persistence is the capture/zombie signal this story is built to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imperial_mandate_kernel_reading_delta,
    'This constraint instantiates the bakufu_delegation_reading of the imperial_mandate kernel; what would the sibling loyalist_restoration_reading change structurally?',
    'Author the sibling story and compare computed classifications. The disagreement is located in one element: whether the emperor''s legitimacy-granting function is separable from the governing function.',
    'Under the loyalist reading the imperial household becomes the primary victim rather than a dual-positioned beneficiary, epsilon rises sharply, and the same standing arrangement likely computes as snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_mandate_kernel_reading_delta, conceptual, 'Committer structure: one reading of the imperial_mandate kernel, routed here per the kernel-reading rules.').

omega_variable(
    mandate_function_separability,
    'Is the emperor''s legitimacy-granting function genuinely separable from the governing function, or does delegation progressively hollow the mandate so that each regime transition requires escalating ceremonial renewal?',
    'Comparative analysis of the three bakufu transitions (Kamakura to Muromachi to Tokugawa): measure ceremonial escalation and court-bakufu friction at each renewal of delegated authority.',
    'If separable, the arrangement is stable coordination-plus-extraction; if delegation self-hollows, the arrangement carries built-in decay toward inertial, ceremony-maintained operation regardless of enforcement effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_function_separability, empirical, 'Whether the bifurcation the reading depends on is stable or self-undermining.').

omega_variable(
    domain_tax_rate_variance,
    'What was the true aggregate burden on peasant producers, given domain-level variation in assessed land-tax share (roughly forty to sixty percent of harvest nominally) and unrecorded surtaxes and levies?',
    'Domain-level cadastral surveys (kenchi) and harvest reconstructions compared against stipend rolls and granary outflows.',
    'Higher true rates push the payer seats'' effective burden toward the pure-extraction boundary; lower rates support the coordination-with-costs reading of the settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_tax_rate_variance, empirical, 'Measurement uncertainty in the arrangement''s principal extraction channel.').

omega_variable(
    court_delegation_voluntariness,
    'Was the imperial household''s acceptance of the delegation settlement voluntary (protection, stipends, and ritual primacy exchanged for political abstinence) or coerced captivity dressed as settlement?',
    'Court diaries and kenmon records across all three regimes; observe court initiative during intervals when central enforcement lapsed, especially the Sengoku decades.',
    'If coerced, the court seat''s derived directionality sits too near the beneficiary end and belongs nearer the full-target end; the court''s experience of the arrangement shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_delegation_voluntariness, empirical, 'Ambiguity in the dual-positioned legitimacy-grantor seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1192, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1192, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1192, 0.2).
narrative_ontology:measurement_basis(impe_tr_t1192, observed).
narrative_ontology:measurement(impe_tr_t1221, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1221, 0.22).
narrative_ontology:measurement_basis(impe_tr_t1221, observed).
narrative_ontology:measurement(impe_tr_t1333, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1333, 0.3).
narrative_ontology:measurement_basis(impe_tr_t1333, observed).
narrative_ontology:measurement(impe_tr_t1467, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1467, 0.48).
narrative_ontology:measurement_basis(impe_tr_t1467, observed).
narrative_ontology:measurement(impe_tr_t1603, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1603, 0.24).
narrative_ontology:measurement_basis(impe_tr_t1603, observed).
narrative_ontology:measurement(impe_tr_t1716, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1716, 0.3).
narrative_ontology:measurement_basis(impe_tr_t1716, observed).
narrative_ontology:measurement(impe_tr_t1853, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1853, 0.42).
narrative_ontology:measurement_basis(impe_tr_t1853, observed).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.5).
narrative_ontology:measurement_basis(impe_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t1192, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1192, 0.46).
narrative_ontology:measurement_basis(impe_be_t1192, observed).
narrative_ontology:measurement(impe_be_t1221, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1221, 0.54).
narrative_ontology:measurement_basis(impe_be_t1221, observed).
narrative_ontology:measurement(impe_be_t1333, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1333, 0.5).
narrative_ontology:measurement_basis(impe_be_t1333, observed).
narrative_ontology:measurement(impe_be_t1467, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1467, 0.56).
narrative_ontology:measurement_basis(impe_be_t1467, observed).
narrative_ontology:measurement(impe_be_t1603, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1603, 0.63).
narrative_ontology:measurement_basis(impe_be_t1603, observed).
narrative_ontology:measurement(impe_be_t1716, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1716, 0.61).
narrative_ontology:measurement_basis(impe_be_t1716, observed).
narrative_ontology:measurement(impe_be_t1853, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1853, 0.65).
narrative_ontology:measurement_basis(impe_be_t1853, observed).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.66).
narrative_ontology:measurement_basis(impe_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1192, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1192, 0.35).
narrative_ontology:measurement_basis(impe_su_t1192, observed).
narrative_ontology:measurement(impe_su_t1221, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1221, 0.55).
narrative_ontology:measurement_basis(impe_su_t1221, observed).
narrative_ontology:measurement(impe_su_t1333, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1333, 0.6).
narrative_ontology:measurement_basis(impe_su_t1333, observed).
narrative_ontology:measurement(impe_su_t1467, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1467, 0.38).
narrative_ontology:measurement_basis(impe_su_t1467, observed).
narrative_ontology:measurement(impe_su_t1603, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1603, 0.72).
narrative_ontology:measurement_basis(impe_su_t1603, observed).
narrative_ontology:measurement(impe_su_t1716, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1716, 0.68).
narrative_ontology:measurement_basis(impe_su_t1716, observed).
narrative_ontology:measurement(impe_su_t1853, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1853, 0.75).
narrative_ontology:measurement_basis(impe_su_t1853, observed).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.8).
narrative_ontology:measurement_basis(impe_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'imperial mandate' conflates two structurally distinct claims. This story models the delegation claim (functions separable; bifurcated sovereignty legitimate); the sibling models the restoration claim (functions inseparable; unmediated rule required). The sibling reads the same standing arrangement as far more extractive (the court as captive rather than dual-positioned beneficiary) and will classify differently. The upstream/downstream pressure runs both ways historically: each bakufu's reliance on renewed imperial appointment strengthened the court's latent claim, and the loyalist movement's success in 1868 terminated this reading's arrangement. Linked via network.affects_constraints in both files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
