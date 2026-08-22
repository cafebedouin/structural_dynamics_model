% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate — Bakufu Delegation Reading (Bifurcated Sovereignty)
 *   domain: political philosophy/comparative constitutional systems/east asian history
 *
 * SUMMARY:
 *   For roughly seven centuries (1192-1868) Japan operated under a bifurcated
 *   sovereignty: the imperial throne, holder of sanctified descent, conferred
 *   legitimacy through investiture of the shogun, while successive warrior
 *   governments (Kamakura, Muromachi, Tokugawa) exercised actual
 *   administration. The arrangement solved a real coordination problem —
 *   continuous government despite the throne lacking coercive capacity and
 *   warrior coalitions lacking sacred sanction — while distributing its goods
 *   asymmetrically: the warrior stratum collected governance and revenue, the
 *   throne was confined to ritual, and farming villages funded the whole.
 *   This file instantiates ONE reading of the contested imperial_mandate
 *   kernel — the delegation reading, under which the bifurcation is a
 *   legitimate working order. The sibling loyalist_restoration_reading (a
 *   separate constraint file) assesses the same standing arrangement as
 *   usurpation; this file does not average across readings and authors a
 *   single stable epsilon for its own reading's referent. KEY AGENTS (by
 *   structural relationship): - bakufu_shogunal_regime: Agenda-setting
 *   enforcer (institutional/identity_locked) — administers the arrangement
 *   and is itself bound to it, since its authority IS the invested title -
 *   bushi_samurai_class: Primary beneficiary (organized/trapped) —
 *   monopolizes governance and stipends - kuge_court_nobility: Preserved
 *   junior beneficiary (moderate/identity_locked) — retains rank and ritual
 *   office, surrenders political agency - imperial_household_line: Primary
 *   target among elites (powerless/trapped) — supplies legitimacy, surrenders
 *   rule - peasant_village_communities: Fiscal target (powerless/trapped) —
 *   funds the warrior stratum - daimyo_domain_lords: Intermediate
 *   beneficiary-payer (organized/constrained) -
 *   loyalist_restoration_factions: Excluded dissent (organized/trapped) -
 *   comparative_constitutional_historians: Analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.8).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate — Bakufu Delegation Reading (Bifurcated Sovereignty)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political philosophy/comparative constitutional systems/east asian history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '5bf398bc-805c-4216-9d52-0da9302384bf').
narrative_ontology:cs_kernel_codification('5bf398bc-805c-4216-9d52-0da9302384bf', fixed_text).
narrative_ontology:cs_authority_grounding('5bf398bc-805c-4216-9d52-0da9302384bf', lineage).
narrative_ontology:cs_interpretation_layer_present('5bf398bc-805c-4216-9d52-0da9302384bf').
narrative_ontology:cs_reading_relation('5bf398bc-805c-4216-9d52-0da9302384bf', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('5bf398bc-805c-4216-9d52-0da9302384bf', foundational, legitimacy_governance_separability).
narrative_ontology:cs_axiom_status(legitimacy_governance_separability, holdable).
narrative_ontology:cs_axiom_grounding('5bf398bc-805c-4216-9d52-0da9302384bf', legitimacy_governance_separability, conventional).
narrative_ontology:cs_axiom('5bf398bc-805c-4216-9d52-0da9302384bf', foundational, warrior_stratum_governance_entitlement).
narrative_ontology:cs_axiom_status(warrior_stratum_governance_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('5bf398bc-805c-4216-9d52-0da9302384bf', warrior_stratum_governance_entitlement, conventional).
narrative_ontology:cs_reference_frame('5bf398bc-805c-4216-9d52-0da9302384bf', delegated_mandate_order).
narrative_ontology:cs_drift_state('5bf398bc-805c-4216-9d52-0da9302384bf', bakumatsu_crisis, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5bf398bc-805c-4216-9d52-0da9302384bf', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_shogunal_regime).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bushi_samurai_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, kuge_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_household_line).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasant_village_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, daimyo_domain_lords).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, kuge_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, daimyo_domain_lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office of shogun under investiture issued by the emperor, administers the warrior government, issues laws binding the court as well as the domains, and approves imperial successions. Its authority is defined by the title it receives; ruling without that title has never been attempted, and its own law codes describe its power as exercised on the throne's behalf.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_shogunal_regime, agenda_setter,
    institutional, generational, identity_locked, national).

% Monopolizes military service, administration, and the right to bear swords; draws hereditary rice stipends assessed on village output. Individual members cannot leave the class without losing legal status and livelihood; the class reproduces itself through hereditary enrollment.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bushi_samurai_class, beneficiary,
    organized, generational, trapped, national).

% Staffs the ritual and ceremonial offices around the throne, holds court rank, and receives stipends tied to designated estates. Its families' standing exists only inside the court hierarchy; in exchange they accept exclusion from military and fiscal administration and live under bakufu regulations governing their conduct, dress, and movements.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, kuge_court_nobility, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, kuge_court_nobility, payer).

% Provides the sanctified descent line from which legitimacy is drawn, performs the rites that invest shoguns, and signs the edicts that authorize warrior governments. Its members' public conduct, marriages, and access to the throne are regulated by bakufu codes; successive emperors who attempted to reclaim direct rule were defeated or exiled.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_household_line, payer,
    powerless, civilizational, trapped, national).

% Cultivate the land under a rice-assessment tax system that funds warrior stipends and domain administration; bound to their registered villages, liable for corvee labor, and represented in the arrangement only through domain officials.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasant_village_communities, payer,
    powerless, biographical, trapped, national).

% Hold semi-autonomous domains under bakufu supremacy confirmed by imperial sanction, administer local justice and taxation, and owe military service, rotating attendance in Edo, and tribute to the shogunate. Their autonomy exists inside the frame; open defiance has meant confiscation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, daimyo_domain_lords, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, daimyo_domain_lords, payer).

% Scholars, courtiers, and later activist samurai who argue the throne must rule directly and treat warrior government as usurpation. They publish, conspire, and occasionally revolt; across most of the interval they are exiled, imprisoned, or executed, and they hold no seat in any council of the arrangement.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_factions, excluded,
    organized, biographical, trapped, national).

% Study the arrangement as a case of divided sovereignty alongside European regencies and dual monarchies, reconstructing how legitimacy and administration were apportioned and why the division held for seven centuries.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, comparative_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, bakufu_shogunal_regime).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the standing mismatch between sacred legitimacy and coercive-administrative capacity: the throne supplies sanctified authorization it alone can confer, the warrior government supplies governance it alone can execute, and the investiture protocol gives each transfer of power between warrior regimes a standard, repeatable form.
% TRANSFER_FUNCTION: Moves sanctified authorization downward (titles, investiture, edicts from throne to shogun); moves rice revenue and labor services upward (villages to warriors to bakufu treasuries); moves day-to-day political decision-making away from the court to warrior councils.
% ABSENT_VOICES: Loyalist court factions and Confucian scholars arguing for direct imperial rule sit outside every council; peasant villages are spoken for by domain officials; no seat represents the throne's own preference, which enters the record only when an emperor acts against the frame and is defeated.
% DISAPPEARANCE_RATIONALE: When the arrangement ended in 1868 the whole order rebuilt itself within a generation: domains replaced by prefectures, the warrior class legally dissolved, a conscript army created, the throne recast as active sovereign head of a centralized state, and the court's ritual apparatus repurposed as state religion — nothing downstream kept its prior form.
% FOUNDING_PROBLEM: Japan needed continuous government while the throne held sanctity without armies and warrior coalitions held armies without sanctity; the arrangement was built so that each transfer of power could be re-authorized rather than fought to annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Ming and Joseon diplomatic records treat the shogun, not the emperor, as Japan's operative sovereign; the Kenmu episode (1333-1336) showed direct imperial rule collapsing without warrior cooperation, as contemporaries such as Kitabatake Chikafusa recorded; modern comparative histories of divided sovereignty attest the legitimacy-capacity mismatch was real. Loyalist partisans dispute that the mismatch ever justified delegation, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.68: the arrangement delivered real goods — continuity across three warrior regimes, a standard transfer protocol, and under the Tokugawa two and a half centuries of peace — while confiscating the throne's political agency and layering warrior stipends on village output; the terminal value reflects the end-state in which the frame's delivery had thinned. Suppression 0.80: persistence always depended on actively suppressing the throne's political involvement — from the Jokyu confiscations (1221) through the Kinchu narabini kuge shohatto (1615) and resident supervision of the court — not on participant preference. Theater 0.42: investiture and rite remained load-bearing (every shogun needed the title) but grew formulaic as the court's role narrowed to performance. Accessibility collapse 0.55: direct-rule alternatives were repeatedly imagined and attempted (Jokyu, Genko and Kenmu, sonno joi) yet structurally foreclosed for centuries; exit existed as aspiration, not option. Resistance 0.45: episodic armed challenges and persistent ideological dissent, unsuccessful until the very end. The three metric series share one time grid (seven points spanning 1192-1868). The suppression series deliberately traces enforcement-capacity change: hardening after Jokyu and again under the Tokugawa legal codification, with a mid-interval decay during the Sengoku breakdown when no one actively enforced anything at the court — a V-shape, not noise.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the shogunal seat the arrangement is the legitimate order it administers — the invested title is its authority, and enforcing the frame is defending its own identity. From the imperial seat the same structure is lifelong confinement of a civilizational office to ritual. From the village seat it is a tax schedule. From the court-noble seat it is a gilded cage: rank preserved, agency surrendered. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the shogunal regime (collects apex revenue and the invested title), the warrior class (monopolizes governance and stipends), and the court nobility (retains rank and ritual office). Victims: the imperial household (political agency confiscated) and village communities (surplus transferred upward). Derived directionalities follow these declarations with exit modulation: the shogunal regime's identity-lock — its authority IS the invested title, and no shogun ever attempted to rule without one — places it deeper in the frame than raw beneficiary status suggests; the villagers' trapped exit places them at the full-target end. One override: kuge_court_nobility is declared beneficiary, which would derive a strongly beneficiary-side d, but their ledger nets near symmetric — preserved status against total surrender of political agency and subjection to bakufu conduct codes — so an override sets d to 0.45 for the moderate power atom they uniquely occupy in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite mislabels. Reading the arrangement as pure extraction erases its genuine coordination achievement: three regime transitions handled by re-investiture rather than annihilation, and a legitimacy-capacity compromise no contemporary alternative delivered. Reading it as pure coordination erases the asymmetry: the throne's agency was not compensated at any rate it could negotiate, and village surplus funded a stratum whose military function atrophied. On mandatrophy: the founding problem (the legitimacy-capacity mismatch) was arguably still live at the end — but the arrangement's supporting stratum had hollowed, the samurai having become salaried administrators under a long peace, so what collapsed in 1868 was less a solved problem abandoned than a frame whose carriers had changed nature. The contested founding_problem_status paired with world_rearranges records this honestly rather than forcing either a zombie or a triumphalist genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading of the imperial_mandate kernel — the delegation reading, under which the mandate operates through institutional delegation and the emperor''s legitimacy-granting function is separable from governing. What would classification change if the sibling loyalist_restoration_reading were instantiated instead?',
    'Author the sibling as its own constraint file: under the loyalist reading the same standing arrangement is assessed as usurpation, the imperial household becomes the primary victim rather than a compensated ritual custodian, the coordination function is reframed as cover, and epsilon rises sharply.',
    'Under the sibling reading the same historical arrangement computes as far more extractive with an expanded victim set; the divergence between the two files is the measured disagreement of the kernel, not an error in either.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: delegation reading versus loyalist restoration reading of the imperial mandate.').

omega_variable(
    mandate_sincerity,
    'Did the shogunal houses regard the invested mandate as genuinely constitutive of their authority, or maintain it as an instrumentally useful fiction?',
    'Close reading of bakufu legal codes, succession-crisis behavior (whether shoguns ever risked material interests to preserve the investiture protocol), and diplomatic correspondence.',
    'If instrumental, the arrangement''s coordination content is thinner than authored and the theater ratio understates drift toward inertial maintenance; if sincere, the coordination function is robust and the hybrid assessment is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_sincerity, empirical, 'Whether the invested mandate was sincerely constitutive or instrumentally maintained.').

omega_variable(
    peasant_extraction_attribution,
    'How much of the village tax burden is attributable to the delegation arrangement specifically, rather than to any agrarian pre-modern state?',
    'Compare effective tax rates and corvee burdens across periods and regimes inside and outside the arrangement''s span, controlling for war and harvest shocks.',
    'If most extraction is generic to agrarian states, the constraint-specific epsilon is materially lower than authored and the arrangement sits nearer the coordination end; if the warrior stipend system added a distinctive layer, the authored value stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_extraction_attribution, empirical, 'Attribution of village extraction to the delegation frame versus generic agrarian statehood.').

omega_variable(
    kenmu_counterfactual_stability,
    'Was the bifurcated arrangement the only workable resolution of the legitimacy-capacity mismatch, or could a stronger direct-ruling throne have held Japan together?',
    'Counterfactual analysis of the Kenmu Restoration''s failure modes and comparison with contemporaneous polities where sacral kingship did exercise direct rule.',
    'If direct rule was viable, the arrangement''s coordination claim weakens and its persistence reads as enforcement of a chosen allocation; if not viable, the coordination function is genuine and load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kenmu_counterfactual_stability, conceptual, 'Counterfactual viability of direct imperial rule as the alternative to delegation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1192, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1192, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1192, 0.08).
narrative_ontology:measurement_basis(impe_tr_t1192, observed).
narrative_ontology:measurement(impe_tr_t1221, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1221, 0.12).
narrative_ontology:measurement_basis(impe_tr_t1221, observed).
narrative_ontology:measurement(impe_tr_t1336, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1336, 0.18).
narrative_ontology:measurement_basis(impe_tr_t1336, observed).
narrative_ontology:measurement(impe_tr_t1467, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1467, 0.28).
narrative_ontology:measurement_basis(impe_tr_t1467, observed).
narrative_ontology:measurement(impe_tr_t1603, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1603, 0.33).
narrative_ontology:measurement_basis(impe_tr_t1603, observed).
narrative_ontology:measurement(impe_tr_t1716, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1716, 0.38).
narrative_ontology:measurement_basis(impe_tr_t1716, observed).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.42).
narrative_ontology:measurement_basis(impe_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t1192, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1192, 0.44).
narrative_ontology:measurement_basis(impe_be_t1192, observed).
narrative_ontology:measurement(impe_be_t1221, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1221, 0.49).
narrative_ontology:measurement_basis(impe_be_t1221, observed).
narrative_ontology:measurement(impe_be_t1336, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1336, 0.54).
narrative_ontology:measurement_basis(impe_be_t1336, observed).
narrative_ontology:measurement(impe_be_t1467, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1467, 0.57).
narrative_ontology:measurement_basis(impe_be_t1467, observed).
narrative_ontology:measurement(impe_be_t1603, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1603, 0.63).
narrative_ontology:measurement_basis(impe_be_t1603, observed).
narrative_ontology:measurement(impe_be_t1716, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1716, 0.66).
narrative_ontology:measurement_basis(impe_be_t1716, observed).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(impe_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1192, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1192, 0.35).
narrative_ontology:measurement_basis(impe_su_t1192, observed).
narrative_ontology:measurement(impe_su_t1221, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1221, 0.52).
narrative_ontology:measurement_basis(impe_su_t1221, observed).
narrative_ontology:measurement(impe_su_t1336, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1336, 0.62).
narrative_ontology:measurement_basis(impe_su_t1336, observed).
narrative_ontology:measurement(impe_su_t1467, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1467, 0.55).
narrative_ontology:measurement_basis(impe_su_t1467, observed).
narrative_ontology:measurement(impe_su_t1603, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1603, 0.74).
narrative_ontology:measurement_basis(impe_su_t1603, observed).
narrative_ontology:measurement(impe_su_t1716, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1716, 0.78).
narrative_ontology:measurement_basis(impe_su_t1716, observed).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.8).
narrative_ontology:measurement_basis(impe_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'divine mandate of Japan' decomposes into two structurally distinct constraints per the epsilon-invariance principle. This file instantiates the delegation reading: mandate operating through institutional delegation, legitimacy-granting separable from governing, epsilon authored at 0.68 for the standing bifurcated arrangement. The sibling loyalist_restoration_reading instantiates the unmediated-exercise reading: the same standing arrangement assessed as usurpation, the imperial household as primary victim, materially higher epsilon. The upstream/downstream relation runs through shared texts — Kitabatake Chikafusa's Jinno Shotoki argues imperial descent while conceding ministerial exercise of rule, and both readings cite it. Each file keeps a single stable epsilon over the same referent (the standing arrangement); the difference in epsilon belongs to the readings, not to observable selection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__bakufu_delegation_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
